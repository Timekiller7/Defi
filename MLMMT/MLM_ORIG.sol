pragma solidity ^0.8.11;


abstract contract ReentrancyGuard {
    uint256 private constant _NOT_ENTERED = 1;
    uint256 private constant _ENTERED = 2;

    uint256 private _status;

    constructor() {
        _status = _NOT_ENTERED;
    }

    /**
     * @dev Prevents a contract from calling itself, directly or indirectly.
     * Calling a `nonReentrant` function from another `nonReentrant`
     * function is not supported. It is possible to prevent this from happening
     * by making the `nonReentrant` function external, and making it call a
     * `private` function that does the actual work.
     */
    modifier nonReentrant() {
        // On the first call to nonReentrant, _notEntered will be true
        require(_status != _ENTERED, "ReentrancyGuard: reentrant call");

        // Any calls to nonReentrant after this point will fail
        _status = _ENTERED;

        _;

        // By storing the original value once again, a refund is triggered (see
        // https://eips.ethereum.org/EIPS/eip-2200)
        _status = _NOT_ENTERED;
    }
}

contract Mlm is ReentrancyGuard{
    address payable public admin;
    uint256 constant DAILY_PROFIT_PERC = 7;

    uint256 constant DEPOSIT_DAYS = 40;
    uint256 constant DEPOSIT_FEE_PERC = 10;
    uint256 constant MIN_DEPOSIT = 5 ether;

    uint256 constant REINVEST_FEE_PERC = 6;
    uint256 constant REINVEST_OF_PROFIT_PERC = 24;
    uint256 constant REINVEST_WITHDRAW_PERC = 70;
    uint256 constant MIN_REINVEST = 0.05 ether;

    uint256 private constant DECIMAL_OFFSET = 10;
    uint256 private constant REF_LEVEL_1 = 7 * DECIMAL_OFFSET;
    uint256 private constant REF_LEVEL_2 = 2 * DECIMAL_OFFSET;
    uint256 private constant REF_LEVEL_3 = 1 * DECIMAL_OFFSET;
    uint256 private constant REF_LEVEL_4 = 5;
    uint256 private constant REF_LEVEL_5 = 5;
    uint256 private constant REF_LEVEL_6 = 3;
    uint256 private constant REF_LEVEL_7 = 2;

    event Deposit(address indexed investor, uint256 amount);
    event ReturnDeposit(address indexed investor, uint256 amount);
    event Reinvest(address indexed investor, uint256 amountWithdrawned, uint256 amountReinvested);
    event RefBonus(address indexed investor, address indexed referrer, uint256 amount);

    struct Investment {
        uint256 deposited;
        uint256 profit;               // доход, для реинвеста
        uint256 lastUpdate;           // обновление времени, для расчета наград
        uint256 deadline;             // последний день начисления %
    }

    mapping(address => Investment[10]) public invests;
    mapping(address => address[7]) public refs;

    modifier checkDate(uint256 index) {
       require(invests[msg.sender][index].deadline != 0,
               "newDeposit function must be called first"
        );
        _;
    }

    modifier checkIndex(uint256 index) {
       require(index < 10,
               "Unappropriate index"
        );
        _;
    }

    constructor(address payable _admin) {
        require(_admin != address(0), "Admin address can't be null");
        admin = _admin;
    }

    function newDeposit(address _ref) external payable {
        require(msg.value >= MIN_DEPOSIT, "Minimum deposit is 5 Matic");
        require(msg.sender != _ref,"The caller and ref address must be different");

        uint256 amount = msg.value;
        uint256 ind = 11;
        for (uint256 i = 0; i < 10; i++) {
            if (invests[msg.sender][i].deadline == 0) {
                ind = i;
                break;
            }
        }
        
        if (ind == 11) {      // тк ограничение на массив
            if (!checkIfFundsWithrawned())
                revert("All deposits should be withdrawned before new investment");
            ind = 0;
            delete invests[msg.sender];
        }
        
        Investment storage invest = invests[msg.sender][ind];

        if (_ref != address(0) && refs[msg.sender][0] == address(0)) { 
            refs[msg.sender][0] = _ref;                  
            sendRefBonus(payable(_ref), 0, amount);
            addReferrers(msg.sender, _ref, amount);
        }
        uint256 time = block.timestamp;
        invest.lastUpdate = time;
        invest.deadline = time + (DEPOSIT_DAYS - 1) * 1 days;  // + 39 дней
        invest.profit = (amount / 100) * DAILY_PROFIT_PERC;   // за первый день
        invest.deposited = amount;

        emit Deposit(msg.sender, amount);

        sendTo(admin, amount * DEPOSIT_FEE_PERC / 100);
    }

    function reinvestAll() external nonReentrant {        
        for (uint256 i = 0; i < 10; i++) {
            if(invests[msg.sender][i].deadline != 0) 
                _reinvest(i);   
            else 
                break;
        }
    }

    function reinvest(uint256 index) external checkIndex(index) checkDate(index) nonReentrant {
        _reinvest(index);
    }

     function getAllDeposits(address investor) public view returns(Investment[10] memory) {
        return invests[investor];
    }

    function getCertainDeposit(address investor, uint256 index) public view checkIndex(index) returns(Investment memory)  {
        return invests[investor][index];
    }

    function getRefs() public view returns(address[7] memory) {
        return refs[msg.sender];
    }

    function checkDaysWithoutReward(uint256 index) public view checkIndex(index) returns(uint256) {
        uint256 deadline = invests[msg.sender][index].deadline;
        uint256 lastUpdate = invests[msg.sender][index].lastUpdate;
        uint256 _days;

        if (deadline < block.timestamp) {
            if (deadline == lastUpdate) {
                _days = 0;
            }
            _days =  (deadline - lastUpdate) / (1 days);
        } else {
            _days = (block.timestamp - lastUpdate) / (1 days);
        }

        return _days;
    }


    function _reinvest(uint256 index) private {
        Investment storage invest = invests[msg.sender][index];
        uint256 amount = invest.profit + calculateReward(index);
        invest.profit = 0;

        require(amount >= MIN_REINVEST, "Minimum withdraw is 0.05 Matic");
       
        uint256 reinvested;
        uint256 send;
        if (invest.deadline >= block.timestamp) {   //deadline - время последнего начисления
            reinvested = amount * REINVEST_OF_PROFIT_PERC / 100;
            send =  amount * REINVEST_WITHDRAW_PERC / 100;
            invest.deposited += reinvested;
        } else {                        // когда счет закрылся
            send =  (amount * (100 - REINVEST_FEE_PERC)) / 100;
        }

        sendTo(msg.sender, send);
        emit Reinvest(msg.sender, send, reinvested);
        
        sendTo(admin, amount * REINVEST_FEE_PERC / 100);

        if (invest.deadline + 1 days <= block.timestamp &&
            invest.deposited != 0) 
        {
            returnDeposit(index);
        }
    }

    function checkIfFundsWithrawned() private view returns(bool) {
        Investment[10] storage invest = invests[msg.sender];
        for (uint256 i = 0; i < 10; i++) {
            if (invest[i].profit == 0 && invest[i].deposited == 0)
                continue;
            else
                return false;
        }
        return true;
    }

    function calculateReward(uint256 index) private returns(uint256) {   
        uint256 amount = invests[msg.sender][index].deposited;
        uint256 differenceDays = checkDaysWithoutReward(index);
        invests[msg.sender][index].lastUpdate += differenceDays * 1 days;

        return (amount / 100) * differenceDays * DAILY_PROFIT_PERC;
    }

    function addReferrers(address investor, address _ref, uint256 amount) private {
        address[7] memory referrers = refs[_ref];
        for (uint256 i = 0; i < 6; i++) {
            if (referrers[i] != address(0)) {
                refs[investor][i+1] = referrers[i];
                sendRefBonus(payable(referrers[i]), i+1, amount);
            } else break;
        }
    }

    function sendRefBonus(address to, uint256 level, uint256 amount) private {
        uint256 bonus;
        if (level == 0)
            bonus = REF_LEVEL_1 * amount / 1000;
        else if (level == 1)
            bonus = REF_LEVEL_2 * amount / 1000;
        else if (level == 2)
            bonus = REF_LEVEL_3 * amount / 1000;
        else if (level == 3)
            bonus = REF_LEVEL_4 * amount / 1000;
        else if (level == 4)
            bonus = REF_LEVEL_5 * amount / 1000;
        else if (level == 5)
            bonus = REF_LEVEL_6 * amount / 1000;
        else if (level == 6)
            bonus = REF_LEVEL_7 * amount / 1000;

        sendTo(to, bonus);

        emit RefBonus(msg.sender, to, bonus);
    }

    function returnDeposit(uint256 index) private {      // конец 40-ка дневного депозита
        uint256 deposit = invests[msg.sender][index].deposited;
        invests[msg.sender][index].deposited = 0;
        
        sendTo(msg.sender, deposit);

        emit ReturnDeposit(msg.sender, deposit);
    }

    function sendTo(address to, uint256 amount) private {
        (bool transferSuccess, ) = payable(to).call{
                value: amount
            }("");
        require(transferSuccess, "Transfer failed");
    }
}