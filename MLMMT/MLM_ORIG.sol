pragma solidity ^0.8.11;


contract Mlm {
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

    struct Investor {
        address[7] refs;
        uint256 deposited;
        uint256 profit;               // доход, для реинвеста
        uint256 lastUpdate;           // обновление времени, для расчета наград
        uint256 deadline;             // последний день начисления %
    }

    mapping(address => Investor) public investors;

    modifier checkDate() {
       require(investors[msg.sender].deadline != 0,
               "newDeposit function must be called first"
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
        if (investors[msg.sender].deadline == 0 || 
            investors[msg.sender].deadline + 1 days <= block.timestamp) {
            if (investors[msg.sender].deadline == 0 && _ref != address(0)) { //инициализация счета
                investors[msg.sender].refs[0] = _ref;                  
                sendRefBonus(payable(_ref), 0, amount);
                addReferrers(msg.sender, _ref, amount);
            }

            if (investors[msg.sender].deadline + 1 days <= block.timestamp) {  // открытие нового счета
                require(investors[msg.sender].deposited == 0 &&
                    investors[msg.sender].profit == 0,
                    "Deposit and profit must be withdrawned first"
                );
            }
            uint256 time = block.timestamp;
            investors[msg.sender].lastUpdate = time;
            investors[msg.sender].deadline = time + (DEPOSIT_DAYS - 1) * 1 days;  // + 39 дней
            investors[msg.sender].profit = (amount / 100) * DAILY_PROFIT_PERC;   // за первый день
            investors[msg.sender].deposited = amount;
        } else {
            if (investors[msg.sender].deadline < block.timestamp)  // по истечении 40 дней можно забрать
                revert("The deposit expires in a day");           // в этом случае остался последний день
            // для ежедневного профита
            if (checkDaysWithoutReward() > 1) {     // логика для обработки профита (от нового депозита) текущего дня и предыдущих
                investors[msg.sender].lastUpdate += 1 days;
                investors[msg.sender].profit += calculateReward();
                investors[msg.sender].lastUpdate -= 1 days;
            }
            investors[msg.sender].deposited += amount;
            investors[msg.sender].profit += calculateReward();
        }

        emit Deposit(msg.sender, amount);

        toAdmin(amount * DEPOSIT_FEE_PERC / 100);
    }

    function reinvestAll() external checkDate {
        uint256 profit = investors[msg.sender].profit + calculateReward();

        require(profit >= MIN_REINVEST, "Minimum withdraw is 0.05 Matic");

        investors[msg.sender].profit = 0;
        _reinvest(profit);

        if (investors[msg.sender].deadline + 1 days <= block.timestamp &&
            investors[msg.sender].deposited != 0)
        {
            returnDeposit();
        }
    }

    function reinvest(uint256 amount) external checkDate {
        require(amount >= MIN_REINVEST, "Minimum withdraw is 0.05 Matic");

        uint256 profit = investors[msg.sender].profit + calculateReward();

        if (amount < profit) {
            investors[msg.sender].profit -= amount;
            _reinvest(amount);
        } else {                    // amount >= profit => реинвестируем весь профит
            investors[msg.sender].profit = 0;
           _reinvest(profit);
        }

        if (investors[msg.sender].deadline + 1 days <= block.timestamp &&
            investors[msg.sender].deposited != 0) 
        {
            returnDeposit();
        }
    }

    function _reinvest(uint256 amount) private {
        uint256 reinvested;
        uint256 send;
        if (investors[msg.sender].deadline >= block.timestamp) {   //deadline - время последнего начисления
            reinvested = amount * REINVEST_OF_PROFIT_PERC / 100;
            send =  amount * REINVEST_WITHDRAW_PERC / 100;
            investors[msg.sender].deposited += reinvested;
        } else {                        // когда счет закрылся
            send =  (amount * (100 - REINVEST_FEE_PERC)) / 100;
        }

        (bool transferSuccess, ) = payable(msg.sender).call{value: send}("");
        require(transferSuccess, "Transfer to investor failed");

        emit Reinvest(msg.sender, send, reinvested);
        toAdmin(amount * REINVEST_FEE_PERC / 100);
    }

    function getInvestorInfo(address investor) public view returns(Investor memory) {
        return investors[investor];
    }

    function checkDaysWithoutReward() public view returns(uint256) {
        uint256 deadline = investors[msg.sender].deadline;
        uint256 lastUpdate = investors[msg.sender].lastUpdate;
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

    function calculateReward() private returns(uint256) {   
        uint256 amount = investors[msg.sender].deposited;
        uint256 differenceDays = checkDaysWithoutReward();
        investors[msg.sender].lastUpdate += differenceDays * 1 days;

        return (amount / 100) * differenceDays * DAILY_PROFIT_PERC;
    }

    function addReferrers(address investor, address _ref, uint256 amount) private {
        address[7] memory referrers = investors[_ref].refs;
        for (uint256 i = 0; i < 6; i++) {
            if (referrers[i] != address(0)) {
                investors[investor].refs[i+1] = referrers[i];
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

        (bool transferSuccess, ) = payable(to).call{
                value: bonus
            }("");
        require(transferSuccess, "Transfer to referrer failed");

        emit RefBonus(msg.sender, to, bonus);
    }

    function returnDeposit() private {      // конец 40-ка дневного депозита
        uint256 deposit = investors[msg.sender].deposited;
        investors[msg.sender].deposited = 0;
        
        (bool transferSuccess, ) = payable(msg.sender).call{
                value: deposit
            }("");
        require(transferSuccess, "Transfer to investor failed");

        emit ReturnDeposit(msg.sender, deposit);
    }

    function toAdmin(uint256 amount) private {
        (bool transferSuccess, ) = admin.call{
                value: amount
            }("");
        require(transferSuccess, "Transfer to admin failed");
    }
}