pragma solidity ^0.8.11;


contract Mlm {  
 /*Комиссия администратора 10% на каждый депозит и 6% на каждый реинвест
Пользователь может открыть неограниченное количество депозитных планов:
⬡ Новый депозит - это первое и последующие пополнения счета пользователем
(В этом случае взымается комиссия на счет админа в размере 10%)
⬡ Реинвест - происходит в момент снятие средств пользователем
(В этом случае взымается комиссия на счет админа в размере 6%)
Пользователь получает в размере 7% ежедневного дохода на свой счет в течение 40 дней. Максимальный доход получаеться в размере 280%
При выводе средств пользователем:
⬡ 70% дохода идет на его счет
⬡ 30% отправляется реинвестом на его счет и доход считается теперь с обновленной суммы (6% от этой суммы идет комиссией на счет администратора)
⬡ Минимальный допустимый депозит который может внести пользователь:
5 MATIC
⬡ Минимальная допустимая сумма для вывода средств составляет: 0.05 MATIC
⬡ Максимальная сумма не ограничена, что означает пользователь может инвестировать любую сумму, превышающую 5 MATIC.
ПАРТНЕРСКАЯ ПРОГРАММА (11,5%)
1 level - 7%
2 level - 2%
3 level - 1%
4 level - 0,5%
5 level - 0,5%
6 level - 0,3%
7 level - 0,2%*/


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
    event Reinvest(address indexed investor, uint256 amountWithdrawned, uint256 amountReinvested);
    event RefBonus(address indexed investor, address indexed referrer, uint256 amount);

    struct Investor {
        address[7] refs;
        uint256 deposited;
        uint256 profit;               // доход, для реинвеста
        uint256 lastUpdate;           // обновление времени, для расчета наград
        uint256 startDate;            // время открытия счета
    }
    
    mapping(address => Investor) public investors;
    
    constructor(address payable _admin) {
        require(_admin != address(0), "Admin address can't be null");
        admin = _admin;
    }
    
    function newDeposit(address _ref) public payable {
        require(msg.value >= MIN_DEPOSIT, "Minimum deposit is 5 Matic");
        require(msg.sender != _ref,"The caller and ref address must be different");

        uint256 amount = msg.value;
        if (investors[msg.sender].startDate <= 1) {   
            if (investors[msg.sender].startDate == 0 && _ref != address(0)) {    //инициал при нуле, 1 - когда был закрыт счет
                investors[msg.sender].refs[0] = _ref;
                sendRefBonus(payable(_ref), 0, amount);
                addReferrers(msg.sender, _ref, amount);
            }

            investors[msg.sender].startDate = block.timestamp;
            investors[msg.sender].lastUpdate = block.timestamp;
        } else {                                                   // повторный депозит
            investors[msg.sender].profit += calculateReward();
        }
        investors[msg.sender].deposited += amount;

        emit Deposit(msg.sender, amount);
        
        toAdmin(amount * DEPOSIT_FEE_PERC / 100);
    }

    function reinvest() external {
        require(investors[msg.sender].startDate != 0,
               "newDeposit function must be called first"
        );

        uint256 profit = investors[msg.sender].profit + calculateReward();

        require(profit >= MIN_REINVEST, "Minimum withdraw is 0.05 Matic");

        investors[msg.sender].profit = 0;
        _reinvest(profit);
        
        if (checkDaysLeft() == 0 && investors[msg.sender].deposited != 0) {
            returnDeposit();
        }
    }

    function _reinvest(uint256 amount) private {
        uint256 reinvested = amount * REINVEST_OF_PROFIT_PERC / 100;
        uint256 send =  amount * REINVEST_WITHDRAW_PERC / 100;

        investors[msg.sender].deposited += reinvested;

        (bool transferSuccess, ) = payable(msg.sender).call{value: send}("");
        require(transferSuccess, "Transfer to investor failed");
        
        emit Reinvest(msg.sender, send, reinvested);
        toAdmin(amount * REINVEST_FEE_PERC / 100);
    }
 
    function getInvestorInfo(address investor) public view returns(Investor memory) {
        return investors[investor];
    }

    function checkDaysLeft() private view returns(uint256) {
        uint256 deadline = investors[msg.sender].startDate + DEPOSIT_DAYS;
        uint256 lastUpdate = investors[msg.sender].lastUpdate;

        if (deadline <= block.timestamp) {
            if (deadline <= lastUpdate)
                return 0;
            return  (deadline - lastUpdate) / (1 days); 
        } else {
            return (block.timestamp - lastUpdate) / (1 days);
        }
    }
    
    function calculateReward() private returns(uint256) {
        uint256 amount = investors[msg.sender].deposited;
        uint256 differenceDays = checkDaysLeft();  
        investors[msg.sender].lastUpdate = block.timestamp;
       
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
            bonus = REF_LEVEL_4 * amount / 1000;
        else if (level == 5)
            bonus = REF_LEVEL_4 * amount / 1000;
        else if (level == 6)
            bonus = REF_LEVEL_4 * amount / 1000;
        
        (bool transferSuccess, ) = payable(to).call{
                value: bonus
            }("");
        require(transferSuccess, "Transfer to referrer failed");

        emit RefBonus(msg.sender, to, bonus);
    }

    function returnDeposit() private {
        uint256 deposit = investors[msg.sender].deposited;
        investors[msg.sender].deposited = 0;
        investors[msg.sender].startDate == 1;          // конец 40-ка дневного депозита
        (bool transferSuccess, ) = payable(msg.sender).call{
                value: deposit
            }("");
        require(transferSuccess, "Transfer to admin failed");
    }

    function toAdmin(uint256 amount) private {
        (bool transferSuccess, ) = admin.call{
                value: amount
            }("");
        require(transferSuccess, "Transfer to admin failed");
    }
}