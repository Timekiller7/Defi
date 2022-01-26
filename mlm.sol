pragma solidity ^0.8.11;

// реинвест алл и реинвест(какая-то сумма)
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

//@dev доделать константы 
    address payable public admin;
    uint256 constant DAILY_PROFIT_PERC = 7;

    uint256 constant DEPOSIT_DAYS = 40;
    uint256 constant DEPOSIT_FEE_PERC = 10;
    uint256 constant MIN_DEPOSIT = 5 ether;

    uint256 constant REINVEST_FEE_PERC = 6;
    uint256 constant REINVEST_of_PROFIT_PERC = 24;
    uint256 constant REINVEST_withdraw_PERC = 70; 
    uint256 constant MIN_REINVEST = 0.05 ether;

    uint256 private constant decimal_offset = 10;    //потом не забыть про деление
    uint256 private constant ref_level_1 = 7 * decimal_offset;
    uint256 private constant ref_level_2 = 2 * decimal_offset;
    uint256 private constant ref_level_3 = 1 * decimal_offset;
    uint256 private constant ref_level_4 = 5;
    uint256 private constant ref_level_5 = 5;
    uint256 private constant ref_level_6 = 3;
    uint256 private constant ref_level_7 = 2;


    event Deposit(address investor, uint256 amount);
    event Reinvest(address investor, uint256 amountWithdraw, uint256 amountReinvested);

    struct Investor {
        address addr;
       // address ref;
        uint256[7] refs;
        uint256 deposited;
        uint256 witdrawned;
        uint256 daysLeft;           //инициализируется  = 40 
        //Investment[] investments;
    }
   /* struct Investment {
        uint256 investmentDate;
        uint256 investment;
        uint256 dividendCalculatedDate;
        bool isExpired;
        uint256 lastWithdrawalDate;
        uint256 dividends;
        uint256 withdrawn;
    }*/
    mapping(address => Investor) public investors;

    constructor(address payable _admin) {
        require(_admin != address(0), "Admin address can't be null");
        admin = _admin;
    }

    function newDeposit(address _ref) public payable {
        require(msg.value >= MIN_DEPOSIT, "Minimum deposit is 5 Matic");
        require(msg.sender != _ref,"The caller and ref address must be different");
        
        uint256 amount = msg.value; //???
        //сделать логику когда дней лефт !=0 в общем да когда инициализировано уже
           //=> просто обновление депозита

        toAdmin(amount * DEPOSIT_FEE_PERC / 100);
    }

    function reinvest(uint256 amount) public {
        require(amount >= MIN_REINVEST, "Minimum withdraw is 0.05 Matic");
       // require(amount >= .deposit, "Amount exceeds balance of investor "); чтобы эмаунт был >= того что имеется

        toAdmin(amount * REINVEST_FEE_PERC / 100);
    }

    function getMyInfo() public view returns (Investor) {
        return investors[msg.sender];
    }

    function toAdmin(uint256 amount) private {
        (bool transferSuccess, ) = admin.call{
                value: amount
            }("");
        require(transferSuccess, "Transfer to admin failed");
    }
}