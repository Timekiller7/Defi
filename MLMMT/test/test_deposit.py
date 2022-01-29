import pytest
import brownie
from brownie import accounts


def test_new_deposit_1(mlm):
    info = mlm.getInvestorInfo(accounts[0])
    balanceAcc1 = accounts[9].balance()

    for i in range(len(info)):
        print(info[i])

    assert info[0][0] == accounts[1]
    assert info[1] == 7 * 10 ** 18

    # проверка функции toAdmin(); 10*10**18 - начальный баланс
    assert balanceAcc1 == 100 * 10 ** 18 + 7 * 10 ** 18 / 10
    balanceAcc1 = accounts[9].balance()

    with brownie.reverts("Minimum deposit is 5 Matic"):
        mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "3 ether"})

    mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "5 ether"})

    assert info[0][0] == accounts[1]
    assert info[1] == 7 * 10 ** 18
    assert info[2] == 7 * 10**16 * 7

    assert accounts[9].balance() == balanceAcc1 + 5 * 10 ** 18 / 10


def test_new_deposit_2(mlm):
    tx = mlm.newDeposit(accounts[2], {'from': accounts[1], 'value': "5 ether"})
    assert tx.events['Deposit']['investor'] == accounts[1]
    assert tx.events['Deposit']['amount'] == "5 ether"

    countBonus = tx.events['RefBonus']

    assert countBonus['investor'] == accounts[1]
    assert countBonus['referrer'] == accounts[2]
    assert countBonus['amount'] == 5 * 10 ** 18 * 7 / 100
    assert (len(countBonus)) == 1

    tx = mlm.newDeposit(accounts[2], {'from': accounts[1], 'value': "6 ether"})
    info = mlm.getInvestorInfo(accounts[1])

    assert info[1] == 11 * 10 ** 18
    assert tx.events['Deposit']['investor'] == accounts[1]
    assert tx.events['Deposit']['amount'] == "6 ether"

# открытие нового счета
def test_new_deposit_3(mlm):
    mlm.newDeposit(accounts[9], {'from': accounts[2], 'value': "90 ether"})  # чтобы хватило на реинвест
    info = mlm.getInvestorInfo(accounts[0])
    profit = 7 * 10**16 * 7
    assert info[2] == profit

    mlm.increaseNowTime(3, 17, {'from': accounts[0]})
    assert mlm.checkDaysWithoutReward() == 3

    # 7 % от 7 eth (прошлого депозита)
    mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "12 ether"})
    info = mlm.getInvestorInfo(accounts[0])

    profit += ((7+12) * 10 ** 16) * 1 * 7 + (7 * 10 ** 16) * 2 * 7
    assert info[2] == profit

    lastUpdate = info[3]
    deadline = info[4]

    mlm.increaseNowTime(37, 18, {'from': accounts[0]})
    assert mlm.checkDaysWithoutReward() == 36

    with brownie.reverts("Deposit and profit must be withdrawned first"):
        mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "6 ether"})
    assert mlm.checkDaysWithoutReward() == 36

    before = accounts[0].balance()

    deposit = (7 + 12) * 10 ** 18
    print("Deposited: ", mlm.getInvestorInfo(accounts[0])[1])
    print("Profit: ", mlm.getInvestorInfo(accounts[0])[2])
   # print("Profit2: ", mlm.getInvestorInfo(accounts[0])[2] - profit)

    assert mlm.getInvestorInfo(accounts[0])[2] == profit
    tx = mlm.reinvestAll({'from': accounts[0]})
    profit += ((7 + 12) * 10 ** 16) * 36 * 7


    info = mlm.getInvestorInfo(accounts[0])
    assert info[1] == 0
    assert info[2] == 0

    assert mlm.getInvestorInfo(accounts[0])[3] == mlm.getInvestorInfo(accounts[0])[4]
    assert mlm.getInvestorInfo(accounts[0])[3] == deadline
    assert mlm.getInvestorInfo(accounts[0])[3] > lastUpdate

    assert tx.events['Reinvest']['amountWithdrawned'] == profit * 94 / 100
    assert tx.events['Reinvest']['amountReinvested'] == 0
    print(before + profit + deposit - accounts[0].balance())  # = 0

    mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "12 ether"})
    info = mlm.getInvestorInfo(accounts[0])

    assert info[1] == "12 ether"
    assert info[2] == 12 * 10**16 * 7
    assert info[4] > deadline
    assert info[3] != lastUpdate


# все реферреры
def test_referrers_1(mlm):
    eth = 5 * 10**16
    for i in range(7):
        tx = mlm.newDeposit(accounts[i + 1], {'from': accounts[i + 2], 'value': "5 ether"})
        events = tx.events['RefBonus']
        if i == 0:
            assert events['referrer'] == accounts[1]
            assert events['amount'] == eth * 7
        elif i == 1:
            assert events[0]['referrer'] == accounts[2]
            assert events[0]['amount'] == eth * 7
            assert events[1]['referrer'] == accounts[1]
            assert events[1]['amount'] == eth * 2
        elif i == 2:
            assert events[0]['referrer'] == accounts[3]
            assert events[0]['amount'] == eth * 7
            assert events[1]['referrer'] == accounts[2]
            assert events[1]['amount'] == eth * 2
            assert events[2]['referrer'] == accounts[1]
            assert events[2]['amount'] == eth * 1

    print(mlm.getInvestorInfo(accounts[8])[0])


# 3 реферрера
def test_referrers_2(mlm):
    for i in range(3):
        mlm.newDeposit(accounts[i + 1], {'from': accounts[i + 2], 'value': "5 ether"})

    refs = mlm.getInvestorInfo(accounts[4])[0]
    print(refs)
    assert refs[0] == accounts[3]
    assert mlm.getInvestorInfo(accounts[3])[0][0] == accounts[2]

