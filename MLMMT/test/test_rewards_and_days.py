import pytest
import brownie
from brownie import accounts


def test_checkRewards_1(mlm):
    assert mlm.checkDaysWithoutReward() == 0
    info = mlm.getInvestorInfo(accounts[0])

    assert info[2] == (7 * 10 ** 16) * 7

    mlm.increaseNowTime(1, 1, {'from': accounts[0]})

    assert mlm.checkDaysWithoutReward() == 1

    mlm.calculateReward({'from': accounts[0]})
    assert mlm.getProfit() == (7 * 10 ** 16) * 1 * 7


# за весь срок
def test_checkRewards_2(mlm):
    mlm.increaseNowTime(40, 17, {'from': accounts[0]})
    assert mlm.checkDaysWithoutReward() == 39

    mlm.calculateReward({'from': accounts[0]})
    assert mlm.getProfit() == 39*7*7*10**18/100

    mlm.increaseNowTime(2, 17, {'from': accounts[0]})
    assert mlm.checkDaysWithoutReward() == 0
    mlm.calculateReward({'from': accounts[0]})
    assert mlm.getProfit() == 0


# с повторными депозитами
def test_checkRewards_3(mlm):
    mlm.newDeposit(accounts[9], {'from': accounts[2], 'value': "90 ether"})  # чтобы хватило на реинвест
    info = mlm.getInvestorInfo(accounts[0])
    profit = (7 * 10 ** 16) * 1 * 7
    assert info[2] == profit

    mlm.increaseNowTime(3, 17, {'from': accounts[0]})
    assert mlm.checkDaysWithoutReward() == 3

    mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "12 ether"})
    info = mlm.getInvestorInfo(accounts[0])

    profit += (7*10**16) * 2 * 7 + ((7+12)*10**16) * 7
    assert info[2] == profit

    mlm.increaseNowTime(36, 18, {'from': accounts[0]})
    assert mlm.checkDaysWithoutReward() == 36
    with brownie.reverts("The deposit expires in a day"):
          mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "6 ether"})

    mlm.increaseNowTime(1, 18, {'from': accounts[0]})
    with brownie.reverts("Deposit and profit must be withdrawned first"):
        mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "6 ether"})

    lastDeadline = mlm.getInvestorInfo(accounts[0])[4]

    mlm.reinvestAll({'from': accounts[0]})

    info = mlm.getInvestorInfo(accounts[0])
    assert info[1] == 0
    assert info[2] == 0

    mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "6 ether"})
    info = mlm.getInvestorInfo(accounts[0])

    assert info[1] == "6 ether"
    assert info[2] == (6*10**16)*7
    assert info[4] > lastDeadline

    mlm.increaseNowTime(4, 18, {'from': accounts[0]})
    assert mlm.checkDaysWithoutReward() == 4


def test_checkRewards_4(mlm):
    info = mlm.getInvestorInfo(accounts[0])
    profit = (7 * 10 ** 16) * 1 * 7

    assert info[2] == profit

    mlm.increaseNowTime(5, 17, {'from': accounts[0]})

    mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "8 ether"})
    info = mlm.getInvestorInfo(accounts[0])

    profit += (7 * 10 ** 16) * 4 * 7 + ((7+8) * 10 ** 16)  * 7
    assert info[2] == profit

    mlm.increaseNowTime(5, 18, {'from': accounts[0]})
    assert mlm.checkDaysWithoutReward() == 5
    mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "6 ether"})

    info = mlm.getInvestorInfo(accounts[0])
    profit += ((7+8) * 10 ** 16) * 4 * 7 + ((7+8+6) * 10 ** 16) * 7
    assert info[2] == profit

    mlm.increaseNowTime(30, 18, {'from': accounts[0]})
    assert mlm.checkDaysWithoutReward() == 29

    with brownie.reverts("Deposit and profit must be withdrawned first"):  # тк счет закрыт
        mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "9 ether"})



