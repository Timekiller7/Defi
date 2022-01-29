import pytest
import brownie
from brownie import accounts


def test_reinvest_1(mlm):
    mlm.newDeposit(accounts[9], {'from': accounts[2], 'value': "90 ether"})  # для выплаты депозитов

    mlm.increaseNowTime(3, 17, {'from': accounts[0]})
    mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "12 ether"})

    profit = (7 * 10 ** 16) * 7 + (7 * 10 ** 16) * 2 * 7 + ((7+12) * 10 ** 16) * 1 * 7
    assert mlm.getInvestorInfo(accounts[0])[2] == profit

    adminBalanceBefore = accounts[9].balance();
    tx = mlm.reinvest(60 * (10 ** 18), {'from': accounts[0]})
    info = mlm.getInvestorInfo(accounts[0])

    assert info[1] == 19*10**18 + profit * 24 / 100
    print(info)
    assert info[2] == 0

    assert tx.events['Reinvest']['amountWithdrawned'] == profit * 7 / 10
    assert tx.events['Reinvest']['amountReinvested'] == profit * 24 / 100

    assert adminBalanceBefore + profit * 6 / 100 == accounts[9].balance()


def test_reinvest_2(mlm):
    mlm.newDeposit(accounts[9], {'from': accounts[2], 'value': "90 ether"})

    mlm.increaseNowTime(3, 17, {'from': accounts[0]})
    mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "12 ether"})

    profit = (7 * 10 ** 16) * 3 * 7 + ((7+12) * 10 ** 16) * 1 * 7

    ethSend = 10 ** 18
    adminBalanceBefore = accounts[9].balance()
    investorBalanceBefore = accounts[0].balance()

    tx = mlm.reinvest(ethSend, {'from': accounts[0]})
    info = mlm.getInvestorInfo(accounts[0])

    assert info[1] == 19*ethSend + (ethSend / 100) * 24
    assert info[2] == profit - ethSend

    assert tx.events['Reinvest']['amountWithdrawned'] == ethSend * 7 / 10
    assert tx.events['Reinvest']['amountReinvested'] == ethSend * 24 / 100

    assert adminBalanceBefore + ethSend * 6 / 100 == accounts[9].balance()
    assert (investorBalanceBefore + (ethSend / 100) * 70) - accounts[0].balance() == 0


def test_reinvest_3(mlm):
    mlm.newDeposit(accounts[9], {'from': accounts[2], 'value': "90 ether"})
    mlm.increaseNowTime(40, 17, {'from': accounts[0]})
    profit = (7 * 10 ** 16) * 7
    assert mlm.getInvestorInfo(accounts[0])[2] == profit
    balanceBefore = accounts[0].balance()
    tx = mlm.reinvestAll({'from': accounts[0]})

    profit *= 40
    assert tx.events['ReturnDeposit']['amount'] == 7*10**18
    assert tx.events['Reinvest']['amountWithdrawned'] == profit * 94/100
    assert tx.events['Reinvest']['amountReinvested'] == 0

    print(accounts[0].balance() - balanceBefore + 7*10**18 + profit * 94/100)
