import pytest
import brownie
from brownie import accounts


def test_reinvest_1(mlm):
    mlm.newDeposit(accounts[9], {'from': accounts[2], 'value': "90 ether"})  # для выплаты депозитов

    mlm.increaseNowTime(3, 17, {'from': accounts[0]})
    mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "12 ether"})

    adminBalanceBefore = accounts[9].balance();
    tx = mlm.reinvest(0, {'from': accounts[0]})

    info = mlm.getCertainDeposit(accounts[0], 0)
    assert info[1] == 0
    assert info[0] == 7*10**18 + (7 * 10 ** 14) * 7 * 4 * 24

    info = mlm.getCertainDeposit(accounts[0], 1)
    assert info[1] == (12 * 10 ** 16) * 7
    assert info[0] == 12 * 10 ** 18

    assert tx.events['Reinvest']['amountWithdrawned'] == (7*10**14)*7*4*70
    assert tx.events['Reinvest']['amountReinvested'] == (7*10**14)*7*4 * 24

    assert adminBalanceBefore + (7*10**14)*7*4 * 6 == accounts[9].balance()


def test_reinvest_2(mlm):
    with brownie.reverts("newDeposit function must be called first"):
        mlm.reinvest(0, {'from': accounts[7]})

    adminBalanceBefore = accounts[9].balance()

    mlm.newDeposit(accounts[9], {'from': accounts[2], 'value': "90 ether"})

    mlm.increaseNowTime(3, 17, {'from': accounts[0]})
    mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "5 ether"})

    mlm.increaseNowTime(7, 17, {'from': accounts[0]})
    mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "6 ether"})

    mlm.increaseNowTime(4, 17, {'from': accounts[0]})
    mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "5 ether"})

    assert mlm.getCertainDeposit(accounts[0], 0)[1] == (7 * 10 ** 16) * 7
    assert mlm.getCertainDeposit(accounts[0], 1)[1] == (5 * 10 ** 16) * 7
    assert mlm.getCertainDeposit(accounts[0], 2)[1] == (6 * 10 ** 16) * 7
    assert mlm.getCertainDeposit(accounts[0], 3)[1] == (5 * 10 ** 16) * 7
    assert mlm.getCertainDeposit(accounts[0], 4)[1] == 0

    investorBalanceBefore = accounts[0].balance()
    adminBalanceBefore += (90 + 10 + 6) * 10**17 + (90*10**16)*7  # по депозиту  и реф бонусу
    assert adminBalanceBefore - accounts[9].balance() == 0


    tx = mlm.reinvestAll({'from': accounts[0]})

    assert tx.events['Reinvest'][0]['amountWithdrawned'] == ((7 * 10**14) * 7 * 15 * 70)

    assert len(tx.events['Reinvest']) == 4
    invSend = ((7 * 10**14) * 7 * 15 * 70) + ((5 * 10**14) * 7 * 12 * 70) + ((6 * 10**14) * 7 * 5 * 70)
    invSend += ((5 * 10**14) * 1 * 7 * 70)


    reinv = (7*10**14)*15*7 + (5*10**14)*12*7 + (6*10**14)*5*7 + (5*10**14)*1*7
    adminBalanceBefore += reinv * 6

    assert adminBalanceBefore - accounts[9].balance() == 0
    assert investorBalanceBefore + invSend - accounts[0].balance() == 0


def test_reinvest_3(mlm):
    mlm.newDeposit(accounts[9], {'from': accounts[2], 'value': "90 ether"})
    mlm.increaseNowTime(40, 17, {'from': accounts[0]})
    profit = (7 * 10 ** 16) * 7
    assert mlm.getCertainDeposit(accounts[0], 0)[1] == profit
    balanceBefore = accounts[0].balance()
    tx = mlm.reinvestAll({'from': accounts[0]})

    profit *= 40
    assert tx.events['ReturnDeposit']['amount'] == 7*10**18
    assert tx.events['Reinvest']['amountWithdrawned'] == profit * 94/100
    assert tx.events['Reinvest']['amountReinvested'] == 0

    print(accounts[0].balance() - balanceBefore + 7*10**18 + profit * 94/100)
