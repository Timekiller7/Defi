import pytest
import brownie
from brownie import accounts


def test_new_deposit_1(mlm):
    info = mlm.getCertainDeposit(accounts[0], 0)   # getAllDeposits
    balanceAcc1 = accounts[9].balance()

    for i in range(len(info)):
        print(info[i])

    assert mlm.getRefs({'from': accounts[0]})[0] == accounts[1]
    assert info[0] == 7 * 10 ** 18
    assert info[1] == (7 * 10 ** 16) * 7

    # проверка функции toSend, перечисление админу
    assert balanceAcc1 == 100 * 10 ** 18 + 7 * 10 ** 18 / 10
    balanceAcc1 = accounts[9].balance()

    with brownie.reverts("Minimum deposit is 5 Matic"):
        mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "3 ether"})

    mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "5 ether"})

    assert mlm.getRefs({'from': accounts[0]})[0] == accounts[1]
    assert mlm.getRefs({'from': accounts[0]})[1] != accounts[1]

    info = mlm.getCertainDeposit(accounts[0], 1)
    assert info[0] == 5 * 10 ** 18
    assert info[1] == (5 * 10 ** 16) * 7

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
    info = mlm.getCertainDeposit(accounts[1], 1)

    assert info[0] == 6 * 10 ** 18
    assert tx.events['Deposit']['investor'] == accounts[1]
    assert tx.events['Deposit']['amount'] == "6 ether"


# с переполнением массива адресов и реинвестом
def test_new_deposit_3(mlm):
    mlm.newDeposit(accounts[9], {'from': accounts[8], 'value': "90 ether"})  # чтобы хватило на реинвест
    mlm.newDeposit(accounts[9], {'from': accounts[7], 'value': "90 ether"})
    mlm.newDeposit(accounts[9], {'from': accounts[3], 'value': "90 ether"})
    info = mlm.getCertainDeposit(accounts[0], 0)
    profit = 7 * 10**16 * 7
    assert info[1] == profit

    mlm.increaseNowTime(3, 17, {'from': accounts[0]})
    assert mlm.checkDaysWithoutReward(0, {'from': accounts[0]}) == 3
    assert mlm.checkDaysWithoutReward(1, {'from': accounts[0]}) == 0

    mlm.newDeposit(accounts[1], {'from': accounts[0], 'value': "12 ether"})
    print(mlm.getAllDeposits(accounts[0]))

    assert mlm.checkDaysWithoutReward(0, {'from': accounts[0]}) == 3
    assert mlm.checkDaysWithoutReward(1, {'from': accounts[0]}) == 0

    info = mlm.getCertainDeposit(accounts[0], 1)

    assert info[1] == 12 * 10**16 * 7

    mlm.increaseNowTime(37, 18, {'from': accounts[0]})
    assert mlm.checkDaysWithoutReward(0, {'from': accounts[0]}) == 39
    assert mlm.checkDaysWithoutReward(1, {'from': accounts[0]}) == 37

    for i in range(10):
        mlm.newDeposit(accounts[2], {'from': accounts[6], 'value': "5 ether"})

    info = mlm.getRefs({'from': accounts[6]})
    assert info[0] == accounts[2]
    assert info[1] != accounts[2] and info[1] != accounts[6]
    assert info[2] != accounts[2]
    
    with brownie.reverts("All deposits should be withdrawned before new investment"):
        mlm.newDeposit(accounts[2], {'from': accounts[6], 'value': "6 ether"})

    mlm.increaseNowTime(40, 18, {'from': accounts[0]})
    mlm.reinvestAll({'from': accounts[6]})
    mlm.newDeposit(accounts[2], {'from': accounts[6], 'value': "6 ether"})

    assert mlm.checkDaysWithoutReward(0, {'from': accounts[6]}) == 0
    assert mlm.checkDaysWithoutReward(9, {'from': accounts[6]}) == 0
    assert mlm.getCertainDeposit(accounts[6], 0)[0] == 6*10**18
    assert mlm.getCertainDeposit(accounts[6], 1)[0] == 0

    before = accounts[0].balance()

    info = mlm.getCertainDeposit(accounts[0], 0)

    lastUpdate = info[2]
    deadline = info[3]

    tx = mlm.reinvestAll({'from': accounts[0]})

    info = mlm.getCertainDeposit(accounts[0], 0)
    assert info[0] == 0
    assert info[1] == 0

    info = mlm.getCertainDeposit(accounts[0], 1)
    assert info[0] == 0
    assert info[1] == 0

    assert mlm.getCertainDeposit(accounts[0], 0)[3] == mlm.getCertainDeposit(accounts[0], 0)[2]
    assert mlm.getCertainDeposit(accounts[0], 0)[3] == deadline
    assert mlm.getCertainDeposit(accounts[0], 0)[2] != lastUpdate

    profit = 7 * 10**14 * 7 * 40 * 94
    profit2 = 12 * 10 ** 14 * 7 * 40 * 94

    assert tx.events['Reinvest'][0]['amountWithdrawned'] == profit
    assert tx.events['Reinvest'][0]['amountReinvested'] == 0
    assert tx.events['Reinvest'][1]['amountWithdrawned'] == profit2
    assert tx.events['Reinvest'][1]['amountReinvested'] == 0
    print(before + profit + profit2 + (12+7) * 10**18 - accounts[0].balance())  # = 0

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

    print(mlm.getRefs({'from': accounts[8]}))


# 3 реферрера
def test_referrers_2(mlm):
    for i in range(3):
        mlm.newDeposit(accounts[i + 1], {'from': accounts[i + 2], 'value': "5 ether"})

    refs = mlm.getRefs({'from': accounts[4]})
    print(refs)
    assert refs[0] == accounts[3]
    assert mlm.getRefs({'from': accounts[3]})[0] == accounts[2]

