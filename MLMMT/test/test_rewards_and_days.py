import pytest
import brownie
from brownie import accounts


def test_checkRewards_1(mlm):
    assert mlm.checkDaysWithoutReward(0, {'from': accounts[0]}) == 0
    info = mlm.getCertainDeposit(accounts[0], 0)

    assert info[1] == (7 * 10 ** 16) * 7

    mlm.increaseNowTime(1, 1, {'from': accounts[0]})

    assert mlm.checkDaysWithoutReward(0, {'from': accounts[0]}) == 1

    mlm.calculateReward(0, {'from': accounts[0]})
    assert mlm.getProfit() == (7 * 10 ** 16) * 1 * 7


# за весь срок
def test_checkRewards_2(mlm):
    mlm.increaseNowTime(40, 17, {'from': accounts[0]})
    assert mlm.checkDaysWithoutReward(0, {'from': accounts[0]}) == 39

    mlm.calculateReward(0, {'from': accounts[0]})
    assert mlm.getProfit() == 39*7*7*10**18/100

    mlm.increaseNowTime(2, 17, {'from': accounts[0]})
    assert mlm.checkDaysWithoutReward(0, {'from': accounts[0]}) == 0
    mlm.calculateReward(0, {'from': accounts[0]})
    assert mlm.getProfit() == 0
