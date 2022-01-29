#!/usr/bin/python3

import pytest

@pytest.fixture(scope="function", autouse=True)
def isolate(fn_isolation):
    pass


@pytest.fixture(scope="module")
def mlm(Mlm, accounts):
    ml = accounts[0].deploy(Mlm, accounts[9])
    tx = ml.newDeposit(accounts[1], {'from': accounts[0], 'value': "7 ether"})
    return ml
