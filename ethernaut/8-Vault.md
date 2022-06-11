# Vault

Here we recall the golden rule: everything on the blockchain is there for everyone to see, and it will always be there. As such, we can find out the password that was used in construction of the contract. There are actually two ways to do it:

1. Find the password by looking at the contract creation transaction.
2. Find the password by looking at the storage variables, such as via `web3.eth.getStorageAt`.

## Reading the Contract Creation Code

The contract creation code is composed of contract ABI encoded and the parameters. Therefore some of the digits we see thereat the end are `constructor` arguments. We know that there is only one argument: password as 32 bytes. In hexadecimals it is 64 digits, so we can check the final 64 characters of the contract creation code and that is the password. Note that while calling the `unlock` function you should add `0x` at the beginning of the string.

## Reading Storage

Looking at the Vault contract, the storage is read from top to bottom. At the top, we have `bool public locked;` and after that we have `bytes32 private password;`. These variable are at index 0 and 1 in the storage respectively. We can therefore read the password by `await web3.eth.getStorageAt(contract.address, 1)` and simply give the result as the parameter to `contract.unlock(...)`.