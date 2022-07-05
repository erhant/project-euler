# [9. King](https://ethernaut.openzeppelin.com/level/0x43BA674B4fbb8B157b7441C2187bCdD2cdF84FD5)

The ponzi starts with 0.001 ether. We can exploit the game by giving an equal/larger ether, but via a contract that disallows receiving ether. This way, if someone is eligible to be the new king, the transaction will fail when it tries to send us the prize!

```solidity
// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.8.0;
 
contract OneWayForward {  

  receive () external payable {
  revert("Im the king!");
  }

  fallback () external payable {
  revert("Im the king!");
  }
  
  function forward(address payable _to) public payable {
  (bool sent, ) = _to.call{value: msg.value}("");
  require(sent, "forwarded call failed");
  }

}
```

## Preventing Money Transfer

The contract is simple: a forward function forwards our sent money to some address. The recieving address will know the contract as `msg.sender`, however they won't be able to send money back. Preventing to recieving money can be done by **not** implementing `receive` and `fallback` functions. In my case, I wanted to be a little bit cheeky and I implement them but inside revert with "Im the king!" message when they send me money ;)

## Call vs. Transfer

Another important point: we used `_to.call{value: msg.value}("")` instead of `_to.transfer(msg.value)`. This is because `transfer` sends 2300 gas to the receiver, but that gas is enough for the code to run on their side; so we must forward all our gas to them with `call`.
