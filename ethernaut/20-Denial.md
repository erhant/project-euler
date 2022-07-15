# [20. Denial](https://ethernaut.openzeppelin.com/level/0xf1D573178225513eDAA795bE9206f7E311EeDEc3)

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.6.0;

import '@openzeppelin/contracts/math/SafeMath.sol';

contract Denial {
  using SafeMath for uint256;
  address public partner; // withdrawal partner - pay the gas, split the withdraw
  address payable public constant owner = address(0xA9E);
  uint timeLastWithdrawn;
  mapping(address => uint) withdrawPartnerBalances; // keep track of partners balances

  function setWithdrawPartner(address _partner) public {
    partner = _partner;
  }

  // withdraw 1% to recipient and 1% to owner
  function withdraw() public {
    uint amountToSend = address(this).balance.div(100);
    // perform a call without checking return
    // The recipient can revert, the owner will still get their share
    partner.call{value:amountToSend}("");
    owner.transfer(amountToSend);
    // keep track of last withdrawal time
    timeLastWithdrawn = now;
    withdrawPartnerBalances[partner] = withdrawPartnerBalances[partner].add(amountToSend);
  }

  // allow deposit of funds
  receive() external payable {}

  // convenience function
  function contractBalance() public view returns (uint) {
    return address(this).balance;
  }
}
```

In this level, the exploit has to do with `call` function: `partner.call{value:amountToSend}("")`. Here, a `call` is made to the partner address, with empty `msg.data` and `amountToSend` value. When using `call`, if you do not specify the amount of gas to forward, it will forward everything! As the comment line says, reverting the call will not affect the execution, but what if we consume all gas in that call?

That is the attack. We will write a `fallback` function because the call is made with no message data, and we will just put an infinite loop in there:

```solidity
contract BadPartner { 
  fallback() external payable {
    while (true) {}
  } 
}
```

We then set the withdrawal partner as this contract address, and we are done. Note that `call` can use at most 63/64 of the remaining gas (see [EIP-150](https://github.com/ethereum/EIPs/blob/master/EIPS/eip-150.md)). If 1/64 of the gas is enough to finish the rest of the stuff, you are good. To be safe though, just specify the amount of gas to forward.
