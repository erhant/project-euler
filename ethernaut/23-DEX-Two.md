# [23. DEX II](https://ethernaut.openzeppelin.com/level/0xC084FC117324D7C628dBC41F17CAcAaF4765f49e)

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

We are asked to "drain all of at least 1 of the 2 tokens from the contract, and allow the contract to report a "bad" price of the assets". Let us take a deeper look into the swap function then:

```solidity
function swap(address from, address to, uint amount) public {
  // token addresses must be valid
  require((from == token1 && to == token2) || (from == token2 && to == token1), "Invalid tokens");

  // sender must have enough balance of FROM
  require(IERC20(from).balanceOf(msg.sender) >= amount, "Not enough to swap");

  // calculate the price, we can inline the actual formula here
  //                         DEX Balance of TO
  // swapAmount = amount * ----------------------
  //                        DEX Balance of FROM
  //
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

todo todo todo