# [17-Recovery](https://ethernaut.openzeppelin.com/level/0x0EB8e4771ABA41B70d0cb6770e04086E5aee5aB2)

My initial solution was to check the internal transactions of the contract creation transaction of my level instance. There, we can very well see the "lost" contract address, and we can call the `destroy` function there with our address as follows:

```js
web3.eth.call({
  from: player,
  to: '0x559905e90cF45D7495e63dA1baEFB54d63B1436A',
  data: web3.utils.encodePacked('0x00f55d9d', web3.utils.padLeft(player, 64))
})
```

For more information on how `data` is constructed, see [Solidity docs](https://docs.soliditylang.org/en/latest/abi-spec.html#examples).

An alternative solution I have learned later has to do with how contract factories child contract addresses are defined. This is deterministic, and we can calculate it. todo...



