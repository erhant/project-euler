# [17-Recovery](https://ethernaut.openzeppelin.com/level/0x0EB8e4771ABA41B70d0cb6770e04086E5aee5aB2)

My initial solution was to check the internal transactions of the contract creation transaction of my level instance. There, we can very well see the "lost" contract address, and we will call the `destroy` function there. To call a function with arguments, you need to provide a `calldata` (see [here](https://docs.soliditylang.org/en/latest/abi-spec.html#examples)). The arguments are given in chunks of 32-bytes, but the first 4 bytes of the `calldata` indicate the function to be called. That is calculated by the first 4 bytes of the function's canonical form. There are several ways to find it:

- Use a tool online, such as the [one I wrote](https://www.erhant.me/tools/ethertools).
- Write a bit of Solidity code and calculate `bytes4(keccak256("destory(address)"))`, which requires you to hand-write the canonical form.
- Write a small contract and run it locally (such as Remix IDE with VM) as follows:

```solidity
contract AAA { 
  // this is the same function from ethernaut
  function destroy(address payable _to) public {
    selfdestruct(_to);
  }

  // we can directly find its selector
  function print() public pure returns (bytes4) {
    return this.destroy.selector;
  }
}
```

With any of the methods above, we find the function selector to be `0x00f55d9d`. We can then call the `destroy` function as follows:

```js
const functionSelector = '0x00f55d9d';
await web3.eth.sendTransaction({
  from: player,
  to: '0x559905e90cF45D7495e63dA1baEFB54d63B1436A', // the lost & found address
  data: web3.utils.encodePacked(functionSelector, web3.utils.padLeft(player, 64))
})
```

## Original Solution

Upon sending my solution to Ethernaut, I have learned the actual solution in the message afterwards! Turns out that contract addresses are deterministic and are calculated by `keccack256(RLP_encode(address, nonce))`. The nonce for a contract is the number of contracts it has created. All nonce's are 0 for contracts, but they become 1 once they are created (their own creation makes the nonce 1).

Read about RLP encoding in the Ethereum docs [here](https://ethereum.org/en/developers/docs/data-structures-and-encoding/rlp). We want the RLP encoding of a 20 byte address and a nonce value of 1, which corresponds to the list such as `[<20 byte string>, <1 byte integer>]`.

For the string:

> if a string is 0-55 bytes long, the RLP encoding consists of a single byte with value 0x80 (dec. 128) plus the length of the string followed by the string. The range of the first byte is thus [0x80, 0xb7] (dec. [128, 183]).

For the list, with the string and the nonce in it:

> if the total payload of a list (i.e. the combined length of all its items being RLP encoded) is 0-55 bytes long, the RLP encoding consists of a single byte with value 0xc0 plus the length of the list followed by the concatenation of the RLP encodings of the items. The range of the first byte is thus [0xc0, 0xf7] (dec. [192, 247]).

This means that we will have:

```text
[
  0xC0
    + 1 (a byte for string length) 
    + 20 (string length itself) 
    + 1 (nonce), 
  0x80
    + 20 (string length),
  <20 byte string>,
  <1 byte nonce>
]
```

In short: `[0xD6, 0x94, <address>, 0x01]`. We need to find the `keccak256` of the packed version of this array, which we can find via:

```js
web3.utils.soliditySha3(
  '0xd6',
  '0x94',
  // <instance address>,
  '0x01'
)
```

The last 20 bytes of the resulting digest will be the contract address! Calling the `destroy` function is same as above.
