# [19. Alien Codex](https://ethernaut.openzeppelin.com/level/0xda5b3Fb76C78b6EdEE6BE8F11a1c31EcfB02b272)

The problem is hinting us to somehow use the `codex` array to change the owner of the contract. The tool in doing so probably has something with the `length` of array. In fact, the `retract` is suspiciously dangerous, and actually might 'underflow' the array length!. The array length is an `uint256`, and once it is underflowed you basically "have" the entire contract storage (all $2^256 - 1$ slots) as a part of your array. Consequently, you can index everything in the memory with that array!

- After `make_contact`, we see that `await web3.eth.getStorageAt(contract.address, 0)` returns `0x000000000000000000000001da5b3fb76c78b6edee6be8f11a1c31ecfb02b272`. Remember that smaller than 32-bytes variables are bundled together if they are conseuctive, so this is actually `owner` and `contact` variable side by side! The `01` at the end of leftmost `0x00..01` stands for the boolean value.
- The next slot, `await web3.eth.getStorageAt(contract.address, 1)` is the length of `codex` array. If you record something you will see that it gets incremented. Well, what if we `retract`? You will be shocked to see that it becomes `0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff`!

So then, how does indexing work and how can we index the `owner` slot now that our array covers the entire storage? We look at the docs of highest version 0.5.0 as that is what the puzzle uses: <https://docs.soliditylang.org/en/v0.5.17/miscellaneous.html#mappings-and-dynamic-arrays>.

> The mapping or the dynamic array itself occupies a slot in storage at some position p according to the above rule (or by recursively applying this rule for mappings of mappings or arrays of arrays). For dynamic arrays, this slot stores the number of elements in the array. Array data is located at keccak256(p).

To see this in action, we can do:

```js
await contract.record('0xffffffffffffffffffffffffffffffff')
await web3.eth.getStorageAt(contract.address , web3.utils.hexToNumberString(web3.utils.soliditySha3(1)))
// 0xffffffffffffffffffffffffffffffff00000000000000000000000000000000
```

Alright, so first we have to `retract` until the array length underflows, and then we just have to offset enough from `keccak256(1)` until we overflow and get back to 0th index, overwriting the `owner`! The array data is located at `uint256(keccak256(1))` and there are `2 ** 256 - 1 - uint256(keccak256(1))` values between that and the end of memory. So, just adding one more to that would mean we go to 0th index. To calculate this index I just wrote a small Solidity code in Remix:

```solidity
function index() public pure returns(uint256) {
  return type(uint256).max - uint256(keccak256(abi.encodePacked(uint256(1)))) + 1; 
}
```

Then I call the `revise` function as follows:

```js
await contract.codex('35707666377435648211887908874984608119992236509074197713628505308453184860938') // if you want to confirm
await contract.revise('35707666377435648211887908874984608119992236509074197713628505308453184860938', web3.utils.padLeft(player, 64))
```

Note: You can't set the array length property since version 0.6.0, thankfully! See <https://ethereum.stackexchange.com/a/84130>.
