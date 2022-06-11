# Delegation

The `delegatecall` is an important function. Normally, contracts call functions by making [message calls](https://docs.soliditylang.org/en/v0.4.21/introduction-to-smart-contracts.html#message-calls). `delegatecall` is a more specialized call, see more about it: [docs](https://docs.soliditylang.org/en/v0.4.21/introduction-to-smart-contracts.html#delegatecall-callcode-and-libraries) and also [this article](https://eip2535diamonds.substack.com/p/understanding-delegatecall-and-how?s=r).

The attack in this example is just one line: 

```js
await sendTransaction({
    from: player,
    to: contract.address,
    data: "0xdd365b8b"
})
```

Now let us look at the `data` part: EVM calls functions by looking at the **first 4 bytes of the function signature**. The function signature is `keccak256` (i.e. `sha3`) of the function prototype. In this case, `web3.utils.sha3('pwn()').slice(2, 2 + 4 * 2)` gives us `dd365b8b`. If there were function parameters, we would give them as 32 bytes for each, but in this case there are no parameters so we only write the function signature as data.

When we call Delegation contract with this, it will go to fallback function. There, a delegatecall is made with msg.data as the parameter, so it will call `pwn` function of Delegate.

The actual exploit has to do with storage. Notice that both contracts have `address public owner;` as their first slot in storage. When you use `delegatecall`, the caller's storage is active and the callee can update it, with respect to the slots. As we see, `pwn` updates `owner` and this in effect updates the caller's storage value at the same slot, which is again the owner address.


