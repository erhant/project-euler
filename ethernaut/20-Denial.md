# [20. Denial](https://ethernaut.openzeppelin.com/level/0xf1D573178225513eDAA795bE9206f7E311EeDEc3)

In this level, the exploit has to do with `call` function: `partner.call{value:amountToSend}("")`. Here, a `call` is made to the partner address, with empty `msg.data` and `amountToSend` value. When using `call`, if you do not specify the amount of gas to forward, it will forward everything! As the comment line says, reverting the call will not affect the execution, but what if we consume all gas in that call?

That is the attack. We will write a `fallback` function because the call is made with no message data, and we will just put an infinite loop in there:

```solidity
contract BadPartner { 
  fallback() external payable {
    while (true) {}
  } 
}
```

We then set the withdrawal partner as this contract address, and we are done. Note that `call` can use at most 63/64 of the remaining gas. If 1/64 of the gas is enough to finish the rest of the stuff, you are good. To be safe though, just specify the amount of gas to forward.
