# [0. Hello Ethernaut](https://ethernaut.openzeppelin.com/level/0x4E73b858fD5D7A5fc1c3455061dE52a53F35d966)

Welcome! It is going to be a long but immensely educational road. This level prepares you on how to use the console. Start by typing `help()`! Get comfortable with the commands you see there. You will need some test ether too, so please do visit the faucets linked there and get some ether!

1. To start, click on the big blue button to get a new level instance. This will be your target contract on all puzzles. You will be doing stuff to it, hijacking it's ownership, draining its balance and whatever!

2. The rest of the level starts by calling `contract.info()`. In Chrome v62 (or Brave browser), you can use `await` in the console. That makes things a lot easier; call `await contract.info()`. Note that once you get to `contract.` the console will show you the properties of the contract object, and there are a bunch of functions there!

3. It tells us that we have what we need in `info1()`. So naturally, we will call `await contract.info1()`.

4. We are told to call `info2()` with `"hello"` parameter! We do so with `await contract.info2("hello")`.

5. We are told to check the `infoNum()` property. When we do so with `await contract.infoNum()` we get some weird stuff back in our console. What the hell is this?? Fret not, tis a mere [BigNumber object](https://web3js.readthedocs.io/en/v1.2.11/web3-utils.html#bn). You can convert it to a number or string via `toNumber()` or `toString()`. Once we do so, we see that it denotes the number 42.

```js
{
  negative: 0,
  words: [
    42,
    // empty
  ],
  length: 1,
  red: null
}
```

6. We know what to do already, let's call `await contract.info42()`. We see that we should have called `theMethodName` instead.

7. So we do: `await contract.theMethodName`. Oh, we need to call `method7123949` instead!

8. Once we call `await contract.method7123949()`, we see the following message: "If you know the password, submit it to `authenticate()`". Do we know the password? There certainly seems to be a `password()` function as a property of our `contract` object, so let's call it!

9. After `await contract.password()` we get the password as `ethernaut0`. Submitting this to `await contract.authenticate("ethernaut0")` prompts us to confirm a transaction!

Once that is mined, we seem to have nothing more to do. As the next natural thing to do, we may as well submit our instance (the orange button). Indeed, that is the end of the level! Congratulations, you are now ready for the real deal.
