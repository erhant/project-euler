# [6. Delegation](https://ethernaut.openzeppelin.com/level/0x9451961b7Aea1Df57bc20CC68D72f662241b5493)

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.6.0;

contract Delegate {
  address public owner;

  constructor(address _owner) public {
    owner = _owner;
  }

  function pwn() public {
    owner = msg.sender;
  }
}

contract Delegation {
  address public owner;
  Delegate delegate;

  constructor(address _delegateAddress) public {
    delegate = Delegate(_delegateAddress);
    owner = msg.sender;
  }

  fallback() external {
    (bool result,) = address(delegate).delegatecall(msg.data);
    if (result) {
      this;
    }
  }
}
```

The `delegatecall` is an important function. Normally, contracts call functions by making [message calls](https://docs.soliditylang.org/en/latest/introduction-to-smart-contracts.html#message-calls). [`delegatecall`](https://docs.soliditylang.org/en/latest/introduction-to-smart-contracts.html#delegatecall-callcode-and-libraries) is a more specialized call, basically forwarding a contract's context to some other contract and let it do whatever it wants with it. This is useful to implement libraries that might work on your storage variables, or have upgradable contracts where a proxy makes delegate calls to various different contracts over time.

During a `delegatecall`, the following do not change:

- `msg.sender`
- `msg.value`
- `address(this)`
- The storage layout (we will exploit this in this challenge)

I would like refer to this article that explains how delegate calls work really well: <https://eip2535diamonds.substack.com/p/understanding-delegatecall-and-how?s=r>.

The attack in this example is just one transaction:

```js
await sendTransaction({
    from: player,
    to: contract.address,
    data: "0xdd365b8b"
})
```

Now let us look at the `data` part: EVM calls functions by looking at the **first 4 bytes of the function signature**. The function signature is `keccak256` (i.e. `sha3`) of the function prototype. In this case, `web3.utils.sha3('pwn()').slice(2, 2 + 4 * 2)` gives us `dd365b8b`. If there were function parameters, we would give them as 32 bytes for each, but in this case there are no parameters so we only write the function signature as data.

When we call Delegation contract with this, it will go to fallback function. There, a delegatecall is made with `msg.data` as the parameter, so it will call `pwn` function of Delegate.

The actual exploit has to do with storage. Notice that both contracts have `address public owner` at their first slot in storage. When you use `delegatecall`, the caller's storage is active and the callee can update it, with respect to the slots. As we see, `pwn` updates `owner` and this in effect updates the caller's storage value at the same slot, which is again the owner address.

The storage variable assignment within `pwn` therefore effects the contract which made `delegatecall`, and we become the owner.
