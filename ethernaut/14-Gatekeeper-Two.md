# [14. Gatekeeper II](https://ethernaut.openzeppelin.com/level/0xdCeA38B2ce1768E1F409B6C65344E81F16bEc38d)

Here is another gate puzzle to pass! Again we have three gates:

1. Simple `msg.sender != tx.origin`.
2. A cute `extcodesize` call via inline assembly.
3. A series of `require`'s tells us what the gate key must be like.

## Gate 1

Similar to previous puzzles, just use a contract as a middleman.

## Gate 2

Here is the actual gate:

```solidity
modifier gateTwo() {
  uint x;
  assembly { x := extcodesize(caller()) }
  require(x == 0);
  _;
}
```

The `extcodesize` basically returns the size of the code in the given address, which is caller for this case. Contracts have code, and user accounts do not. To have 0 code size, you must be an account; but wait, how will we pass the first gate if that is the case? Here is the trick of this gate: `extcodesize` returns 0 if it is being called in the `constructor`! Here is a [link](https://ethereum.stackexchange.com/a/15642) to where I stumbled upon this info.

In short, we have to execute our attack from within the constructor.

## Gate 3

This gate has the following form:

```solidity
modifier gateThree(bytes8 _gateKey) {
  require(uint64(bytes8(keccak256(abi.encodePacked(msg.sender)))) ^ uint64(_gateKey) == uint64(0) - 1);
  _;
}
```

It is just an XOR operation, and there is really only one parameter we can control here: the gate key. Well, how do we find it? XOR has the property that if the same value XORs itself they cancel out; furthermore, XOR is commutative so $a \oplus b = b \oplus a$. Let us denote the operation above as $a \oplus b = c$. If we XOR both sides with $a$ we get $a \oplus a \oplus b = c \oplus a$, and the left side cancels out to give $b = c \oplus a$.

One more thing: `(uint64(0) - 1)` causes is not really good for Solidity, and even caused gas estimation errors for me! The result is basically the maximum possible valueof `uint64`, and we have a cool way to find it via `type(uint64).max`.

We can safely find the gate key as:

```solidity
bytes8 key = bytes8(type(uint64).max ^ uint64(bytes8(keccak256(abi.encodePacked(address(this))))));
```

That is all for this one!
