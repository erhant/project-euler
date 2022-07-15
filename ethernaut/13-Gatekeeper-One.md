# [13. Gatekeeper One](https://ethernaut.openzeppelin.com/level/0x9b261b23cE149422DE75907C6ac0C30cEc4e652A)

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.6.0;

import '@openzeppelin/contracts/math/SafeMath.sol';

contract GatekeeperOne {
  using SafeMath for uint256;
  address public entrant;

  modifier gateOne() {
    require(msg.sender != tx.origin);
    _;
  }

  modifier gateTwo() {
    require(gasleft().mod(8191) == 0);
    _;
  }

  modifier gateThree(bytes8 _gateKey) {
    require(uint32(uint64(_gateKey)) == uint16(uint64(_gateKey)), "GatekeeperOne: invalid gateThree part one");
    require(uint32(uint64(_gateKey)) != uint64(_gateKey), "GatekeeperOne: invalid gateThree part two");
    require(uint32(uint64(_gateKey)) == uint16(tx.origin), "GatekeeperOne: invalid gateThree part three");
    _;
  }

  function enter(bytes8 _gateKey) public gateOne gateTwo gateThree(_gateKey) returns (bool) {
    entrant = tx.origin;
    return true;
  }
}
```

Wow this was challenging! We must pass 3 obstacles (gates) that are implemented as modifiers:

1. Simple `msg.sender != tx.origin`.
2. A cute `gasLeft().mod(8191) == 0`.
3. A series of `require`'s telling us what the gate key must look like.

## Gate 1

Solution to the first gate is trivial, just use a contract as a middleman. From previous puzzles we have learned that `msg.sender` is the immediate sender of a transaction, which may be a contract; however, `tx.origin` is the originator of the transaction which is usually you.

## Gate 2

Here we need to adjust the gas used in the transaction. We can do this by specifying the gas to be forwarded similar to how we specify ether value: `foo{gas: ...}()`. To find the proper gas amount is the tricky part, because we don't know exactly how much gas we will have by then. Here is what we can do: we will find a good approximate gas value, and then brutely try a range of values around it. The steps to do that is as follows:

1. Copy paste the contract in Remix, and try to enter the gate (assuming that gate 1 is passing at this point). I wrote a small utility for this in my attacker contract:

```solidity
  function enterOnce(uint _gas) public {
    bytes memory callbytes = abi.encodeWithSignature(("enter(bytes8)"),key);
    (bool success, ) = target.call{gas: _gas}(callbytes);
    require(success, "failed my boy.");
  } 
```

2. Unless you are extremely lucky, the transaction will be rejected by this gate. That is ok, because we want to debug it!

3. Debug the transaction in Remix to get to the [`GAS`](https://github.com/crytic/evm-opcodes) opcode, which is what `gasleft()` is doing in the background. There, we will look at the `remaining gas` field in "Step Details". You can easily get there in several ways:

    - Clicking "Click _here_ to jump where the call reverted." and then going backward a bit until you find the opcode.
    - Putting a breakpoint to the line with `gasleft()` and clicking right arrow at the debugger, which will go very close to that opcode.
    - Another cool way is to actually get inside the SafeMath libraries modulus function, and then look at the local variables in the debugger. One of them will be 8191, the other will be the gas in question.

4. In my case, I had forwarded $10000$ gas and right at the `GAS` opcode I had $9748$ left. That means I used $252$ gas to get there. If I start with $8191 \times k + 252$ gas for some large enough $k$ to meet the overall gas requirement, I should be okay! The thing is, gas usage can change with respect to the compiler version, but in the puzzle we see that `^0.6.0` is used above, so we will do all the steps above with that version.

5. I set the gas candidate as $8191 \times 5 + 252 = 41207$ with a margin of $32$. Then I let it loose on the gate keeper!

```solidity
  function enter(uint _gas, uint _margin) public { 
    bytes memory callbytes = abi.encodeWithSignature(("enter(bytes8)"),key);
    bool success;
    for (uint g = _gas - _margin; g <= _gas + _margin; g++) {
      (success, ) = target.call{gas: g}(callbytes);
      if (success) {
        correctGas = g; // for curiosity
        break;
      }
    }
    require(success, "failed again my boy.");
  }
```

6. It was successfull, and I also kept record of the correct gas amount, which turned out to be $41209$.

## Gate 3

We are using an 8-byte key, so suppose the key is `ABCD` where each letter is 2 bytes (16 bits).

  1. `CD == D` so `C`: must be all zeros.
  2. `CD != ABCD` so `AB` must **not** be all zeros.
  3. `CD == uint16(tx.origin)`: `C` is already zeros, and now we know that `D` will be the last 16-bits of `tx.origin`.

So, my `uint16(tx.origin)` is `C274`; and I will just set `AB = 0x 0000 0001` to get `_gateKey = 0x 0000 0001 0000 C274`. Alternatively, you can use bitwise masking by bitwise-and'ing (`&`) your `tx.origin` with `0x FFFF FFFF 0000 FFFF`.

That is all folks :)
