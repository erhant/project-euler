# [18. Magic Number](https://ethernaut.openzeppelin.com/level/0x200d3d9Ac7bFd556057224e7aEB4161fED5608D0)

In this level, we have to write a contract that returns 42 in as little as 10 opcodes.

When I write the following contract:

```solidity
contract Solver { 
  function whatIsTheMeaningOfLife() public pure returns (uint) {
    return 42;
  }
}
```

and deploy it, I see that there are waaay more than 10 opcodes when I call the function and check the opcodes in the Remix IDE debugger. So, we need to somehow write our own assembly there. To do this, we will become the compiler and do barebones contract creation: a transaction to address `0x0` with some data that is our contract! Contracts are made of two parts: initialization and runtime. Let us do the runtime first, since we know what to do there: return 42 somehow!

## Runtime Code

I remembered the days I was taking an Assembly x8086 class back in my bachelor's, where we had to bring a bunch of papers stabled together, with all x8086 instructions on it! Our documentation here for opcodes will be <https://www.ethervm.io/>. You can also check <https://www.evm.codes/>.

1. I CTRL+F to search "return", and first check the [`RETURN`](https://www.ethervm.io/#F3) opcode: `RETURN <offset> <length>`. Apparently, it returns `length` bytes from the `offset` in memory. So we need to store our 42 in **memory** first.
2. I CTRL+F "memory" to find the [related section](https://www.ethervm.io/#memory), and there we have 3 instructions. I find [`MSTORE`](https://www.ethervm.io/#52) to be good for our use-case. `MSTORE <offset> <value>`. Now we need to provide the actual data that these instructions read from the **stack**. _Note: using `MSTORE8` did not work._
3. I CTRL+F "stack" to find the [related section](https://www.ethervm.io/#stack) and there we find [`PUSH1`](https://www.ethervm.io/#60) to be useful for us. How to provide argument to this guy? Here is the answer:

> Each opcode is encoded as one byte, except for the PUSH opcodes, which take a immediate value. All opcodes pop their operands from the top of the stack and push their result.

So here is the plan:

```c
PUSH1 0x2A // our 1 byte value 42 = 0x2A
PUSH1 0x80 // memory position 0x80, the first free slot
MSTORE     // stores 0x2A at 0x80
PUSH1 0x20 // to return an uint256, we need 32 bytes (not 1)
PUSH1 0x80 // position to return the data
RETURN     // returns 32 bytes from 0x80
```

The memory slot `0x80` is very important to note. I initially wrote to other smaller memory slots but my solution was not accepted; turns out that the first 4 32-byte slots are reserved! Read more at <https://docs.soliditylang.org/en/v0.8.13/internals/layout_in_memory.html>.

In terms of bytecode, we need all of these written consecutively as one big chunk, with the actual opcodes instead of instructions. `PUSH1` is `60`, `MSTORE` is `52` and `RETURN` is `F3`. Writing everything side by side we get: `60 2A 60 80 52 60 20 60 80 F3`; our brand new runtime code; exactly 10 bytes!

## Initialization Code

So how exactly do we tell EVM to use that thing above as our runtime code? We need to write the initalization part too. In the [contract creation section](https://www.ethervm.io/#contract-creation) we see that:

> The data payload of a transaction creating a smart contract is itself bytecode that runs the contract constructor, sets up the initial contract state and returns the final contract bytecode.

Aha, we have to "return the final contract bytecode". So we need to somehow put our code in memory at some index, and `return` just like above. At this point:

1. I CTRL+F "contract" and stumble upon [`CODECOPY`](https://www.ethervm.io/#39) instruction, which seems to be just what we need: putting code in memory. `CODECOPY <destOffset> <offset> <length>` puts the code at `offset` with `length` bytes to memory at `destOffset`. The `offset` refers to the actual bytecode, so this will be the starting index of our runtime code above. However, we do not know that until we finish writing the initialization code, because runtime code comes **after** it.
2. The return part is same as above, `RETURN <offset> <length>` where `offset` is the index of our runtime code and `length` is the length of it, which we know to be 10 bytes.

Our initialization code is thus:

```c
PUSH1 0x0a // 10 bytes
PUSH1 ;;;; // position in bytecode, we dont know yet
PUSH1 0x00 // write to memory position 0
CODECOPY   // copies the bytecode 
PUSH1 0x0a // 10 bytes
PUSH1 0x00 // read from memory position 0
RETURN     // returns the code copied above
```

Writing this in bytecode gives us `60 0a 60 ;; 60 00 39 60 0a 60 00 F3` which is 12 bytes. So that dummy `;;;;` has to be 12, i.e. `0x0C`.

## Deploying the Contract

In Ethereum, any transaction that is targeted at `0x0` is a contract creation transaction, so we will do a call like:

```js
await web3.eth.sendTransaction({
  from: player,
  to: 0, // contract creation 
  data: '0x600a600C600039600a6000F3602a60805260206080F3' // bytecodes
})
```

The returned object in console will have a `contractAddress` if everything goes well. You can confirm that the bytecode is correct by checking it on <https://rinkeby.etherscan.io/>, and look at the opcodes by clicking "Switch To Opcodes View" button under the "Contract" tab. Afterwards, just set the solver to this contract address and submit!
