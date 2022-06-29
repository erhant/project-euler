# [Gatekeeper One](https://ethernaut.openzeppelin.com/level/0x9b261b23cE149422DE75907C6ac0C30cEc4e652A)

Wow this was challenging! We must pass 3 obstacles (gates):

1. `msg.sender != tx.origin`: From previous puzzles we have learned that `msg.sender` is the immediate sender of a transaction, which may be a contract; however, `tx.origin` is the originator of the transaction which is usually you.
2. `gasLeft().mod(8191) == 0`: Here we need to adjust the gas used in the transaction. We can do this by specifying the gas to be forwarded similar to how we specify ether value: `foo{gas: ...}()`. To find the proper gas amount is the tricky part, because we don't know when exactly that check will happen.
3. A series of `require`'s give us an idea of what the key must look like. We are using an 8-byte key, so suppose the key is `ABCD` where each letter is 2 bytes (16 bits).
    1. `CD == D` so `C`: must be all zeros.
    2. `CD != ABCD` so `AB` must **not** be all zeros.
    3. `CD == uint16(tx.origin)`: `C` is already zeros, and now we know that `D` will be the last 16-bits of `tx.origin`.

Solution to (1) is trivial, just use a contract. For (2), my `uint16(tx.origin)` is `C274`; and I will just set `AB = 0000 0001` to get `_gateKey = 0x000000010000C274`. The gas problem is the hard part in this puzzle. Here is what I did:

- We can write the contract in VM using Remix, try to enter (assuming gate one and gate three are passing).
- Most likely we will fail due to `gasLeft()` puzzle. That is ok, because we want to debug it!
- Debug the transaction in Remix to get to the [`GAS`](https://github.com/crytic/evm-opcodes) opcode (which is what `gasLeft()` is doing). You can easily get there by:
  - clicking "Click _here_ to jump where the call reverted." and then going backward a bit until you find the opcode.
  - putting a breakpoint to the modulus line and clicking right arrow
- In my case, I had forwarded 10K gas and right at the GAS opcode I had 9746 left. That means I used 254 gas to get there.
- If I start with 8191 + 254 = 8445 gas instead of 10K, I might pass this.

