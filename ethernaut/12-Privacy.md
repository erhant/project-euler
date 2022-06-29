# [Privacy](https://ethernaut.openzeppelin.com/level/0x11343d543778213221516D004ED82C45C3c8788B)

This is similar to the 8th level [Vault](./8-Vault.md), where we read the EVM storage. Here in addition to that, we learn about a [small optimization of EVM](https://docs.soliditylang.org/en/v0.8.13/internals/layout_in_storage.html) and [how casting works](https://www.tutorialspoint.com/solidity/solidity_conversions.htm).

EVM stores state variables in chunks of 32 bytes. If consecutive variables make up a 32-byte space (such as in this case 8 + 8 + 16 = 32) they are stored in the same chunk. If you were to write them elsewhere, this optimization may not have happened.

Let us check the results of `await web3.eth.getStorageAt(contract.address, i)` for the following values of `i`:

- `0: 0x0000000000000000000000000000000000000000000000000000000000000001`
This is the `bool public locked = true` which is stored as 1.
- `1: 0x0000000000000000000000000000000000000000000000000000000062bc6f36`
This is the `uint256 public ID = block.timestamp` which is the UNIX timestamp in hex, `62bc6f36` (of this [block](https://rinkeby.etherscan.io/block/10937345) in my [instance](https://rinkeby.etherscan.io/address/0x99181B0E39A3b17fc44f99972bF3E6Afd6296a07)])
- `2: 0x000000000000000000000000000000000000000000000000000000006f36ff0a`
This is the 32 byte chunk of 3 variables all captures in `6f36ff0a` : 
  - `uint8 private flattening = 10` which is `0a`
  - `uint8 private denomination = 255` which is `ff`
  - `uint16 private awkwardness = uint16(now)` which is `6f36`.
Well, that `awkwardness` variable is just the `block.timestamp` casted to 16-bits. We already know the actual 256-bit (32-byte) value of timestamp above: `62bc6f36`. When casted down 16-bits, it became `6f36` (4 x 4-bit hexadecimals).
- `3: 0x0ec18718027136372f96fb04400e05bac5ba7feda24823118503bff40bc5eb55`
This is `data[0]`.
- `4: 0x61a99635e6d4b7233a35f3d0d5d8fadf2981d424110e8bca127d64958d1e68c0`
This is `data[1]`.
- `5: 0x46b7d5d54e84dc3ac47f57bea2ca5f79c04dadf65d3a0f3581dcad259f9480cf`
This is `data[2]`.

Now we just need `data[2]` casted down to `bytes16`. Here is how casting works in very few words:

- Conversion to smaller type costs more signficant bits. (e.g. `uint32 -> uint16`)
- Conversion to higher type adds padding bits to the left. (e.g. `uint16 -> uint32`)
- Conversion to smaller byte costs less significant bits. (e.g. `bytes32 -> bytes16`)
- Conversion to larger byte add padding bits to the right. (e.g. `bytes16 -> bytes32`)

So, when we cast down `data[2]` we will get the left-half of it: `'0x46b7d5d54e84dc3ac47f57bea2ca5f79c04dadf65d3a0f3581dcad259f9480cf'.slice(0, 2 + 32)` and then `await contract.unlock('0x46b7d5d54e84dc3ac47f57bea2ca5f79')`. That is all!

Here is a good article on reading storage: <https://medium.com/@dariusdev/how-to-read-ethereum-contract-storage-44252c8af925>.