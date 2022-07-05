# Notes

General resources that may be helpful.

## Ether Unit Conversion

I used <https://eth-converter.com/>; and I also wrote one at my [homepage](https://www.erhant.me/tools/ethertools).

## Sending Ether

A quick reference on how to send `_amount` ethers to some payable address `_to`:

- `_to.transfer(_amount)`
  - throws on failure
  - forwards 2300 gas stipend
  - safe against re-entrancy

- `_to.send(_amount)`
  - returns `false` on failure
  - forwards 2300 gas stipend
  - safe against re-entrancy
  - not preferred

- `_to.call{value: _amount}("some data")`
  - returns `false` on failure
  - forwards all available gas; or can specify the gas to forward
  - should be used carefully, open for re-entrancy
  - is the preferred method after Istanbul hard-fork

Note that the recieving contract must have implemented `receive` or `fallback` functions. If not, you can't send money with these methods. You could forcefully send money by the `selfdestruct(_to)` command with some positive balance on the self-destructing contract though.

## Function Accessibilitiy

Function accessibility with respect to keywords:

- **public:** everyone
- **external:** nothing internally, only externally
- **internal:** only this contract and contracts deriving from it
- **private:** only this contract

## Delegate Call

`delegatecall` is one helluva function. todo...

## Casting

- Conversion to smaller type costs more signficant bits. (e.g. `uint32 -> uint16`)
- Conversion to higher type adds padding bits to the left. (e.g. `uint16 -> uint32`)
- Conversion to smaller byte costs less significant bits. (e.g. `bytes32 -> bytes16`)
- Conversion to larger byte add padding bits to the right. (e.g. `bytes16 -> bytes32`)
