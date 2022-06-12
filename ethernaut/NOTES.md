# Sending Ether

A quick reference on how to send `_amount` ethers to some payable address `_to`:

- `_to.transfer(_amount)`
  - throws on failure
  - forwards 2300 gas stipend
  - safe against re-entrancy
  - is the preferred safe way to send ether
- `_to.send(_amount)`
  - returns `false` on failure
  - forwards 2300 gas stipend
  - safe against re-entrancy
  - not preferred
- `_to.call{value: _amount}("some data")`
  - returns `false` on failure
  - forwards all available gas; or can specify the gas to forward
  - should be used carefully, open for re-entrancy

Note that the recieving contract must have implemented `receive` or `fallback` functions. If not, you can't send money with these methods. You could forcefully send money by the `selfdestruct(_to)` command with some positive balance on the self-destructing contract though.
