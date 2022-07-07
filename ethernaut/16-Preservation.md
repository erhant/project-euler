# [16. Preservation](https://ethernaut.openzeppelin.com/level/0x97E982a15FbB1C28F6B8ee971BEc15C78b3d263F)

Here we are in the hands of the almighty `delegatecall`. The given contract actually suffers from a bug, which we used as an exploit in the 6th level (Delegation). When we call `setFirstTime`, it actually overwrites the value in `timeZone1Library` storage variable! Here is what we do:

1. Create a contract that has a function with `setTime(uint256)` signature. This contract should have enough storage variables so that you can overwrite `owner` variable in the caller's context.
2. Set the `timeZone1Library` address to the address of this contract via `setFirstTime(<your contract address>)`.
3. Call `setFirstTime(<whatever>)` again to execute your custom function.
4. Et voila! You are the owner.

A good takeaway from this level, quoting the author's message: "This example demonstrates why the library keyword should be used for building libraries, as it prevents the libraries from storing and accessing state variables."
