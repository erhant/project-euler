# [16. Preservation](https://ethernaut.openzeppelin.com/level/0x97E982a15FbB1C28F6B8ee971BEc15C78b3d263F)

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.6.0;

contract Preservation {
  // public library contracts 
  address public timeZone1Library;
  address public timeZone2Library;
  address public owner; 
  uint storedTime;
  // Sets the function signature for delegatecall
  bytes4 constant setTimeSignature = bytes4(keccak256("setTime(uint256)"));

  constructor(address _timeZone1LibraryAddress, address _timeZone2LibraryAddress) public {
    timeZone1Library = _timeZone1LibraryAddress; 
    timeZone2Library = _timeZone2LibraryAddress; 
    owner = msg.sender;
  }
 
  // set the time for timezone 1
  function setFirstTime(uint _timeStamp) public {
    timeZone1Library.delegatecall(abi.encodePacked(setTimeSignature, _timeStamp));
  }

  // set the time for timezone 2
  function setSecondTime(uint _timeStamp) public {
    timeZone2Library.delegatecall(abi.encodePacked(setTimeSignature, _timeStamp));
  }
}

// Simple library contract to set the time
contract LibraryContract {
  // stores a timestamp 
  uint storedTime;  

  function setTime(uint _time) public {
    storedTime = _time;
  }
}
```

Here we are in the hands of the almighty `delegatecall`. The given contract actually suffers from a bug, which we used as an exploit in the 6th level (Delegation). When we call `setFirstTime`, it actually overwrites the value in `timeZone1Library` storage variable! Here is what we do:

1. Create a contract that has a function with `setTime(uint256)` signature. This contract should have enough storage variables so that you can overwrite `owner` variable in the caller's context.
2. Set the `timeZone1Library` address to the address of this contract via `setFirstTime(<your contract address>)`.
3. Call `setFirstTime(<whatever>)` again to execute your custom function.
4. Et voila! You are the owner.

A good takeaway from this level, quoting the author's message: "This example demonstrates why the library keyword should be used for building libraries, as it prevents the libraries from storing and accessing state variables."
