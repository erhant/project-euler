# [2. Fallout](https://ethernaut.openzeppelin.com/level/0x5732B2F88cbd19B6f01E3a96e9f0D90B917281E5)

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.6.0;

import '@openzeppelin/contracts/math/SafeMath.sol';

contract Fallout {
  
  using SafeMath for uint256;
  mapping (address => uint) allocations;
  address payable public owner;
 
  /* constructor */
  function Fal1out() public payable {
    owner = msg.sender;
    allocations[owner] = msg.value;
  }

  modifier onlyOwner {
    require(
      msg.sender == owner,
      "caller is not the owner"
    );
    _;
  }

  function allocate() public payable {
    allocations[msg.sender] = allocations[msg.sender].add(msg.value);
  }

  function sendAllocation(address payable allocator) public {
    require(allocations[allocator] > 0);
    allocator.transfer(allocations[allocator]);
  }

  function collectAllocations() public onlyOwner {
    msg.sender.transfer(address(this).balance);
  }

  function allocatorBalance(address allocator) public view returns (uint) {
    return allocations[allocator];
  }
}
```

Prior to the `constructor` function, the constructor was used as the function that has the same name with the contract. However, if by any chance the supposed constructor function has a different name, you are open to attacks! In this case, the name is `Fallout` but the function is written as `Fal1out`.

This actually happened to [Rubixi](https://github.com/crytic/not-so-smart-contracts/tree/master/wrong_constructor_name). The author initially used DynamicPyramid as the contract name, and therefore the constructor. Later, he only changed the contract name to Rubixi and forgot the DynamicPyramid constructor as is, effectively leaving it up for grabs. Someone did grab eventually.
