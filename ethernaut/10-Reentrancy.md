# [Re-entrancy](https://ethernaut.openzeppelin.com/level/0xe6BA07257a9321e755184FB2F995e0600E78c16D)

There is a pattern called [Checks - Effects - Interactions](https://fravoll.github.io/solidity-patterns/checks_effects_interactions.html) in Solidity. Basically:
    1. you **check** whether you can do something, such as checking balance
    2. you apply the **effects** of doing it on your contract, such as updating balance
    3. you do the actual **interactions** on-chain with other, such as transferring money

In this case, the function is to withdraw but the **interaction** comes before the **effect**. This means that when we receive money via withdraw, things are briefly in our control until the program goes back to the withdraw function to do the effect. When we have the control, we can call withdraw once more and the same thing will happen again and again.

When we create the instance in this game, we can see that:
    - `await getBalance(contract.address)` is 0.001 ether.
    - `await contract.balanceOf(player)` is 0.

```solidity

// todo

```

We will donate some money to create our initial balance at the target, which will allow the `balances[msg.sender] >= _amount` to be true. Now, we can repeadetly withdraw that amount by re-entering the withdraw function. Since balance update effect happens after the transfer interaction, we will go on and on until the balance is depleted.

_Note: This is how "The DAO" hack was executed, which resulted in the creation of Ethereum Classic._
