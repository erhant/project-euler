# [2. Fallout](https://ethernaut.openzeppelin.com/level/0x5732B2F88cbd19B6f01E3a96e9f0D90B917281E5)

Prior to the `constructor` function, the constructor was used as the function that has the same name with the contract. However, if by any chance the supposed constructor function has a different name, you are open to attacks! In this case, the name is `Fallout` but the function is written as `Fal1out`. 

This actually happened to [Rubixi](https://github.com/crytic/not-so-smart-contracts/tree/master/wrong_constructor_name). The author initially used DynamicPyramid as the contract name, and therefore the constructor. Later, he only changed the contract name to Rubixi and forgot the DynamicPyramid constructor as is, effectively leaving it up for grabs. Someone did grab eventually.
