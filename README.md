## Hascell
#### Cellular automata in Haskell

A library that uses an Array instance of comonad to define 1-dimensional and 2-dimensional cellular automata (CA). More about the development of this code is available on [mis.pm/hascell](https://mis.pm/hascell-wolfram).

```
 src
 ├── Hascell
 │   ├── Conway.hs
 │   ├── Graphics.hs
 │   ├── Simulate1D.hs
 │   ├── Simulate2D.hs
 │   ├── Simulate.hs
 │   └── Wolfram.hs
 └── Main.hs
```
Implements the following models:

- Wolfram's elementary CA
- Conway's Game of Life


#### Running the code

Note (2020-04): Currently the project fails to build on some Linux distributions due to dependencies failing to compile.

The project can be compiled and run using a Nix sandbox as follows:

```
# setup Nix
curl https://nixos.org/nix/install | sh
source ~/.nix-profile/etc/profile.d/nix.sh

# cd into code folder and use the Nix shell
cd ~/hascell
nix-shell shell.nix
```
This will open up the Nix shell where the code should be able to compile without issue:
```
[nix-shell:~/hascell/]$ ghc src/Hascell/* src/Main.hs
```

#### Parallelisation

Due to the design of comonads computing the next state of each cell can be done in parallel. In order to achieve that in Haskell only some compilation flags are necessary:

```
[nix-shell:~/hascell/]$ ghc -threaded -O3 src/Main.hs src/Hascell/*.hs -o hascell
```
then to run specify the number of threads as a flag

```
./hascell +RTS -N4 -RTS
```


