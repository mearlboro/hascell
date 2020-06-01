## Hascell
#### Cellular automata in Haskell

A library that uses an Array instance of comonad to define 1-dimensional and 2-dimensional cellular automata (CA). More about the development of this code as well as running it on various platforms is available on [mis.pm/hascell](https://mis.pm/hascell-wolfram).

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
- Greenberg-Hastings excitable models

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

#### Compiling & Profiling

To compile and profile, we either need to use Nix or to install the required profiling tools, as well as the profiling libraries for each module used and its dependencies:

```
apt install ghc-prof haskell-platform-prof
cabal install -p --force-reinstalls array
cabal install -p --force-reinstalls bitwise
cabal install -p --force-reinstalls base-orphans tagged distributive transformers-compat comonad
cabal install -p --force-reinstalls JuicyPixels
```

Then run the script

```
./profiler.sh
```

which will produce a build of the program that will output profiling information when run. Then run it with the appropriate flags:

```
./out/Main-profiled +RTS -N -p -s -h -i0.1 -RST
```

The profiler produces a number of files that can be parsed with hp2hml and hp2pretty to be human readable.

The flags passed for the first build allow enabling a number of features for multithreading and optimisation

- `-j` for parallel building
- `-threaded` for multithreading
- `-rtsopts` allows CLI flags by enabling runtime system options
- `-outputdir ./out` to specify ouput location for all generated .hi and .o files
- `-main-is Main` to specify the name for the file containing the `main` function (in case it has a specific name)

For the second build the profiler is enabled (`-prof`) and every single expression will be considered (`-fprof-auto`). The flag `-osuf=p_o` allows using a different suffix for the output object files produced in the second pass.
