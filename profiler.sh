# Profiling guide:
# https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#time-and-allocation-profiling
#
# Default RTS flags:
# +RTS -N -P -s -h -i0.1 -RST
#
# To run:
# ./out/Main-profiled +RTS -N -p -s -h -i0.1 -RTS

mkdir -p ./out

ghc                \
  ./src/*.hs       \
  ./src/*/*.hs     \
  -j               \
  -Rghc-timing     \
  -threaded        \
  -rtsopts         \
  -o ./out/Main    \
  -outputdir ./out \
  -main-is Main

ghc                      \
  ./src/*.hs             \
  ./src/*/*.hs           \
  -j                     \
  -Rghc-timing           \
  -threaded              \
  -rtsopts               \
  -o ./out/Main-profiled \
  -outputdir ./out       \
  -main-is Main          \
  -prof                  \
  -fprof-auto            \
  -osuf=p_o
