rm counterexample*.csv
ghc -O2 -L. -fglasgow-exts -prof --make -auto-all -caf-all -fforce-recomp -lstdc++ abs.cpp agent.cpp models/transmission.cpp MC2MABS.lhs -o mc2mabs &&
time ./mc2mabs
