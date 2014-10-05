rm counterexample*.csv
rm ./mc2mabs
ghc -O2 -L. -fglasgow-exts -prof --make -auto-all -caf-all -fforce-recomp -lstdc++ -rtsopts abs.cpp agent.cpp ./models/evaluation.cpp MC2MABS.lhs -o mc2mabs &&
sh prof_mc2mabs.sh
#time ./mc2mabs
