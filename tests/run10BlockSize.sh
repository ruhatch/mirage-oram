let i

for (( i = 0; i < 10; i++ )); do
  ../blockSizeExperiment.native 10 1 6 > "blockSizeResults$i.dat";
done
