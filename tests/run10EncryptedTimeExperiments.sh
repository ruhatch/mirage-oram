let i

for (( i = 0; i < 10; i++ )); do
  ../encryptedTimeExperiment.native 1000 1 11 > "encTimeResults$i.dat";
done
