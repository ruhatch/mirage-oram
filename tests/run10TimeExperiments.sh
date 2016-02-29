let i

for (( i = 0; i < 10; i++ )); do
  ../oramMacroBench.native 1000 1 11 > "timeResults$i.dat";
done
