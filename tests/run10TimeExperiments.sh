let i

for (( i = 0; i < 10; i++ )); do
  ../oramMacroBench.native 100 1 11 > "timeResults$i.dat";
done
