let i

for (( i = 0; i < 10; i++ )); do
  ~/mirage-oram/oramMacroBench.native 1000 1 10 > "timeResults$i.dat";
done
