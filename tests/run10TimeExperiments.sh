let i

for (( i = 0; i < 10; i++ )); do
  ~/mirage-oram/oramMacroBench.native 1000 1 11 > "timeResults$i.dat" 2> "timeResults$i.err";
done
