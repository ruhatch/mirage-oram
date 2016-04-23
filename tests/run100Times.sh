let i

for (( i = 0 ; i < 100 ; i++ ));
do ../oramMacroBench.native 10000 1 17 > "results10k$i.dat";
done;
