let i

for (( i = 0 ; i < 50 ; i++ ));
do ~/mirage-oram/controlMacroBench.native 1000 1 11 > "controlResults$i.dat";
done;
