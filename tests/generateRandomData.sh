let i
for ((i = 0; i < 50; i++));
do dd if=/dev/urandom of=testFiles/random/random$i.txt bs=400k count=1;
done;
