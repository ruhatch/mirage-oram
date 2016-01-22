let i

for (( i = 0 ; i < 144 ; i++ ));
do ../encryptionTests.native test "Encryption Tests $i" --json | grep -o '\d*\.\d*' >> results.dat;
done;
