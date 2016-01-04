a=`detex diss.tex | tr -cd '0-9A-Za-z \n' | wc -w`
b=`detex proposal.tex | tr -cd '0-9A-Za-z \n' | wc -w`
(( e = a - b ))
echo $e
