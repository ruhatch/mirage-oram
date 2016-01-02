a=`/usr/local/texlive/2014/bin/x86_64-darwin/detex diss.tex | tr -cd '0-9A-Za-z \n' | wc -w`
b=`/usr/local/texlive/2014/bin/x86_64-darwin/detex proposal.tex | tr -cd '0-9A-Za-z \n' | wc -w`
(( e = a - b ))
echo $e
