# skladanie wszystkich plikow *.bib w jeden plik
# o nazwie cytowania.bib
touch cytowania.bib

cd ./pliki_bib/

FILES=*.bib # pliki *.bib z prawidlowo
# wstawionymi cytowaniami przez CL

for f in $FILES
do
    cat ./$f >> ../cytowania.bib
done
