# wymaga bibutils

mkdir ./pliki_medline_xml ./pliki_bib
cd ./pliki_medline/ # katalog utworzony wczesniej przez skrypt *.lisp

# konwersja plikow z PubMed xml do MODS xml
FILES=*_medline # pliki medline utworzyl lisp z informacji z PubMedu
for f in $FILES;
do
    med2xml ./$f > ../pliki_medline_xml/$f.xml
done

cd ../pliki_medline_xml/

# konwersja plikow z MODS xml do bibtex
FILES=*.xml
for f in $FILES;
do
    # xml2bib -o unicode ./$f > ../pliki_bib/$f.bib # jesli pelna nazwa journala
    # xml2bib -o unicode -at ./$f > ../pliki_bib/$f.bib # jesli abbreviated nazwa journala
    #do_wstawienia_przez_cl_pelny_or_abbreviated_journal
done


cd ../pliki_bib/

# zamiana plikow z formatu *_medline.xml.bib do formatu *.bib
mmv '*_medline.xml.*' '#1.#2'
