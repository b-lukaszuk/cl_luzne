;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                   ; NAGLOWEK
                              ; INFORMACJE OGOLNE
         ; wydobywanie pmid z artykulu, formatowanie ich pod Pandoc-a ;
                             ; pisane na kolanie ;
           ; ale potem popoprawiane wiec jest raczej niezle/dobrze ;
                  ; NIE ZALECANE DO UZYTKU PRZEZ NIKOGO ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;         BIBLIOTEKI        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :cl-ppcre) ; wyrazenia regularne
(ql:quickload :drakma) ; sciaganie plikow z neta
(ql:quickload :asdf) ; do wywolywania komend bash-owych




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;          Funkcje          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(Defun my/get-file (filename) ; wczytuje plik jako liste linii tekstu
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun my/concat (lst) ; zamiana listy stringow w 1 dlugi string
  (format nil "~{~a~%~}" lst))

(defun my/string<-file (filename) ; wczytuje plik jako 1 dlugi string
  (my/concat
   (my/get-file filename)))

(defun my/str->file (string-obj filename) ; zapisuje obiekt (1 dlugi string) do pliku
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede)
    (format stream "~a" string-obj))) ; zapisz plik do katalogu

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
               ;      ponizej proba (chyba udana -> testowac)   ;
          ; napisania funkcji uppercase-ujacej 1 litere kazdego wyrazu
                     ; (oprocz krotkich "and", "of", itp.)
                      ; w nazwie journala w pliku "*.bib"
          ; (moze bylc tylko 1 linijka zaczynajaca sie od "journal=")
      ; (niestety regex-y w cl-ppcre nie wspieraja \U (uppercase-owania))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bierze string z 1 charem i
;; zwraca reprezentacje CL, np. "a" -> #\a
(defun my/str->char (x)
  (aref x 0))

;; zwraca pierwsza znaleziona pozycje znaku w stringu lub nil
;; funkcja do porownania: #'char= (case-sensitive) lub #'char-equal (case-insensitive)
;; znak <- "a", "b" lub podobne
(defun my/pos-of-char (znak tekst &optional (fun-do-por #'char=) (poczatek 0))
  (position-if
   (lambda (x) (and (characterp x)
		    (funcall fun-do-por (my/str->char znak) x)))
   tekst :start poczatek))

;; zwraca wszystkie pozycje danego znaku w stringu (indeksowanie od 0) lub nil jesli brak znaku
;; funkcja do porownania: #'char= (case-sensitive) lub #'char-equal (case-insensitive)
;; znak <- "a", "b" lub podobne
(defun my/all-pos-of-char (znak tekst &optional (fun-do-por #'char=) (poczatek 0) (accum nil))
  (let ((zwrocona-pozycja
	 (my/pos-of-char znak tekst fun-do-por poczatek)))
    (cond ((null zwrocona-pozycja)
	   (reverse accum)) ; bo cons-owanie zwraca liste w odwrotnej kolejnosci
	  (t (my/all-pos-of-char znak tekst fun-do-por
				 (1+ zwrocona-pozycja)
				 (cons zwrocona-pozycja accum))))))


;; przyjmuje liste liczb
;; zwraca liste liczb ktore nalezy usunac aby odstep miedzy liczbami wynosil co najmniej x
;; do usuwania sugeruje liczby z lewej ktore sa za blisko tych z prawej
;; np. (my/co-usun-aby-byl-rozstep '(1 2 3 14 17) 3) -> (1 2 14)
;; uwaga do pozniejszej funkcji kapitalizujacej pierwsza litere kazdego slowa
;; (wewnetrznie bedzie ona korzystac z tej funkcji):
;; z powodu dzialania tej funkcji
;; nie wskaze do usuniecia jesli 2-3 literowe slowo jest na koncu stringu
;; bedzie kapitalizowalo 1 litere  2-3 literowego slowana koncu zdania/stringu
;; u nas w pliku *.bib to (krotkie ostatnie slowo) i tak nie powinno miec miejsca
(defun my/co-usun-aby-byl-rozstep (l-liczb &optional (rozstep 4) (accum nil))
  (let ((l1 (first l-liczb))
	(l2 (second l-liczb)))
    (cond ((< (length l-liczb) 2) (reverse accum))
	  ((<= (- l2 l1) rozstep)
	   (my/co-usun-aby-byl-rozstep (cdr l-liczb) rozstep (cons l1 accum)))
	  (t (my/co-usun-aby-byl-rozstep (cdr l-liczb) rozstep accum)))))

;; przyjmuje liste liczb
;; zwraca liste liczb po usunieciu tych ktore sa za blisko (odstep) liczb z prawej strony
;; np. (my/usun-aby-byl-rozstep '(1 2 3 14 17) 3) -> (3 17)
;; uwagi jak w funkcji wyzej
(defun my/usun-aby-byl-rozstep (l-liczb &optional (rozstep 4))
  (let ((l-liczb2 l-liczb))
    (loop for i in
	 (my/co-usun-aby-byl-rozstep l-liczb rozstep)
       :do
	 (setf l-liczb2 (remove i l-liczb2)))
    l-liczb2))


;; upper-caseuje string w danej pozycji (0 indexed)
;; zwraca oryginalny string
;; (bedzie uzywany przez my/upcase-all-words ktore najpierw utworzy kopie stringu)
(defun my/char->upper (pozycja tekst)
  (setf (aref tekst pozycja)
	(char-upcase (aref tekst pozycja)))
  tekst)

;; zwraca kopie stringu (argument tekst)
;; z kazdym slowem (dluzszym niz x znakow) pisanym z duzej litery
;; wbudowane (string-capitalize *tekst*) kapitalizuje kazde slowo
;; bez wzgledu na dlugosc
(defun my/upcase-all-words (l-id-pierw-liter tekst)
  (let ((new-tekst (copy-seq tekst)))
    (loop for i in l-id-pierw-liter
       :do
	 (my/char->upper i new-tekst))
    new-tekst))

;; przyjmuje tekst pisany jak w zdaniu (np. "Journal of molecular sciences")
;; uppercaseuje 1 litere wszystkich slow (litera obok spacji) powyzej (>=) zadanej dlugosci
;; nie uppercaseuje 1 litery zdania i nieczuly na krotkie slowo na koncu zdania
;; patrz uwaga do (my/co-usun-aby-byl-rozstep)
;; od-dlug-slowa - od jakiej dlugosci slowa ma kapitalizowac (>=)
;; start-string i end-string wskazuja linijke w ktorej ma byc dokonana zmiana
;; zaczyna sie ona od indeksu start-string (inclusive) a konczy na end-string (exclusive)
(defun my/up-every-first-letter-journal (tekst &optional (od-dlug-slowa 4)
						 (znak " ") (start-string "journal=")
						 (end-string "year="))
  (let* ((start-index (search start-string tekst))
	 (end-index (search end-string tekst))
	 (linijka-tekstu (subseq tekst start-index end-index))
	 (ind-spacji-w-linijce (my/all-pos-of-char znak linijka-tekstu))
	 (ind-spacji-z-rozstepem (my/usun-aby-byl-rozstep
				  ind-spacji-w-linijce od-dlug-slowa))
	 (ind-pierw-liter-z-rozstepem-w-linijce
	  (mapcar #'(lambda (x)
		      (1+ x)) ind-spacji-z-rozstepem))
	 (ind-pierw-liter-z-rozst-w-tekscie
	  (mapcar #'(lambda (x)
		      (+ x start-index)) ind-pierw-liter-z-rozstepem-w-linijce)))
    (my/upcase-all-words ind-pierw-liter-z-rozst-w-tekscie tekst)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ;          ZMIENNE GLOBALNE         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *manuskrypt-n-pliku* "manuskrypt.md") ; nazwa pliku manuskryptu na dysku
(defparameter *manuskrypt* nil) ; tekst manuskryptu
(defparameter *pmid_wydobyte* nil) ; lista wydobytych pmid
(defparameter *pmid_unique* nil) ; lista unikalnych pmid (stringi cyfr)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tu zmiana, PubMed zmienil interfejs jakis 2-3 mies temu (dzis 20-07-2020) ;
; teraz za: https://api.ncbi.nlm.nih.gov/lit/ctxp nie dziala, bo, np.
; https://api.ncbi.nlm.nih.gov/lit/ctxp/v1/pubmed/?format=medline&id=23209333
; zwraca nieodpowiedni format (Medline, ale nie xml)
; dzialajace api znaleziono za:
; http://www.genomearchitecture.com/2014/06/how-to-convert-pubmed-references-to-bibtex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defparameter *url_pocz* "https://www.ncbi.nlm.nih.gov/pubmed/") ; do sciagania z pubmed
;; (defparameter *url_kon* "?report=xml&format=text") ; do sciagania z pubmed
(defparameter *url_pocz*
  "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=")
(defparameter *url_kon* "&retmode=xml") ; do sciagania z pubmed

(defparameter *str_www* nil) ; tu kazdorazowo
;; zapisana bedzie sciagnieta z neta (PubMed) stronka/cytowanie
(defparameter *plik_bibtex* nil) ; tu beda po kolei wczytywane pliki bibtex
(defparameter *manuskrypt_md* nil) ; manuskrypt z cytowaniami do uzycia przez Pandoc
(defparameter *cytowania.bib* "cytowania.bib") ; plik *.bib z wszystkimi cytowaniami
(defparameter *plik_csl* "apa-6th-edition.csl") ; plik *.csl ze stronki zotero (https://www.zotero.org/styles)
;; ze stylem bibliografii dla danego journala
(defparameter *komenda-pandoc* nil) ; komenda Pandoc do sformatowania bibliografii
(defparameter *tekst-skrypt-med-do-bib-sh* nil) ; tresc pliku ./skrypt_med_do_bib.sh
(defparameter *skrocona-czy-pelna-nazwa-journala* 'pelna) ; do wstawienia do pliku
;; ./.skrypt_med_do_bib.sh (w cytowaniach wstawionych przez pandoc do manuskryptu
;; pojawi sie odpowiednio skrocona lub pelna nazwa journala)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                           ;          AKCJA         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; wczytanie manuskryptu
(setf *manuskrypt* ; wczytuje jako 1 dlugi string
      (my/string<-file *manuskrypt-n-pliku*))

(setf *manuskrypt* ; zaminana "," miedzy numerami pmid na ";"
      (cl-ppcre:regex-replace-all
       "((PMID|pmid):*? *?[0-9]{4,9}),"
       *manuskrypt*
       "\\1;"))

;; wydobycie pmid
(setf *pmid_wydobyte*
      (cl-ppcre:all-matches-as-strings "(PMID|pmid):*? *?[0-9]{4,9}"
				       *manuskrypt*))

(setf *pmid_wydobyte*
      (apply #'append  ; zwraca liste stringow (same cyfry pmid)
	     (loop for i in *pmid_wydobyte*
		collect
		  (cl-ppcre:all-matches-as-strings
		   "[0-9]{4,9}" i))))

(setf *pmid_unique* ; tylko unikalne numery pmid (same cyfry)
      (remove-duplicates *pmid_wydobyte* :test #'string-equal))


;; tworzenie kalalogu na pliki sciagniete z neta
(uiop:run-program "mkdir pliki_medline"
		  :input nil :output *standard-output*)

(loop for pmid in *pmid_unique* and nr-id from 0

   :do
     (format t "~%Processing [~a]:~2,6@TPMID: ~a" nr-id pmid)
   ;; informacja dla usera (np. jakby sie cos zacielo)

     (setq *str_www* ; sciagnij stronke/cytowanie
	   (drakma:http-request
	    (concatenate 'string *url_pocz* pmid *url_kon*)))

   ;; zmien "&lt;" i "&gt;" (w sciagnietych danych dla danego pmid) na "<" i ">"
     (loop for co-zmien in '("&lt;" "&gt;") and
	na-co-zmien in '("<" ">")
	do
	  (setf *str_www*
		(cl-ppcre:regex-replace-all
		 co-zmien *str_www* na-co-zmien)))

   ;; zapisz plik na dysk
     (my/str->file *str_www* (concatenate 'string "./pliki_medline/"
					  "pmid_" pmid "_medline"))

     (sleep 3) ; czekaj 3 sek

   :finally ; na koniec wyswietl komunikat
     (format t "~%Zakonczono pobieranie.~%Zgrano ~a plikow o indeksach: 0-~a"
	     (1+ nr-id) nr-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;     sprawdzic wielkosc plikow (powinno byc >= X KB)     ;
    ; jesli jest blad serwera to moga byc pliki puste lub z info o bledzie ;
 ; trzeba wtedy odczekac jakis czas (pare godzin, dzien) i sprobowac ponownie ;
  ; sprawdzic czy pliki *_medline nie zawieraja dziwnego znaku na pocz liniji
           ; przed "@article" (widac to w Vimie) jesli jest to usunac
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; zamiana linijki w pliku ".skrypt_med_bo_bib.sh" w zaleznosci czy jest wymagana
;; skrotowa czy pelna nazwa journala,
;; a potem zapisanie odpowiednio sformatowanego pliku na dysk
(setf *tekst-skrypt-med-do-bib-sh*
      (my/string<-file "./.skrypt_med_do_bib.sh"))

(cond ((eql *skrocona-czy-pelna-nazwa-journala* 'skrocona) ; jesli skrocona to wstaw odp linijke kodu
       (setf *tekst-skrypt-med-do-bib-sh*
	     (cl-ppcre:regex-replace-all
	      "    #do_wstawienia_przez_cl_pelny_or_abbreviated_journal"
	      *tekst-skrypt-med-do-bib-sh*
	      "    xml2bib -o unicode -at ./\$f > ../pliki_bib/\$f.bib")))
      ((eql *skrocona-czy-pelna-nazwa-journala* 'pelna) ; jesli pelna to wstaw odp linijke kodu
       (setf *tekst-skrypt-med-do-bib-sh*
	     (cl-ppcre:regex-replace-all
	      "    #do_wstawienia_przez_cl_pelny_or_abbreviated_journal"
	      *tekst-skrypt-med-do-bib-sh*
	      "    xml2bib -o unicode ./\$f > ../pliki_bib/\$f.bib")))
      ;; w przeciwnym wypadku poinformuj uzytkownika
      (t (format t
		 "~&~%~%-------- ~&WYBIERZ-JEDNO-Z-DWOCH-PELNA-LUB-SKROCONA~&--------~%~%")))


;; zapisz poprawny (pod katem skrocona/pelna nazwa journala) plik .skrypt_med_do_bib.sh
(my/str->file *tekst-skrypt-med-do-bib-sh* "./.skrypt_med_do_bib.sh")

;; uruchom skrypt bash-owy konwertujacy pliki medline do bib-ow
(uiop:run-program "bash .skrypt_med_do_bib.sh"
		  :input nil :output *standard-output*)

;; zamienia identyfikator/znaczniki pmid w manuskrypcie i plikach bib
(loop for pmid in *pmid_unique*

   :do
     (setf *plik_bibtex* ; wczytaj plik bibtex z dysku (jako 1 dlugi string)
	   (my/string<-file
	    (concatenate 'string "./pliki_bib/pmid_" pmid ".bib")))

     (setf *plik_bibtex* ; podmienia identyfikator
	   (cl-ppcre:regex-replace
	    "@Article{(.*),"
	    *plik_bibtex*
	    (concatenate 'string "@article{pmid" pmid ",")))

   ;; bashowe funkcje z pliku ".skrypt_med_do_bib.sh" (tj. xml2bib)
   ;; zwracaja nazwe journala jak w zdaniu (np. "Tomek lubi ole, ale ola go nie lubi")
   ;; journale wymagaja tzw. title-case, funkcja (my/up-every-first-letter-journal) to zapewnia
     (when (eql *skrocona-czy-pelna-nazwa-journala* 'pelna)
       (setf *plik_bibtex* (my/up-every-first-letter-journal *plik_bibtex*)))

   ;; zapisuje poprawiony plik bibtex
     (my/str->file *plik_bibtex*
		   (concatenate 'string "./pliki_bib/pmid_" pmid ".bib"))

   ;; podmienia w manuskrypcie wszystkie wystapienia PMID: XXX na @pmidXXX dla pandoc-a
     (setf *manuskrypt*
	   (cl-ppcre:regex-replace-all
	    (concatenate 'string "(PMID|pmid):*? *?" pmid)
	    *manuskrypt*
	    (concatenate 'string "@pmid" pmid)))

   :finally
   ;; zapisuje poprawny plik manuskryptu
     (my/str->file *manuskrypt* "manuskrypt_z_ref_lisp.md")
   ;; wyswietla komunikat
     (format t "~%Wstawianie cytowan dla programu pandoc-citeproc zakonczone"))


(uiop:run-program "bash .skrypt_biby_do_cytowania.sh"
		  :input nil :output *standard-output*)


;; ustawianie danych dla komendy pandoc
(setf *manuskrypt_md* "manuskrypt_z_ref_lisp.md")

(setf *komenda-pandoc* (concatenate
			'string
			"pandoc " *manuskrypt_md*
			" --bibliography=" *cytowania.bib*
			" --csl=" *plik_csl*
			" --smart --normalize -s -o"
			" wynik.odt"))

;; uruchamiamy pandoc-citeproc
;; manuskrypt musi byc bez Tab-ow na poczatku linii bo nie powstawia cytowan
(uiop:run-program *komenda-pandoc*
		  :input nil :output *standard-output*)
