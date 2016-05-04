# Graafialgoritmide visualiseerimine
Hiie Villi bakalaureusetöö

###Käivitamiseks vajalikud:

1. OCaml:
	* Windows: https://www.typerex.org/ocpwin.html ja lisada tee bin kaustani environment variables'isse, kui seda automaatselt ei lisatud
	* Linux: sudo apt-get install ocaml
	
2. GNU make:
	* Windows: MinGW või Cygwin
	* Linux: juba olemas
	
Ainult läbimängu tegemiseks piisab eelnevatest, slaidide tekitamiseks on lisaks vaja ka järgnevaid:

3. MetaPost:
	* Windows: Installida http://miktex.org/download ja lisada tee (vaikimisi C:\Program Files\MiKTeX 2.9\miktex\bin\x64) environment variables'isse, kui seda automaatselt ei lisatud
	* Linux:
		1) sudo apt-get install texlive-binaries
		2) mpost: sudo apt-get install texlive-metapost
		3) epstopdf: sudo apt-get install texlive-font-utils
		
Järgmine pole enamasti vajalik, ainult tagavaravariant juhuks, kui epstopdfi abil slaidide tekitamine ei õnnestu. Sel juhul ei koondata kõiki EPS faile üheks kokku ega tee sellest PDFi, vaid tehakse igast PostScripti failist eraldi PDF ja liidetakse need kokku.
	
4. PDF toolkit:
	* Windows: https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/
	* Linux: sudo apt-get install pdftk

### Programmi kasutamine

1. Sisendandmeid (algoritm, graafi tipud ja kaared, algtipp) saab muuta mooduli Programm funktsioonis main
2. Minna kataloogi, kus asub Makefile
3. Käsurealt 'make'
4. Käsurealt Windowsis 'tulem.exe', Linuxis './tulem.exe' - käivitab programmi
	* klahv 'n' võimaldab slaidide haaval läbimängu teha
	* klahv 's' teostab automaatselt terve läbimängu alates käesolevale järgnevast sammust, tekitab slaidid ning salvestab PDFina.
	* klahv 'q' sulgeb programmi
	* klahv 'f' sulgeb programmi veateatega (ajutiselt - q enne ei toiminud)
5. Käsurealt 'make clean' - kustutab kõik programmi töö käigus tekkinud failid (NB! Kaasa arvatud tekkinud PDFi)


