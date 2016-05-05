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

1. Sisendandmeid (algoritm, graafi tipud ja kaared, algtipp) saab muuta mooduli Programm funktsioonis main või andes programmile parameetrina ette sisendfaili, kus on kirjeldatud sisendgraaf. Failis peavad olema graafi andmed kirjeldatud järgmisel kujul:
	* 1. real: Algoritm: [Laiuti|SygavutiEes|SygavutiLopp|Prim|Kruskal|Dijkstra|FloydWarshall|TopoLopp|TopoKahn|Eeldusgraaf|Kosaraju]
	* 2. real: Tippe: {tippude arv}, hindadega: [true|false]
	* 3. real: Servi: {servade arv}, kaaludega: [true|false], suundadega: [true|false]
	* järgneval m-l real, kus m on tippude arv: {tipu nimi} <, [{tipu x-koordinaat}|-], [{tipu y-koordinaat}|-] <, [{tipu hind}|-]> >
	* järgneval n-l real, kus n on servade arv: {1. tipu nimi}, {2. tipu nimi} <, [{tipu kaal}|-]>
	* tühikuid võib vahele jätta, kuid reavahetusi mitte. Komad, koolonid, tippude arv ja servade arv peavad kindlasti õiged olema.
	* kui valikulised väljad (koordinaadid, hind, kaal) tühjaks jätta või sinna "-" kirjutada, valitakse suvaliselt sobiv arv
	* algtippu nõudvate algoritmide puhul võetakse algtipuks esimene failis kirjeldatud tipp
	* näide on failis Andmed.txt
2. Minna kataloogi, kus asub Makefile
3. Käsurealt 'make'
4. Käsurealt Windowsis 'tulem.exe <sisendfail>', Linuxis './tulem.exe <sisendfail>' - käivitab programmi
	* klahv 'n' võimaldab slaidhaaval läbimängu teha
	* klahv 's' teostab automaatselt terve läbimängu alates käesolevale järgnevast sammust, tekitab slaidid ning salvestab PDFina.
	* klahv 'q' sulgeb programmi
	* klahv 'f' sulgeb programmi veateatega (ajutiselt - q enne ei toiminud)
	* hiirega on võimalik tippe lohistada ja servade kumerust muuta
5. Käsurealt 'make clean' - kustutab kõik programmi töö käigus tekkinud failid, kui programm ise seda ei teinud. Kustutatakse ka tekkinud PDF(id).


