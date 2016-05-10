
TULEM = tulem.exe

build:
	ocamlc graphics.cma str.cma Struktuurid.ml AlgoBaas.ml Laiuti.ml SygavutiEes.ml SygavutiLopp.ml Prim.ml Kruskal.ml Dijkstra.ml FloydWarshall.ml TopoKahn.ml TopoLopp.ml Eeldusgraaf.ml Kosaraju.ml Graafika.ml NtGraafid.ml MetaPost.ml Programm.ml -o $(TULEM)

clean:
	rm -f $(TULEM)
	rm -f *.cmi
	rm -f *.cmo
	rm -f *.log		% kõikide failide puhul mpostiga tekkinud temp.log fail
	rm -f *.pdf		% õnnestunud slaidide genereerimise korral tekkinud PDF failid
	rm -f *.mp*		% kõikide variantide puhul tekkinud temp.mp ning .mps failid
	rm -f temp.*	
	rm -f *.1		% variant 1 puhul tekkinud temp.1 fail
	rm -f *.tex		% variant 3 puhul tekkinud LaTeX failid
