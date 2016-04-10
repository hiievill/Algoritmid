
type algoritm = Laiuti
	| SygavutiEes
	| SygavutiLopp
	| Prim
	| Kruskal
	| Dijkstra
	| FloydWarshall
	| TopoLopp
	| TopoKahn
	| EeldusAnalyys
	| Kosaraju;;

type algoSamm = Algus
	| EsimeneTipp
	| ServaVaatlus
	| ServaValik
	| ServaLisamine
	| Lopp
	| L2bi;;

type vaadeldavus = Vaatlemata	(* tähistab tipu/serva vaadeldavust, oluline kuvamisel värvi valikuks *)
	| Vaadeldav
	| Valitud
	| Vaadeldud;;
	
type tipp = {
	nimi : string;
	x : int ref;
	y : int ref;
	tv : vaadeldavus ref;
}

let string_of_tipp(tipp) =
	"Tipp {" ^ tipp.nimi ^ ", " ^ string_of_int(!(tipp.x)) ^ ", " ^ string_of_int(!(tipp.y)) ^ "}";;

type serv = {
	tipp1 : tipp ref;
	tipp2 : tipp ref;
	kaal : int option;
	nool : bool; (*kui true, siis alati esimesest teise*)
	sv : vaadeldavus ref;
}

(*ei kasuta veel *)
type graaf = {
	tipud : tipp list;
	servad : serv list;
}
	
let string_of_vaadeldavus(v) =
	match v with
		| Vaatlemata -> "Vaatlemata"
		| Vaadeldav -> "Vaadeldav"
		| Valitud -> "Valitud"
		| Vaadeldud -> "Vaadeldud";;

let tippVaadeldud(tipp) =	(*tagastab true, kui vastav tipp on vaadeldud*)
	!(tipp.tv) = Vaadeldud;;

