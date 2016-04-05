
let algoL2bi = ref(false);; (* v�i selle asemel algo l�pus ebavajalikud servad �ra kustutada? *)

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

let i = ref(Algus);; (* counter algoritmi sammude jaoks*)

let tekst = ref("");;	(* algoritmi sammudel kuvatav tekst *)

type vaadeldavus = Vaatlemata	(* t�histab tipu/serva vaadeldavust, oluline kuvamisel v�rvi valikuks *)
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

type noolesuund = Puudub
	| EsimesestTeise
	| TeisestEsimesse;;

type serv = {
	tipp1 : tipp ref;
	tipp2 : tipp ref;
	kaal : int option;
	nool : noolesuund;
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

