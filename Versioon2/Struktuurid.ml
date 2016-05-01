
type algoritm = Laiuti
	| SygavutiEes
	| SygavutiLopp
	| Prim
	| Kruskal
	| Dijkstra
	| FloydWarshall
	| TopoLopp
	| TopoKahn
	| Eeldusgraaf
	| Kosaraju;;

let string_of_algo(algo) =
	match algo with
		| Laiuti -> "Laiuti"
		| SygavutiEes -> "SygavutiEes"
  	| SygavutiLopp -> "SygavutiLopp"
  	| Prim -> "Prim"
  	| Kruskal -> "Kruskal"
  	| Dijkstra -> "Dijkstra"
  	| FloydWarshall -> "FloydWarshall"
  	| TopoLopp -> "TopoLopp"
  	| TopoKahn -> "TopoKahn"
  	| Eeldusgraaf -> "Eeldusgraaf"
  	| Kosaraju -> "Kosaraju";;

type algoSamm = Algus
	| EsimeneTipp
	| ServaVaatlus
	| ServaValik
	| ServaLisamine
	| Lopp
	| L2bi
	| Sygavuti
	| PooratudGraaf
	| Topo
	| Vahe1
	| Vahe2
	| Vahe3
	| TipuValikVL
	| TipuLisamineVL
	| TipuValikHA
	| TipuLisamineHA
	| SobimatuServ
	| Tagurpidi
	| EsimeneKriitiline
	| Kriitiline
	| EsimeseTipuLisamine
	| TeisedTipud
	| Fikseerimine
	| LahtriVaatlus
	| LahtriMuutmine;;

(* tähistab tipu/serva vaadeldavust, oluline kuvamisel värvi valikuks *)
type vaadeldavus = Vaatlemata
	| Vaadeldav
	| Valitud
	| Vaadeldud
	| Sobimatu;;

let string_of_vaadeldavus(v) =
	match v with
		| Vaatlemata -> "Vaatlemata"
		| Vaadeldav -> "Vaadeldav"
		| Valitud -> "Valitud"
		| Vaadeldud -> "Vaadeldud"
		| Sobimatu -> "Sobimatu";;
	
type tipp = {
	nimi : string;
	x : int ref;
	y : int ref;
	tv : vaadeldavus ref;
	hind: int option
}

let string_of_tipp(tipp) =
	"Tipp {" ^ tipp.nimi ^ ", " ^ string_of_int(!(tipp.x)) ^ ", " ^ string_of_int(!(tipp.y)) ^ "}";;

(* väljad x, y ja r on ringjoone võrrandite jaoks *)
(*type kaal = {
	suurus : int;
	x : int ref;
	y : int ref;
	r : int ref;
}*)

type serv = {
	tipp1 : tipp ref;
	tipp2 : tipp ref;
	kaal : int option;
	nool : bool; (*kui true, siis alati esimesest teise*)
	sv : vaadeldavus ref;
}

let string_of_serv(serv) =
	"Serv {" ^ string_of_tipp(!(serv.tipp1)) ^ ", " ^ string_of_tipp(!(serv.tipp2)) ^ ", kaal: " ^ 
		(match serv.kaal with | None -> "puudub" | Some k -> string_of_int(k)) ^ ", nool: " ^ string_of_bool(serv.nool) ^ "}";;

type graaf = {
	tipud : tipp list;
	servad : serv list;
}

