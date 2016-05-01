open Struktuurid;;
open AlgoBaas;;

(* TODO: TopoKahn.string_of_topo ära kasutada *)
(* TODO: string_of_... funktsioonid *)

let sygavutiTipud = ref([]);;

(* funktsioon, mis läbib graafi sügavuti lõppjärjestuses seni, kuni kõik tipud on külastatud, alustades vajadused nullise *)
(* sisendastmega tippudest uuesti. Vajalik Kosaraju ja TopoLopp jaoks. Tagastatakse tekkinud järjestus *)
let l2biSygavuti(algtipp, tipud, servad) =
	let sygavutiTipud = ref([]) in
	let esimeneTipp = ref(algtipp) in
	!esimeneTipp.tv := Vaatlemata;	(* ühe tipuga juhuks, muidu ei tööta (sest alguses märkisime algtipu juba valituks) *)
	while List.exists (fun t -> !(t.tv) = Vaatlemata) tipud (* kuniks leidub veel vaatlemata tippe *)
		do
			i := Algus;
			(* läbime graafi sügavuti lõppjärjestuses alates tipust esimeneTipp *)
			algoL2bi := false;
			SygavutiLopp.toodeldudTipud := [];
			while !algoL2bi = false
				do
					SygavutiLopp.sygavutiLopp(!esimeneTipp, tipud, servad)		(*läbime sügavuti*)
				done;
  		sygavutiTipud := !sygavutiTipud @ !(SygavutiLopp.toodeldudTipud); (*lisame järjestusse*)
  		if List.exists (fun t -> !(t.tv) = Vaatlemata) tipud (* kui leidub veel vaatlemata tippe *)
  			then esimeneTipp := TopoKahn.valiTipp(tipud) (* määrame uue algtipu (sisendastmega 0) *)
  	done;
		algoL2bi := false;
		!sygavutiTipud;;

let algus(tipud, servad) =
	List.iter (fun t -> TopoKahn.uuendaSisendastet (TopoKahn.leiaSisendaste(t, servad)) t) tipud; (* sügavuti läbimise jaoks *)
	tekst := "Algoritm opoloogilise järjestuse leidmiseks lõppjärjestuse kaudu alustab. ";
	tekst := !tekst ^ "Läbime graafi sügavuti lõppjärjestusega.";
	i := ServaVaatlus;;

(* TODO: rename. Algosamme oleks juurde vaja. *)
(* TODO: või peaks kõik sügavuti läbimise sammud ka kujutama? Kas või ainult tippude lisamist? *)
let servaVaatlus(algtipp, tipud, servad) =	
	sygavutiTipud := l2biSygavuti(algtipp, tipud, servad);			(* läbime graafi sügavuti lõppjärjestuses *)
	nk1 := "Lõppjärjestus: " ^ string_of_tipud(!sygavutiTipud);
	nk2 := "";
	algoL2bi := false;
	i := Tagurpidi;;

let tagurpidi() =
	tekst := "Topoloogilise järjestuse leidmiseks keerame tekkinud järjestuse tagurpidi.";
	nk1 := "Lõppärjestus: " ^ string_of_tipud(!sygavutiTipud);
	nk2 := "Tagurpidi lõppjärjestus: " ^ string_of_tipud(List.rev !sygavutiTipud);
	i := Lopp;;

let lopp() =
	tekst := "Algoritm lõpetab, olles leidnud topoloogilise järjestuse.";
	nk1 := "Topoloogiline järjestus: " ^ string_of_tipud(List.rev !sygavutiTipud);
	nk2 := "";
	AlgoBaas.lopp();;

let topoLopp(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus(tipud, servad);
		| ServaVaatlus -> servaVaatlus(algtipp, tipud, servad);
		| Tagurpidi -> tagurpidi();
		| Lopp -> lopp();
		| _ -> ();;

