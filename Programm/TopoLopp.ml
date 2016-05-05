open Struktuurid;;
open AlgoBaas;;

(* TODO: TopoKahn.string_of_topo �ra kasutada *)
(* TODO: string_of_... funktsioonid *)

let sygavutiTipud = ref([]);;

(* funktsioon, mis l�bib graafi s�gavuti l�ppj�rjestuses seni, kuni k�ik tipud on k�lastatud, alustades vajadused nullise *)
(* sisendastmega tippudest uuesti. Vajalik Kosaraju ja TopoLopp jaoks. Tagastatakse tekkinud j�rjestus *)
let l2biSygavuti(algtipp, tipud, servad) =
	let sygavutiTipud = ref([]) in
	let esimeneTipp = ref(algtipp) in
	!esimeneTipp.tv := Vaatlemata;	(* �he tipuga juhuks, muidu ei t��ta (sest alguses m�rkisime algtipu juba valituks) *)
	while List.exists (fun t -> !(t.tv) = Vaatlemata) tipud (* kuniks leidub veel vaatlemata tippe *)
		do
			i := Algus;
			(* l�bime graafi s�gavuti l�ppj�rjestuses alates tipust esimeneTipp *)
			algoL2bi := false;
			SygavutiLopp.toodeldudTipud := [];
			while !algoL2bi = false
				do
					SygavutiLopp.sygavutiLopp(!esimeneTipp, tipud, servad)		(*l�bime s�gavuti*)
				done;
  		sygavutiTipud := !sygavutiTipud @ !(SygavutiLopp.toodeldudTipud); (*lisame j�rjestusse*)
  		if List.exists (fun t -> !(t.tv) = Vaatlemata) tipud (* kui leidub veel vaatlemata tippe *)
  			then esimeneTipp := TopoKahn.valiTipp(tipud) (* m��rame uue algtipu (sisendastmega 0) *)
  	done;
		algoL2bi := false;
		!sygavutiTipud;;

let algus(tipud, servad) =
	List.iter (fun t -> TopoKahn.uuendaSisendastet (TopoKahn.leiaSisendaste(t, servad)) t) tipud; (* s�gavuti l�bimise jaoks *)
	tekst := "Algoritm opoloogilise j�rjestuse leidmiseks l�ppj�rjestuse kaudu alustab. ";
	tekst := !tekst ^ "L�bime graafi s�gavuti l�ppj�rjestusega.";
	i := ServaVaatlus;;

(* TODO: rename. Algosamme oleks juurde vaja. *)
(* TODO: v�i peaks k�ik s�gavuti l�bimise sammud ka kujutama? Kas v�i ainult tippude lisamist? *)
let servaVaatlus(algtipp, tipud, servad) =	
	sygavutiTipud := l2biSygavuti(algtipp, tipud, servad);			(* l�bime graafi s�gavuti l�ppj�rjestuses *)
	nk1 := "L�ppj�rjestus: " ^ string_of_tipud(!sygavutiTipud);
	nk2 := "";
	algoL2bi := false;
	i := Tagurpidi;;

let tagurpidi() =
	tekst := "Topoloogilise j�rjestuse leidmiseks keerame tekkinud j�rjestuse tagurpidi.";
	nk1 := "L�pp�rjestus: " ^ string_of_tipud(!sygavutiTipud);
	nk2 := "Tagurpidi l�ppj�rjestus: " ^ string_of_tipud(List.rev !sygavutiTipud);
	i := Lopp;;

let lopp() =
	tekst := "Algoritm l�petab, olles leidnud topoloogilise j�rjestuse.";
	nk1 := "Topoloogiline j�rjestus: " ^ string_of_tipud(List.rev !sygavutiTipud);
	nk2 := "";
	AlgoBaas.lopp();;

let topoLopp(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus(tipud, servad);
		| ServaVaatlus -> servaVaatlus(algtipp, tipud, servad);
		| Tagurpidi -> tagurpidi();
		| Lopp -> lopp();
		| _ -> ();;

