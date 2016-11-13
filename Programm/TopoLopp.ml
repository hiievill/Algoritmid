(* moodul TopoLopp teostab sammsammulist l�bim�ngu topoloogilise j�rjestuse leidmiseks l�ppj�rjestuse abil *) 

open Struktuurid;;
open AlgoBaas;;

let sygavutiTipud = ref([]);;

(* funktsioon, mis l�bib graafi s�gavuti l�ppj�rjestuses seni, kuni k�ik tipud on k�lastatud, alustades vajadused nullise *)
(* sisendastmega tippudest uuesti. Vajalik Kosaraju ja TopoLopp jaoks. Tagastatakse tekkinud j�rjestus *)
let l2biSygavuti(algtipp, tipud, servad) =
	let sygavutiTipud = ref([]) in
	let esimeneTipp = ref(algtipp) in
	!esimeneTipp.tv := Vaatlemata;	(* �he tipuga juhuks, muidu ei t��ta (sest alguses m�rkisime algtipu juba valituks) *)
	while List.exists (fun t -> !(t.tv) = Vaatlemata) tipud (* kuniks leidub veel vaatlemata tippe *)
		do
			i := Algus;	(* l�bime graafi s�gavuti l�ppj�rjestuses alates tipust esimeneTipp *)
			algoL2bi := false;
			SygavutiLopp.toodeldudTipud := [];
			(*print_endline(!esimeneTipp.nimi);*)
			while !algoL2bi = false
				do
					SygavutiLopp.samm(!esimeneTipp, tipud, servad)									(* l�bime s�gavuti *)
				done;
  		sygavutiTipud := !sygavutiTipud @ !(SygavutiLopp.toodeldudTipud); 	(* lisame j�rjestusse *)
			(*print_endline(string_of_tipud(!sygavutiTipud));*)
  		if List.exists (fun t -> !(t.tv) = Vaatlemata) tipud 								(* kui leidub veel vaatlemata tippe *)
				then esimeneTipp := TopoKahn.valiTipp(tipud)
  	done;
		algoL2bi := false;
		nk2 := "";
		nk3 := "";
		!sygavutiTipud;;

(* algoritmi algus, mille k�igus m��rame igale tipule s�gavuti l�bimise jaoks sisendastme *)
let algus(tipud, servad) =
	List.iter (fun t -> TopoKahn.uuendaSisendastet (TopoKahn.leiaSisendaste(t, servad)) t) tipud;
	tekst := "Algoritm topoloogilise j�rjestuse leidmiseks l�ppj�rjestuse kaudu alustab valitud tipust. ";
	i := SygavutiL2bimine;;

(* graafi s�gavuti l�ppj�rjestuses l�bimine ja tekkinud j�rjestuse saamine *)
let sygavutiL2bimine(algtipp, tipud, servad) =	
	sygavutiTipud := l2biSygavuti(algtipp, tipud, servad);
	nk1 := "L�ppj�rjestus: " ^ string_of_tipud(!sygavutiTipud);
	nk2 := "";
	tekst := "L�bime graafi s�gavuti l�ppj�rjestuses.";
	i := Tagurpidi;;

(* tekkinud l�ppj�rjestuse tagurpidi keeramine *)
let tagurpidi() =
	tekst := "Topoloogilise j�rjestuse leidmiseks keerame tekkinud j�rjestuse tagurpidi.";
	nk1 := "L�pp�rjestus: " ^ string_of_tipud(!sygavutiTipud);
	nk2 := "Tagurpidi l�ppj�rjestus: " ^ string_of_tipud(List.rev !sygavutiTipud);
	i := Lopp;;

(* algoritmi l�pp *)
let lopp() =
	tekst := "Algoritm l�petab, olles leidnud graafi topoloogilise j�rjestuse.";
	nk1 := "Topoloogiline j�rjestus: " ^ string_of_tipud(List.rev !sygavutiTipud);
	nk2 := "";
	AlgoBaas.lopp();;

(* algoritmi samm *)
let samm(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus(tipud, servad);
		| SygavutiL2bimine -> sygavutiL2bimine(algtipp, tipud, servad);
		| Tagurpidi -> tagurpidi();
		| Lopp -> lopp();
		| _ -> ();;

