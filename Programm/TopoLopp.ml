(* moodul TopoLopp teostab sammsammulist läbimängu topoloogilise järjestuse leidmiseks lõppjärjestuse abil *) 

open Struktuurid;;
open AlgoBaas;;

let sygavutiTipud = ref([]);;

(* funktsioon, mis läbib graafi sügavuti lõppjärjestuses seni, kuni kõik tipud on külastatud, alustades vajadused nullise *)
(* sisendastmega tippudest uuesti. Vajalik Kosaraju ja TopoLopp jaoks. Tagastatakse tekkinud järjestus *)
let l2biSygavuti(algtipp, tipud, servad) =
	let sygavutiTipud = ref([]) in
	let esimeneTipp = ref(algtipp) in
	!esimeneTipp.tv := Vaatlemata;	(* ühe tipuga juhuks, muidu ei tööta (sest alguses märkisime algtipu juba valituks) *)
	while List.exists (fun t -> !(t.tv) = Vaatlemata) tipud (* kuniks leidub veel vaatlemata tippe *)
		do
			i := Algus;	(* läbime graafi sügavuti lõppjärjestuses alates tipust esimeneTipp *)
			algoL2bi := false;
			SygavutiLopp.toodeldudTipud := [];
			(*print_endline(!esimeneTipp.nimi);*)
			while !algoL2bi = false
				do
					SygavutiLopp.samm(!esimeneTipp, tipud, servad)									(* läbime sügavuti *)
				done;
  		sygavutiTipud := !sygavutiTipud @ !(SygavutiLopp.toodeldudTipud); 	(* lisame järjestusse *)
			(*print_endline(string_of_tipud(!sygavutiTipud));*)
  		if List.exists (fun t -> !(t.tv) = Vaatlemata) tipud 								(* kui leidub veel vaatlemata tippe *)
				then esimeneTipp := TopoKahn.valiTipp(tipud)
  	done;
		algoL2bi := false;
		nk2 := "";
		nk3 := "";
		!sygavutiTipud;;

(* algoritmi algus, mille käigus määrame igale tipule sügavuti läbimise jaoks sisendastme *)
let algus(tipud, servad) =
	List.iter (fun t -> TopoKahn.uuendaSisendastet (TopoKahn.leiaSisendaste(t, servad)) t) tipud;
	tekst := "Algoritm topoloogilise järjestuse leidmiseks lõppjärjestuse kaudu alustab valitud tipust. ";
	i := SygavutiL2bimine;;

(* graafi sügavuti lõppjärjestuses läbimine ja tekkinud järjestuse saamine *)
let sygavutiL2bimine(algtipp, tipud, servad) =	
	sygavutiTipud := l2biSygavuti(algtipp, tipud, servad);
	nk1 := "Lõppjärjestus: " ^ string_of_tipud(!sygavutiTipud);
	nk2 := "";
	tekst := "Läbime graafi sügavuti lõppjärjestuses.";
	i := Tagurpidi;;

(* tekkinud lõppjärjestuse tagurpidi keeramine *)
let tagurpidi() =
	tekst := "Topoloogilise järjestuse leidmiseks keerame tekkinud järjestuse tagurpidi.";
	nk1 := "Lõppärjestus: " ^ string_of_tipud(!sygavutiTipud);
	nk2 := "Tagurpidi lõppjärjestus: " ^ string_of_tipud(List.rev !sygavutiTipud);
	i := Lopp;;

(* algoritmi lõpp *)
let lopp() =
	tekst := "Algoritm lõpetab, olles leidnud graafi topoloogilise järjestuse.";
	nk1 := "Topoloogiline järjestus: " ^ string_of_tipud(List.rev !sygavutiTipud);
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

