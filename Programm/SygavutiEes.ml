(* moodul SygavutiEes teostab sammsammulist graafi sügavuti eesjärjestuses läbimist *) 

open Struktuurid;;
open Laiuti;;
open AlgoBaas;;

(* algoritmi algus *)
let algus() =
	tekst := "Sügavuti eesjärjestuses läbimise algoritm alustab valitud algtipust.";
	i := EsimeneTipp;;

(* algoritmi lõpp *)
let lopp() =
	tekst := "Magasin on tühi. Sügavuti eesjärjestuses läbimise algoritm lõpetab, olles leidnud sügavuti eesjärjestuses läbimise puu";
	nk1 := string_of_toodeldudTipud(!toodeldudTipud);
	(*nk2 := string_of_j2rgmisedTipud(!j2rgmisedServad);*)
	nk2 := string_of_j2rgmisedServad(!j2rgmisedServad);
	AlgoBaas.lopp();;

(* algoritmi samm *)
let samm(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus();
		| EsimeneTipp -> Laiuti.esimeneTipp(algtipp, servad);
		| ServaValik -> Laiuti.servaValik(servad);
		| ServaLisamine -> Laiuti.servaLisamine(tipud, servad);
		| Lopp -> lopp();
		| _ -> ();;