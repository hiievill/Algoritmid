(* moodul SygavutiEes teostab sammsammulist graafi s�gavuti eesj�rjestuses l�bimist *) 

open Struktuurid;;
open Laiuti;;
open AlgoBaas;;

(* algoritmi algus *)
let algus() =
	tekst := "S�gavuti eesj�rjestuses l�bimise algoritm alustab valitud algtipust.";
	i := EsimeneTipp;;

(* algoritmi l�pp *)
let lopp() =
	tekst := "Magasin on t�hi. S�gavuti eesj�rjestuses l�bimise algoritm l�petab, olles leidnud s�gavuti eesj�rjestuses l�bimise puu";
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