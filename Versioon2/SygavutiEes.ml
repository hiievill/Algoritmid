open Struktuurid;;
open Laiuti;;
open AlgoBaas;;

let algus() =
	(*AlgoBaas.graafiKontroll(...);*)
	tekst := "S�gavuti eesj�rjestuses l�bimise algoritm alustab";
	i := EsimeneTipp;;
	
let servaLisamine(tipud, servad) =	(* sama mis laiuti, aga 2 erinevat rida: tekst ja js @ !j2rgmisedServad *)
	let lisatavServ = List.find (fun s -> !(s.sv) = Valitud) servad in
	let lisatavTipp = leiaLisatavTipp(lisatavServ) in				(* lisatav tipp *)
	toodeldudTipud := (!toodeldudTipud) @ [lisatavTipp];	(* lisame selle k�lastatud tippude hulka *)
	let js = leiaJ2rgServad(lisatavTipp, servad) in 				(* leiame need servad, kuhu valitud tipust viib *)
	j2rgmisedServad := js @ (!j2rgmisedServad); 						(* lisame need j�rgmiste servade j�rjekorra algusesse *)
	j2rgmisedServad := eemalda(!j2rgmisedServad); 					(* eemaldame j�rgmiste servade j�rjekorrast need servad, 
																														mis �hendavad k�lastatud tippe *)
	lisaServ(lisatavServ);																	(* m�rgime serva ja tema tipud vaadelduks *)
	tekst := "M�rgime selle tipu t��delduks ja lisame j�rjekorra algusesse need tipud, kuhu siit p��seb.";
	tekst := !tekst ^ "\n" ^ string_of_toodeldudTipud(!toodeldudTipud);
	tekst := !tekst ^ "\n" ^ string_of_j2rgmisedTipud(!j2rgmisedServad);
	if List.length !j2rgmisedServad = 0											(* kui j�rgmiste servade j�rjekord on t�hi, l�hme l�pule *)
		then i := Lopp
	else i := ServaValik;;																	(* kui j�rgmisi servi leidub, l�hme uut serva valima *)
	
let lopp() =
	tekst := "Algoritm l�petab, olles leidnud s�gavuti eesj�rjestuses otsingu otsingupuu.";
	tekst := !tekst ^ "\n" ^ "Tippude t��tlemise j�rjekord: " ^ string_of_tipud(!toodeldudTipud);
	AlgoBaas.lopp();;

let sygavutiEes(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus();
		| EsimeneTipp -> Laiuti.esimeneTipp(algtipp, servad);
		| ServaValik -> Laiuti.servaValik(servad);
		| ServaLisamine -> servaLisamine(tipud, servad);
		| Lopp -> lopp();
		| _ -> ();;