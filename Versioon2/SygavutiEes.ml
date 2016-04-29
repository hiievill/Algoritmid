open Struktuurid;;
open Laiuti;;
open AlgoBaas;;

let algus() =
	tekst := "Sügavuti eesjärjestuses läbimise algoritm alustab.";
	i := EsimeneTipp;;
	
let servaLisamine(tipud, servad) =	(* sama mis laiuti, aga 2 erinevat rida: tekst ja js @ !j2rgmisedServad *)
	let lisatavServ = List.find (fun s -> !(s.sv) = Valitud) servad in
	let lisatavTipp = leiaLisatavTipp(lisatavServ) in				(* lisatav tipp *)
	toodeldudTipud := (!toodeldudTipud) @ [lisatavTipp];		(* lisame selle külastatud tippude hulka *)
	let js = leiaJ2rgServad(lisatavTipp, servad) in 				(* leiame need servad, kuhu valitud tipust viib *)
	j2rgmisedServad := js @ (!j2rgmisedServad); 						(* lisame need järgmiste servade järjekorra algusesse *)
	j2rgmisedServad := eemalda(!j2rgmisedServad); 					(* eemaldame järgmiste servade järjekorrast need servad, 
																														mis ühendavad külastatud tippe *)
	lisaServ(lisatavServ);																	(* märgime serva ja tema tipud vaadelduks *)
	tekst := "Märgime selle tipu töödelduks ja lisame magasini need töötlemata tipud, kuhu äsja töödeldud tipust serv viib.";
	nk1 := string_of_toodeldudTipud(!toodeldudTipud);
	nk2 := string_of_j2rgmisedTipud(!j2rgmisedServad);
	if List.length !j2rgmisedServad = 0											(* kui järgmiste servade järjekord on tühi, lähme lõpule *)
		then i := Lopp
	else i := ServaValik;;																	(* kui järgmisi servi leidub, lähme uut serva valima *)
	
let lopp() =
	tekst := "Magasin on tühi.\nAlgoritm lõpetab, olles leidnud sügavuti eesjärjestuses otsingu otsingupuu.";
	nk1 := string_of_toodeldudTipud(!toodeldudTipud);
	nk2 := string_of_j2rgmisedTipud(!j2rgmisedServad);
	AlgoBaas.lopp();;

let sygavutiEes(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus();
		| EsimeneTipp -> Laiuti.esimeneTipp(algtipp, servad);
		| ServaValik -> Laiuti.servaValik(servad);
		| ServaLisamine -> servaLisamine(tipud, servad);
		| Lopp -> lopp();
		| _ -> ();;