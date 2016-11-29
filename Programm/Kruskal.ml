(* moodul Kruskal teostab sammsammulist Kruskali algoritmi läbimängu *) 

open Struktuurid;;
open AlgoBaas;;

let hulgad = Hashtbl.create 10;; 	(* sisuliselt map tipp.nimi : tipp.nimi, tippude ja hulkade vastavuse hoiustamiseks *)

let sej = ref([]);; 							(* servade eelistusjärjekord *)

(* funktsioon, mis määrab igale t2 hulgaga tipule t1 hulga, et kõigil sama hulk oleks*)
let lisaHulka(t1, t2) =
	let hulk1 = Hashtbl.find hulgad t1.nimi in
	let hulk2 = Hashtbl.find hulgad t2.nimi in
	Hashtbl.iter (fun k v -> if v = hulk2 then Hashtbl.replace hulgad k hulk1) hulgad;;

(* funkstioon, mis märgib serva ja tema tipud valituks ning määrab, kas serva lisada või mitte*)
let valiServ(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; sv = v;} -> (
			v := Valitud;
			tekst := "Valime väikseima kaaluga vaatlemata serva.";
			nk1 := string_of_sej(!sej);
			if Hashtbl.find hulgad !t1.nimi = Hashtbl.find hulgad !t2.nimi (*tekitabTsykli(!t1, !t2)*)
				then
					i := SobimatuServ
			else (
				lisaHulka(!t1, !t2);
				i := ServaLisamine
			);
			if !((!t1).tv) = Vaatlemata then (!t1).tv := Valitud;
			if !((!t2).tv) = Vaatlemata then (!t2).tv := Valitud
		);
		sej := List.tl !sej;;																									(* eemaldame serva eelistusjärjekorrast *)

(* funktsioon, mis muudab serva ja selle tippude vaadeldavust v1-st v2-ks *)
let muudaServa v1 v2 serv =
	match serv with
		| {tipp1 = t1; tipp2 = t2; sv = v;} -> (
			if !v = v1 then (
  			v := v2;
  			if !((!t1).tv) = v1 then (!t1).tv := v2;
  			if !((!t2).tv) = v1 then (!t2).tv := v2
			)
		);;

(* funktsioon, mid tagastab, kas tekkinud graaf on sidus (ehk kas kõik tipud kuuluvad samasse hulka) *)
let onSidus(tipud) =
	let esimeseHulk = Hashtbl.find hulgad (List.hd tipud).nimi in
	List.for_all (fun t -> Hashtbl.find hulgad t.nimi = esimeseHulk) tipud;;

(* algoritmi algus *)
let algus() =
	tekst := "Kruskali algoritm alustab.";
	i := Vahe1;;

(* graafi servade eelistusjärjekorda lisamine *)
let vahe1(tipud, servad) =
	List.iter (fun t -> t.tv := Vaadeldud) tipud;						(* märgime kõik tipud vaadelduteks *)
	List.iter (fun t -> Hashtbl.add hulgad t.nimi t.nimi) tipud;		(* lisame igale tipule oma hulga *)
	List.iter (fun s -> sej := !sej @ [s]) servad;
	sej := sordiJ2rjekord(!sej);
	tekst := "Lisame kõik servad eelistusjärjekorda. Koostatav graafi toes koosneb alguses ainult ühetipulistest puudest.";
	nk1 := string_of_sej(!sej);
	if List.length !sej = 0
		then (
			List.iter (fun t -> Hashtbl.add hulgad t.nimi t.nimi) tipud;	(* lisame sellele ühele tipule ka hulga *)  
			i := Lopp
		)
	else i := ServaValik;;

(* eelistusjärjekorrast lühima serva eemaldamine ja koos vastavate vaatlemata tippudega valituks märkimine *)
let servaValik(servad) =
	match List.length !sej with																									(* servade eelistusjärjekorra pikkus *)
		| 0 -> failwith("Ühtegi serva pole eelistusjärjekorras, nii ei tohiks juhtuda.")
		| _ -> valiServ(List.hd !sej);;																						(* lühim vaatlemata serv *)

(* kaht külastatud tippu ühendava serva sobimatuks märkimine ja eelistusjärjekorrast eemaldamine *)
let sobimatuServ(servad) =
	List.iter (fun s -> if !(s.sv) = Valitud then s.sv := Sobimatu) servad; 		(* märgime serva sobimatuks *)
	tekst := "See serv ühendab samas sidusas komponendis olevaid tippe, nii et seda me ei lisa. Eemaldame serva eelistusjärjekorrast.";
	nk1 := string_of_sej(!sej);
	if List.length !sej = 0
		then i := Lopp
	else i := ServaValik;;

(* valitud serva vaadelduks märkimine ja eelistusjärjekorrast eemaldamine *)
let servaLisamine(servad, tipud) =
	let lisatavServ = List.find (fun s -> !(s.sv) = Valitud) servad in
	muudaServa Valitud Vaadeldud lisatavServ; 																	(* märgime serva vaadelduks *)
	tekst := "See serv ühendab eri sidusates komponentides olevaid tippe, nii et lisame selle ja eemaldame eelistusjärjekorrast.";
	nk1 := string_of_sej(!sej);
	if List.length !sej = 0
		then i := Lopp
	else i := ServaValik;;

(* algoritmi lõpp *)
let lopp(tipud) =
	tekst := "Eelistusjärjekord on tühi. Algoritm lõpetab, olles leidnud minimaalse toes" ^ 
			(if onSidus(tipud) then "puu." else "metsa.");
	nk1 := string_of_sej(!sej);
	AlgoBaas.lopp();;

(* algoritmi samm *)
let samm(tipud, servad) = 
	match !i with
		| Algus -> algus()
		| Vahe1 -> vahe1(tipud, servad)
		| ServaValik -> servaValik(servad)
		| SobimatuServ -> sobimatuServ(servad)
		| ServaLisamine -> servaLisamine(servad, tipud)
		| Lopp -> lopp(tipud)
		| _ -> ();;