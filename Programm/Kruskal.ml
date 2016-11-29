(* moodul Kruskal teostab sammsammulist Kruskali algoritmi l�bim�ngu *) 

open Struktuurid;;
open AlgoBaas;;

let hulgad = Hashtbl.create 10;; 	(* sisuliselt map tipp.nimi : tipp.nimi, tippude ja hulkade vastavuse hoiustamiseks *)

let sej = ref([]);; 							(* servade eelistusj�rjekord *)

(* funktsioon, mis m��rab igale t2 hulgaga tipule t1 hulga, et k�igil sama hulk oleks*)
let lisaHulka(t1, t2) =
	let hulk1 = Hashtbl.find hulgad t1.nimi in
	let hulk2 = Hashtbl.find hulgad t2.nimi in
	Hashtbl.iter (fun k v -> if v = hulk2 then Hashtbl.replace hulgad k hulk1) hulgad;;

(* funkstioon, mis m�rgib serva ja tema tipud valituks ning m��rab, kas serva lisada v�i mitte*)
let valiServ(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; sv = v;} -> (
			v := Valitud;
			tekst := "Valime v�ikseima kaaluga vaatlemata serva.";
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
		sej := List.tl !sej;;																									(* eemaldame serva eelistusj�rjekorrast *)

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

(* funktsioon, mid tagastab, kas tekkinud graaf on sidus (ehk kas k�ik tipud kuuluvad samasse hulka) *)
let onSidus(tipud) =
	let esimeseHulk = Hashtbl.find hulgad (List.hd tipud).nimi in
	List.for_all (fun t -> Hashtbl.find hulgad t.nimi = esimeseHulk) tipud;;

(* algoritmi algus *)
let algus() =
	tekst := "Kruskali algoritm alustab.";
	i := Vahe1;;

(* graafi servade eelistusj�rjekorda lisamine *)
let vahe1(tipud, servad) =
	List.iter (fun t -> t.tv := Vaadeldud) tipud;						(* m�rgime k�ik tipud vaadelduteks *)
	List.iter (fun t -> Hashtbl.add hulgad t.nimi t.nimi) tipud;		(* lisame igale tipule oma hulga *)
	List.iter (fun s -> sej := !sej @ [s]) servad;
	sej := sordiJ2rjekord(!sej);
	tekst := "Lisame k�ik servad eelistusj�rjekorda. Koostatav graafi toes koosneb alguses ainult �hetipulistest puudest.";
	nk1 := string_of_sej(!sej);
	if List.length !sej = 0
		then (
			List.iter (fun t -> Hashtbl.add hulgad t.nimi t.nimi) tipud;	(* lisame sellele �hele tipule ka hulga *)  
			i := Lopp
		)
	else i := ServaValik;;

(* eelistusj�rjekorrast l�hima serva eemaldamine ja koos vastavate vaatlemata tippudega valituks m�rkimine *)
let servaValik(servad) =
	match List.length !sej with																									(* servade eelistusj�rjekorra pikkus *)
		| 0 -> failwith("�htegi serva pole eelistusj�rjekorras, nii ei tohiks juhtuda.")
		| _ -> valiServ(List.hd !sej);;																						(* l�him vaatlemata serv *)

(* kaht k�lastatud tippu �hendava serva sobimatuks m�rkimine ja eelistusj�rjekorrast eemaldamine *)
let sobimatuServ(servad) =
	List.iter (fun s -> if !(s.sv) = Valitud then s.sv := Sobimatu) servad; 		(* m�rgime serva sobimatuks *)
	tekst := "See serv �hendab samas sidusas komponendis olevaid tippe, nii et seda me ei lisa. Eemaldame serva eelistusj�rjekorrast.";
	nk1 := string_of_sej(!sej);
	if List.length !sej = 0
		then i := Lopp
	else i := ServaValik;;

(* valitud serva vaadelduks m�rkimine ja eelistusj�rjekorrast eemaldamine *)
let servaLisamine(servad, tipud) =
	let lisatavServ = List.find (fun s -> !(s.sv) = Valitud) servad in
	muudaServa Valitud Vaadeldud lisatavServ; 																	(* m�rgime serva vaadelduks *)
	tekst := "See serv �hendab eri sidusates komponentides olevaid tippe, nii et lisame selle ja eemaldame eelistusj�rjekorrast.";
	nk1 := string_of_sej(!sej);
	if List.length !sej = 0
		then i := Lopp
	else i := ServaValik;;

(* algoritmi l�pp *)
let lopp(tipud) =
	tekst := "Eelistusj�rjekord on t�hi. Algoritm l�petab, olles leidnud minimaalse toes" ^ 
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