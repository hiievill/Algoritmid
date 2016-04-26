open Struktuurid;;
open AlgoBaas;;

let hulgad = Hashtbl.create 10;; (* sisuliselt map tipp.nimi : tipp.nimi *)

(* funktsioon, mis määrab tippudele t1 ja t2 hulgad järgnevalt:*)
(* kui mõlemal tipul on juba hulk, paneb kõikidele t2 hulgaga tippudele t1 hulga, et kõigil sama hulk oleks*)
(* kui ühel neist on hulk, teisel mitte, määrab hulgata tipule hulgaga tipu hulga*)
(* kui kummalgi pole hulka, siis määrab mõlemale uue hulga, milleks on t1 nimi*)
let lisaHulka(t1, t2) =
	if !(t1.tv) = Vaadeldud && !(t2.tv) = Vaadeldud
		then (
			let hulk1 = Hashtbl.find hulgad t1.nimi in
			let hulk2 = Hashtbl.find hulgad t2.nimi in
			Hashtbl.iter (fun k v -> if v = hulk2 then Hashtbl.replace hulgad k hulk1) hulgad
		)
	else if !(t1.tv) = Vaadeldud 
		then (
  		let hulk = Hashtbl.find hulgad t1.nimi in
  		Hashtbl.add hulgad t2.nimi hulk;
  	)
  else if !(t2.tv) = Vaadeldud 
		then (
			let hulk = Hashtbl.find hulgad t2.nimi in
			Hashtbl.add hulgad t1.nimi hulk;
  	)
  else (
		Hashtbl.add hulgad t1.nimi t1.nimi;
		Hashtbl.add hulgad t2.nimi t1.nimi;
	);;

(* funktsioon, mis tagastab, kas tippude t1 ja t2 vahelise serva lisamine tekitaks tsükli (kuuluvad samasse sidusasse*)
(* komponenti), st kas mõlemad tipud on vaadeldud ning kuuluvad ühte hulka (puusse) *)
let tekitabTsykli(t1, t2) =
	!(t1.tv) = Vaadeldud && !(t2.tv) = Vaadeldud && Hashtbl.find hulgad t1.nimi = Hashtbl.find hulgad t2.nimi

(* funkstioon, mis märgib serva ja tema tipud valituks ning määrab, kas serva lisada või mitte*)
let valiServ(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; sv = v;} -> (
			v := Valitud;
			tekst := "Valime väikseima kaaluga vaatlemata serva.";
			if tekitabTsykli(!t1, !t2)
				then
					i := SobimatuServ
			else (
				lisaHulka(!t1, !t2);
				i := ServaLisamine
			);
			if !((!t1).tv) = Vaatlemata then (!t1).tv := Valitud;
			if !((!t2).tv) = Vaatlemata then (!t2).tv := Valitud
		);;

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

let algus(servad) =
	(*AlgoBaas.graafiKontroll(servad, true, false, true);*)
	tekst := "Kruskali algoritm alustab.";
	i := ServaValik;;

let servaValik(servad) =
	let vs = List.filter (fun s -> !(s.sv) = Vaatlemata) servad in 							(*vaatlemata servad*)
	match List.length vs with
		| 0 -> print_endline("Mingi viga, nii ei tohiks juhtuda.") 								(* TODO!! See kontroll varem.*)
		| 1 -> valiServ(List.hd vs)
		| _ -> (
			let lvs = AlgoBaas.leiaLyhimServ(vs) in 																(*lühim vaatlemata serv*)
    	valiServ(lvs)
		);;

let sobimatuServ(servad) =
	tekst := "See serv ühendab samas sidusas komponendis olevaid tippe, nii et seda me ei lisa.";
	List.iter (fun s -> if !(s.sv) = Valitud then s.sv := Sobimatu) servad; 		(* märgime serva sobimatuks *)
	i := ServaValik;;

let servaLisamine(servad, tipud) =
	tekst := "See serv ühendab eri sidusaid komponente olevaid tippe, nii et lisame selle.";
	let lisatavServ = List.find (fun s -> !(s.sv) = Valitud) servad in
	muudaServa Valitud Vaadeldud lisatavServ; 																	(* märgime serva vaadelduks *)
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud
		then i := Lopp
	else i := ServaValik;;

let lopp() =
	tekst := "Algoritm lõpetab, olles leidnud minimaalse toesepuu.";
	AlgoBaas.lopp();;


let kruskal(tipud, servad) = 
	match !i with
		| Algus -> algus(servad)
		| ServaValik -> servaValik(servad)
		| SobimatuServ -> sobimatuServ(servad)
		| ServaLisamine -> servaLisamine(servad, tipud)
		| Lopp -> lopp()
		| _ -> ();;