open Struktuurid;;

let valitudServ = ref(None);;

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

(* funktsioon, mis tagastab, kas tippude t1 ja t2 vahelise serva lisamine tekitaks tsükli, st*)
(* kas mõlemad tipud on vaadeldud ning kuuluvad ühte hulka (puusse) *)
let tekitabTsykli(t1, t2) =
	!(t1.tv) = Vaadeldud && !(t2.tv) = Vaadeldud && Hashtbl.find hulgad t1.nimi = Hashtbl.find hulgad t2.nimi

(* funkstioon, mis märgib serva ja tema tipud vaadelduks ning määrab, kas serva lisada või mitte*)
let vaatle(serv) =
	valitudServ := Some serv;
	match serv with
		| {tipp1 = t1; tipp2 = t2; sv = v;} -> (
			v := Vaadeldav;
			if tekitabTsykli(!t1, !t2)
				then (
					tekst := "Vaatleme serva. Selle lisamine tekitaks tsükli, nii et seda me ei lisa.";
					i := ServaVaatlus
				)
			else (
				lisaHulka(!t1, !t2);
				tekst := "Vaatleme serva.";
				i := ServaValik
			);
			if !((!t1).tv) = Vaatlemata then (!t1).tv := Vaadeldav;
			if !((!t2).tv) = Vaatlemata then (!t2).tv := Vaadeldav
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

let algus() =
	tekst := "Kruskali algoritm alustab.";
	i := ServaVaatlus;;

let servaVaatlus(servad) =
	let vs = List.filter (fun s -> !(s.sv) = Vaatlemata) servad in (*vaatlemata servad*)
	match List.length vs with
		| 0 -> print_endline("Mingi viga, nii ei tohiks juhtuda.") (* TODO!! *)
		| 1 -> vaatle(List.hd vs)
		| _ -> (
    	let lvs = List.fold_left (fun s1 s2 -> if s1.kaal < s2.kaal then s1 else s2) (List.hd vs) (List.tl vs) in
    		(*lühim vaatlemata serv*)
    	vaatle(lvs)
		);;

let servaValik(servad) =
	tekst := "Selle serva lisamine tsüklit ei tekitaks, nii et valime selle.";
	match !valitudServ with
		| None -> ()
		| Some s -> muudaServa Vaadeldav Valitud s;
	i := ServaLisamine;;

let servaLisamine(servad, tipud) =
	tekst := "Lisame serva.";
	match !valitudServ with
		| None -> ()
		| Some s -> muudaServa Valitud Vaadeldud s; 
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud
		then i := Lopp
	else i := ServaVaatlus;;

let lopp() =
	tekst := "Algoritm lõpetab, olles leidnud minimaalse toesepuu.";
	algoL2bi := true;
	i := L2bi;;


let kruskal(tipud, servad) = 
	match !i with
		| Algus -> algus()
		| ServaVaatlus -> servaVaatlus(servad)
		| ServaValik -> servaValik(servad)
		| ServaLisamine -> servaLisamine(servad, tipud)
		| Lopp -> lopp()
		| _ -> ();;