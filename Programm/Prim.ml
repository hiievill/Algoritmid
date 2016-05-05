open Struktuurid;;
open AlgoBaas;;

let lisatudTipp = ref(tyhiTipp);;

let sej = ref([]);;

(* funktsioon, mis tagastab, kas serv on sobiv, st kui ta on suunatud, siis ega tema esimene tipp vaadeldud pole, kui aga *)
(* mittesuunatud, siis ega mõlemad tipud vaadeldud pole *)
let sobivJ2rgServ tipp serv = (* sarnane nagu Laiutis, aga mitte päris *)
	match serv with 
		| {tipp1 = t1; tipp2 = t2; nool = n} ->
			match n with
				| false -> !t1 = tipp && !((!t2).tv) <> Vaadeldud || !t2 = tipp && !((!t1).tv) <> Vaadeldud	(* mittesuunatud graaf *)
				| true -> !t1 = tipp && !((!t2).tv) <> Vaadeldud;;																					(* suunatud graaf *)

(* funktsioon järgmiste sobivate servade leidmiseks *)
let leiaJ2rgServad(tipp, servad) =
	List.filter (sobivJ2rgServ tipp) servad;;

(* funktsioon järjekorras olevate servade sõnena esitamiseks *)
let string_of_j2rgmisedServad(servad) =
	"Järjekorras olevad servad: " ^ string_of_servad(servad);;

(* funktsioon, mis märgib serva ja tema tipud vaadeldavaks, kui need enne vaatlemata olid *)
let vaatleServ(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> (
			if !((!t1).tv) = Vaatlemata then (!t1).tv := Vaadeldav;
			if !((!t2).tv) = Vaatlemata then (!t2).tv := Vaadeldav;
			serv.sv := Vaadeldav;
		);;

(* funktsioon, mis märgib serva ja tema tipud valituks, kui nad enne vaadeldavad olid, ja eemaldab serva järjekorrast *)
let valiServ(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> (
			if !((!t1).tv) = Vaadeldav then (!t1).tv := Valitud;
			if !((!t2).tv) = Vaadeldav then (!t2).tv := Valitud;
			serv.sv := Valitud;
		);;

(* funkstioon, mis märgib tipu vaadelduks ning omistab lisatudTipule *)
let lisaTipp(tipp) =
	tipp.tv := Vaadeldud;
	lisatudTipp := tipp;;

let algus(servad) =
	tekst := "Primi algoritm alustab."; (* TODO: kas lõpetama peaks, sest kõik tipud on läbitud, või peaks viimase serv ka ära vaatama? *)
	nk1 := string_of_sej(!sej);
	i := EsimeneTipp;;

let esimeneTipp(algtipp, servad) =
	lisaTipp(algtipp);
	tekst := "Märgime esimese tipu külastatuks.";
	nk1 := string_of_sej(!sej);
	i := ServaVaatlus;;

let servaVaatlus(servad) =
	let js = leiaJ2rgServad(!lisatudTipp, servad) in				(* leiame servad, mis viivad sellest tipust külastamata tippu *)
	List.iter vaatleServ js;																(* märgime need vaadeldavateks *)
	sej := !sej @ js;
	sej := sordiJ2rjekord(!sej);
	tekst := "Lisame eelistusjärjekorda kõik servad, mis äsja lisatud tippu mõne külastamata tipuga ühendavad.";
	nk1 := string_of_sej(!sej);
		if List.length !sej = 0																(* kui enam servi lisada ei saa, siis lähme lõpule *)								
		then i := Lopp
	else i := ServaValik;;																	(* vastasel juhul lähme järgmisi servi lisama *)

let servaValik() =
	let lyhimServ = List.hd !sej in																			(* valime eelistusjärjekorrast lühima serva *)
	valiServ(lyhimServ);																								(* märgime selle valituks *)
	tekst := "Valime eelistusjärjekorrast väikseima kaaluga serva.";
	nk1 := string_of_sej(!sej);
	i := ServaLisamine;;

let servaLisamine(algtipp, tipud, servad) =
	let lisatavServ = List.hd !sej in
	if !(!(lisatavServ.tipp1).tv) = Vaadeldud && !(!(lisatavServ.tipp2).tv) = Vaadeldud
		then (	(* serv ühendab juba vaadeldud tippe -> muudame sobimatuks *)
			lisatavServ.sv := Sobimatu;
			tekst := "See serv ei sobi.";
		)
	else (		(* sobib, lisame *)
  	let lisatavTipp = List.find (fun t -> !(t.tv) = Valitud) tipud in
  	lisaTipp(lisatavTipp);																							(* märgime lisatava tipu vaadelduks *)
  	lisatavServ.sv := Vaadeldud;																				(* märgime lisatava serva vaadelduks *)
		tekst := "Märgime valitud serva teise otstipu külastatuks ja eemaldame serva eelistusjärjekorrast.";
	);
	sej := List.tl !sej;																								(* eemaldame järjekorrast üleliigsed servad *)
	nk1 := string_of_sej(!sej);
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud								(* kui kõik tipud on vaadeldud, lähme lõpule *)
		then i := Lopp
	else i := ServaVaatlus;;																						(* vastasel juhul lähme veel servi vaatlema *)

let lopp() =
	tekst := "Algoritm lõpetab, olles leidnud minimaalse toesepuu.";
	nk1 := string_of_sej(!sej);
	AlgoBaas.lopp();;

let prim(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus(servad)
		| EsimeneTipp -> esimeneTipp(algtipp, servad)
		| ServaVaatlus -> servaVaatlus(servad)
		| ServaValik -> servaValik()
		| ServaLisamine -> servaLisamine(algtipp, tipud, servad)
		| Lopp -> lopp()
		| L2bi -> ()
		| _ -> ();;
		