open Struktuurid;;
open AlgoBaas;;

let j2rgmisedServad = ref([]);;

let lisatudTipp = ref(tyhiTipp);;

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
	"Järjekorras olevad servad: [" ^ string_of_servad(servad) ^ "]";;

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
		);
	j2rgmisedServad := List.filter (fun s -> s <> serv) !j2rgmisedServad;;	(* eemaldame lisatava serva järjekorrast *)

(* funkstioon, mis märgib tipu vaadelduks ning omistab lisatudTipule *)
let lisaTipp(tipp) =
	tipp.tv := Vaadeldud;
	lisatudTipp := tipp;;

(* funktsioon, mis eemaldab järgmiste servade hulgast need, mis ühendavad kaht vaadeldud tippu, ja märgib need vaatlemata *)
let eemaldaServad() =
	List.iter (fun s -> s.sv := Vaatlemata) !j2rgmisedServad;
	j2rgmisedServad := List.filter (fun s -> !(!(s.tipp1).tv) <> Vaadeldud || !(!(s.tipp2).tv) <> Vaadeldud) !j2rgmisedServad;
	List.iter (fun s -> s.sv := Vaadeldav) !j2rgmisedServad;;

let algus() =
	tekst := "Primi algoritm alustab.";
	i := EsimeneTipp;;

let esimeneTipp(algtipp, servad) =
	lisaTipp(algtipp);
	tekst := "Märgime esimese tipu külastatuks.";
	(*tekst := !tekst ^ "\n" ^ string_of_j2rgmisedServad(!j2rgmisedServad);*)
	i := ServaVaatlus;;

let servaVaatlus(servad) =
	let js = leiaJ2rgServad(!lisatudTipp, servad) in				(* leiame servad, mis viivad sellest tipust külastamata tippu *)
	j2rgmisedServad := !j2rgmisedServad @ js;								(* lisame need järjekorda *)
	List.iter vaatleServ js;																(* märgime need vaadeldavateks *)
	tekst := "Lisame järjekorda kõik servad, mis lisatud tippu mõne külastamata tipuga ühendavad.";
	tekst := !tekst ^ "\n" ^ string_of_j2rgmisedServad(!j2rgmisedServad);
		if List.length !j2rgmisedServad = 0										(* kui enam servi lisada ei saa, siis lähme lõpule *)								
		then i := Lopp
	else i := ServaValik;;																	(* vastasel juhul lähme järgmisi servi lisama *)

let servaValik() =
	let lyhimServ = AlgoBaas.leiaLyhimServ(!j2rgmisedServad) in					(* valime järjekorrast lühima serva *)
	valiServ(lyhimServ);																								(* märgime selle valituks *)
	tekst := "Valime järjekorrast lühima serva.";
	tekst := !tekst ^ "\n" ^ string_of_j2rgmisedServad(!j2rgmisedServad);
	i := ServaLisamine;;

let servaLisamine(algtipp, tipud, servad) =
	let lisatavTipp = List.find (fun t -> !(t.tv) = Valitud) tipud in
	lisaTipp(lisatavTipp);																							(* märgime lisatava tipu vaadelduks *)
	let lisatavServ = List.find (fun s -> !(s.sv) = Valitud) servad in
	lisatavServ.sv := Vaadeldud;																				(* märgime lisatava serva vaadelduks *)
	eemaldaServad();																										(* eemaldame järjekorrast üleliigsed servad *)
	tekst := "Märgime vastava tipu külastatuks ja eemaldame järjekorrast need servad, mis ühendavad kaht külastatud tippu.";
	tekst := !tekst ^ "\n" ^ string_of_j2rgmisedServad(!j2rgmisedServad);
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud								(* kui kõik tipud on vaadeldud, lähme lõpule *)
		then i := Lopp
	else i := ServaVaatlus;;																						(* vastasel juhul lähme veel servi vaatlema *)

let lopp() =
	tekst := "Algoritm lõpetab, olles leidnud minimaalse toesepuu.";
	AlgoBaas.lopp();;

let prim(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus()
		| EsimeneTipp -> esimeneTipp(algtipp, servad)
		| ServaVaatlus -> servaVaatlus(servad)
		| ServaValik -> servaValik()
		| ServaLisamine -> servaLisamine(algtipp, tipud, servad)
		| Lopp -> lopp()
		| L2bi -> ()
		| _ -> ();;
		