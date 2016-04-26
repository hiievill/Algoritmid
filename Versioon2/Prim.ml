open Struktuurid;;
open AlgoBaas;;

let j2rgmisedServad = ref([]);;

let lisatudTipp = ref(tyhiTipp);;

(* funktsioon, mis tagastab, kas serv on sobiv, st kui ta on suunatud, siis ega tema esimene tipp vaadeldud pole, kui aga *)
(* mittesuunatud, siis ega m�lemad tipud vaadeldud pole *)
let sobivJ2rgServ tipp serv = (* sarnane nagu Laiutis, aga mitte p�ris *)
	match serv with 
		| {tipp1 = t1; tipp2 = t2; nool = n} ->
			match n with
				| false -> !t1 = tipp && !((!t2).tv) <> Vaadeldud || !t2 = tipp && !((!t1).tv) <> Vaadeldud	(* mittesuunatud graaf *)
				| true -> !t1 = tipp && !((!t2).tv) <> Vaadeldud;;																					(* suunatud graaf *)

(* funktsioon j�rgmiste sobivate servade leidmiseks *)
let leiaJ2rgServad(tipp, servad) =
	List.filter (sobivJ2rgServ tipp) servad;;

(* funktsioon j�rjekorras olevate servade s�nena esitamiseks *)
let string_of_j2rgmisedServad(servad) =
	"J�rjekorras olevad servad: [" ^ string_of_servad(servad) ^ "]";;

(* funktsioon, mis m�rgib serva ja tema tipud vaadeldavaks, kui need enne vaatlemata olid *)
let vaatleServ(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> (
			if !((!t1).tv) = Vaatlemata then (!t1).tv := Vaadeldav;
			if !((!t2).tv) = Vaatlemata then (!t2).tv := Vaadeldav;
			serv.sv := Vaadeldav;
		);;

(* funktsioon, mis m�rgib serva ja tema tipud valituks, kui nad enne vaadeldavad olid, ja eemaldab serva j�rjekorrast *)
let valiServ(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> (
			if !((!t1).tv) = Vaadeldav then (!t1).tv := Valitud;
			if !((!t2).tv) = Vaadeldav then (!t2).tv := Valitud;
			serv.sv := Valitud;
		);
	j2rgmisedServad := List.filter (fun s -> s <> serv) !j2rgmisedServad;;	(* eemaldame lisatava serva j�rjekorrast *)

(* funkstioon, mis m�rgib tipu vaadelduks ning omistab lisatudTipule *)
let lisaTipp(tipp) =
	tipp.tv := Vaadeldud;
	lisatudTipp := tipp;;

(* funktsioon, mis eemaldab j�rgmiste servade hulgast need, mis �hendavad kaht vaadeldud tippu, ja m�rgib need vaatlemata *)
let eemaldaServad() =
	List.iter (fun s -> s.sv := Vaatlemata) !j2rgmisedServad;
	j2rgmisedServad := List.filter (fun s -> !(!(s.tipp1).tv) <> Vaadeldud || !(!(s.tipp2).tv) <> Vaadeldud) !j2rgmisedServad;
	List.iter (fun s -> s.sv := Vaadeldav) !j2rgmisedServad;;

let algus() =
	tekst := "Primi algoritm alustab.";
	i := EsimeneTipp;;

let esimeneTipp(algtipp, servad) =
	lisaTipp(algtipp);
	tekst := "M�rgime esimese tipu k�lastatuks.";
	(*tekst := !tekst ^ "\n" ^ string_of_j2rgmisedServad(!j2rgmisedServad);*)
	i := ServaVaatlus;;

let servaVaatlus(servad) =
	let js = leiaJ2rgServad(!lisatudTipp, servad) in				(* leiame servad, mis viivad sellest tipust k�lastamata tippu *)
	j2rgmisedServad := !j2rgmisedServad @ js;								(* lisame need j�rjekorda *)
	List.iter vaatleServ js;																(* m�rgime need vaadeldavateks *)
	tekst := "Lisame j�rjekorda k�ik servad, mis lisatud tippu m�ne k�lastamata tipuga �hendavad.";
	tekst := !tekst ^ "\n" ^ string_of_j2rgmisedServad(!j2rgmisedServad);
		if List.length !j2rgmisedServad = 0										(* kui enam servi lisada ei saa, siis l�hme l�pule *)								
		then i := Lopp
	else i := ServaValik;;																	(* vastasel juhul l�hme j�rgmisi servi lisama *)

let servaValik() =
	let lyhimServ = AlgoBaas.leiaLyhimServ(!j2rgmisedServad) in					(* valime j�rjekorrast l�hima serva *)
	valiServ(lyhimServ);																								(* m�rgime selle valituks *)
	tekst := "Valime j�rjekorrast l�hima serva.";
	tekst := !tekst ^ "\n" ^ string_of_j2rgmisedServad(!j2rgmisedServad);
	i := ServaLisamine;;

let servaLisamine(algtipp, tipud, servad) =
	let lisatavTipp = List.find (fun t -> !(t.tv) = Valitud) tipud in
	lisaTipp(lisatavTipp);																							(* m�rgime lisatava tipu vaadelduks *)
	let lisatavServ = List.find (fun s -> !(s.sv) = Valitud) servad in
	lisatavServ.sv := Vaadeldud;																				(* m�rgime lisatava serva vaadelduks *)
	eemaldaServad();																										(* eemaldame j�rjekorrast �leliigsed servad *)
	tekst := "M�rgime vastava tipu k�lastatuks ja eemaldame j�rjekorrast need servad, mis �hendavad kaht k�lastatud tippu.";
	tekst := !tekst ^ "\n" ^ string_of_j2rgmisedServad(!j2rgmisedServad);
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud								(* kui k�ik tipud on vaadeldud, l�hme l�pule *)
		then i := Lopp
	else i := ServaVaatlus;;																						(* vastasel juhul l�hme veel servi vaatlema *)

let lopp() =
	tekst := "Algoritm l�petab, olles leidnud minimaalse toesepuu.";
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
		