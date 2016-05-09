(* moodul Prim teostab sammsammulist Primi algoritmi l�bim�ngu *) 

open Struktuurid;;
open AlgoBaas;;

let lisatudTipp = ref(tyhiTipp);;		(* lisatud tipp *)

let sej = ref([]);;									(* servade eelistusj�rjekord *)

(* funktsioon, mis tagastab, kas serv on sobiv, st kui ta on suunatud, siis ega tema esimene tipp vaadeldud pole, kui aga *)
(* mittesuunatud, siis ega m�lemad tipud vaadeldud pole *)
let sobivJ2rgServ tipp serv =
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
	"J�rjekorras olevad servad: " ^ string_of_servad(servad);;

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
		);;

(* funkstioon, mis m�rgib tipu vaadelduks ning omistab lisatudTipule *)
let lisaTipp(tipp) =
	tipp.tv := Vaadeldud;
	lisatudTipp := tipp;;

(* algoritmi algus *)
let algus(servad) =
	tekst := "Primi algoritm alustab valitud tipust.";
	(*nk1 := string_of_sej(!sej);*)
	i := EsimeneTipp;;

(* esimese tipu vaadelduks m�rkimine *)
let esimeneTipp(algtipp, servad) =
	lisaTipp(algtipp);
	tekst := "M�rgime esimese tipu k�lastatuks.";
	nk1 := string_of_sej(!sej);
	i := ServaVaatlus;;

(* seda tippu teiste k�lastamata tippudega �hendavate servade leidmine, kaalude p�hjal sortimine ja vaadeldavateks m�rkimine *)
let servaVaatlus(servad) =
	let js = leiaJ2rgServad(!lisatudTipp, servad) in				(* leiame servad, mis viivad sellest tipust k�lastamata tippu *)
	List.iter vaatleServ js;																(* m�rgime need vaadeldavateks *)
	sej := !sej @ js;
	sej := sordiJ2rjekord(!sej);
	tekst := "Lisame eelistusj�rjekorda k�ik servad, mis �sja lisatud tippu m�ne k�lastamata tipuga �hendavad.";
	nk1 := string_of_sej(!sej);
		if List.length !sej = 0																(* kui enam servi lisada ei saa, siis l�hme l�pule *)								
		then i := Lopp
	else i := ServaValik;;																	(* vastasel juhul l�hme j�rgmisi servi lisama *)

(* l�hima eelistusj�rjekorras oleva serva valituks m�rkimine *)
let servaValik() =
	let lyhimServ = List.hd !sej in																			(* valime eelistusj�rjekorrast l�hima serva *)
	valiServ(lyhimServ);																								(* m�rgime selle valituks *)
	tekst := "Valime eelistusj�rjekorrast v�ikseima kaaluga serva.";
	nk1 := string_of_sej(!sej);
	i := ServaLisamine;;

(* valitud serva lisamine, kui tema �ks otstippe on k�lastamata, ja eelistusj�rjekorrast eemaldamine *)
let servaLisamine(algtipp, tipud, servad) =
	let lisatavServ = List.hd !sej in
	if !(!(lisatavServ.tipp1).tv) = Vaadeldud && !(!(lisatavServ.tipp2).tv) = Vaadeldud
		then (	(* serv �hendab juba vaadeldud tippe -> muudame sobimatuks *)
			lisatavServ.sv := Sobimatu;
			tekst := "Seda serva ei lisa, sest see �hendab kahte k�lastatud tippu. Eemaldame serva eelistusj�rjekorrast.";
			sej := List.tl !sej;																							(* eemaldame j�rjekorrast �leliigsed servad *)
			nk1 := string_of_sej(!sej);
			if List.length !sej = 0																							(* kui enam servi lisada ei saa, siis l�hme l�pule *)
  			then i := Lopp
			else i := ServaValik
		)
	else (		(* sobib, lisame *)
  	let lisatavTipp = List.find (fun t -> !(t.tv) = Valitud) tipud in
  	lisaTipp(lisatavTipp);																							(* m�rgime lisatava tipu vaadelduks *)
  	lisatavServ.sv := Vaadeldud;																				(* m�rgime lisatava serva vaadelduks *)
		tekst := "M�rgime valitud serva teise otstipu k�lastatuks ja eemaldame serva eelistusj�rjekorrast.";
		sej := List.tl !sej;																								(* eemaldame j�rjekorrast �leliigsed servad *)
		nk1 := string_of_sej(!sej);
		i := ServaVaatlus);;																						(* vastasel juhul l�hme veel servi vaatlema *)

(* algoritmi l�pp *)
let lopp() =
	tekst := "Servade eelistusj�rjekord on t�hi. Algoritm l�petab, olles leidnud minimaalse toesepuu.";
	nk1 := string_of_sej(!sej);
	AlgoBaas.lopp();;

(* algoritmi samm *)
let samm(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus(servad)
		| EsimeneTipp -> esimeneTipp(algtipp, servad)
		| ServaVaatlus -> servaVaatlus(servad)
		| ServaValik -> servaValik()
		| ServaLisamine -> servaLisamine(algtipp, tipud, servad)
		| Lopp -> lopp()
		| L2bi -> ()
		| _ -> ();;
		