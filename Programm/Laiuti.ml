(* moodul Laiuti teostab sammsammulist graafi laiuti l�bimist *) 

open Struktuurid;;
open AlgoBaas;;

let j2rgmisedServad = ref([]);;			(* j�rgmised servad *)

let toodeldudTipud = ref([]);;			(* t��deldud tipud *)

(* funktsioon, mis tagastab, kas serv v�ljub tipust ning on vaatlemata *)
let sobivJ2rgServ tipp serv =
	match serv with 
		| {tipp1 = t1; tipp2 = t2; nool = n} ->
			match n with
				| false -> !t1 = tipp && !((!t2).tv) = Vaatlemata || !t2 = tipp && !((!t1).tv) = Vaatlemata	(* mittesuunatud graaf *)
				| true -> !t1 = tipp && !((!t2).tv) = Vaatlemata;;																					(* suunatud graaf *)

(* funktsioon, mis leiab tipu p�hjal j�rgmised vaatlemata servad, mida k�lastada. Mittesuunatud graafide puhul sobivad servad, *)
(* mille esimene tipp on vastav tipp ja teine on vaatlemata v�i vastupidi, suunatud graafide puhul sobivad vaid servad, mille*)
(* esimene tipp on vastav tipp ja teine on vaatlemata *)
let leiaJ2rgServad(tipp, servad) =
	List.filter (sobivJ2rgServ tipp) servad;;

(* leiab serva j�rgi lisatava tipu (selle, mis on valitud) *)
let leiaLisatavTipp(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> if !((!t1).tv) = Valitud then !t1 else !t2;;

(* funktsioon, mis eemaldab serva j�rgmiste servade nimekirjast, juhul kui kumbki tema tipp pole enam vaatlemata *)
let rec eemalda(servad) =
	List.filter (fun s -> !(!(s.tipp1).tv) = Vaatlemata || !(!(s.tipp2).tv) = Vaatlemata) servad;;

(* funktsioon, mis j�rgib serva ja vastava vaatlemata tipu valituks *)
let valiServ(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; sv = v;} -> (
			v := Valitud;
			if !((!t1).tv) = Vaatlemata then (!t1).tv := Valitud;
			if !((!t2).tv) = Vaatlemata then (!t2).tv := Valitud; 
		);;

(* funktsioon, mis lisab serva, st m�rgib tema ja tema tipud vaadelduks *)
let lisaServ(lisatavServ) = 
	match lisatavServ with
		| {tipp1 = t1; tipp2 = t2; sv = v} -> (
			v := Vaadeldud;
			(!t1).tv := Vaadeldud;
			(!t2).tv := Vaadeldud;
		);;

(* funktsioon listist korduvate elementide eemaldamiseks algusest, tagades j�rjekorra s�ilimise l�pust vaadates. *)
let rec eemaldaKorduvadAlgusest(list) =
	match list with
		| x::xs -> if List.mem x xs then xs else x :: eemaldaKorduvadAlgusest(xs)
		| [] -> [];;

(* funktsioon listist korduvate elementide eemaldamiseks l�pust, tagades j�rjekorra s�ilimise algusest vaadates.*)
let eemaldaKorduvadLopust(list) = List.rev (eemaldaKorduvadAlgusest(List.rev list));;

(* funktsioon j�rgmiste tippude s�nena esitamiseks *)
(*let string_of_j2rgmisedTipud(servad) = 
	let tipud =	List.map (fun s -> if !(!(s.tipp1).tv) = Vaatlemata then !(s.tipp1) else !(s.tipp2)) servad in
	let unikaalsedTipud = eemaldaKorduvadLopust(tipud) in
	"J�rgmised tipud: " ^ string_of_tipud(unikaalsedTipud);;*)
	
let string_of_j2rgmisedServad(servad) =
	"J�rgmised servad: " ^ string_of_servad(servad);;

(* funktsioon nimekirjade uuendamiseks *)
let lisatekst() =
	nk1 := string_of_toodeldudTipud(!toodeldudTipud);
	(*nk2 := string_of_j2rgmisedTipud(!j2rgmisedServad);;*)
	nk2 := string_of_j2rgmisedServad(!j2rgmisedServad);;

(* algoritmi algus, mille k�igus nullime �ra t��deldud tipud (t�idetud graafikontrollis) ja j�rgmised servad *)
let algus() =
	tekst := "Laiuti l�bimise algoritm alustab valitud algtipust.";
	toodeldudTipud := [];
	j2rgmisedServad := [];
	i := EsimeneTipp;;
	
(* esimese tipu vaadelduks m�rkimine, j�rgmiste servade leidmine ja j�rjekorda/magasini lisamine *)
let esimeneTipp(algtipp, servad) =
	algtipp.tv := Vaadeldud;
	toodeldudTipud := (!toodeldudTipud) @ [algtipp];
	let js = leiaJ2rgServad(algtipp, servad) in
	j2rgmisedServad := (!j2rgmisedServad) @ js;
	tekst := "M�rgime esimese tipu k�lastatuks ja lisame " ^ (if !algo = Laiuti then "j�rjekorda" else "magasini") ^ 
		(*" need tipud, kuhu �sja t��deldud tipust serv viib.";*)
		" k�ik selle tipuga seotud servad.";
	lisatekst();
	if List.length !j2rgmisedServad = 0											(* kui j�rgmiste servade j�rjekord on t�hi, l�hme l�pule *)
		then i := Lopp
	else i := ServaValik;;																	(* kui j�rgmisi servi leidub, l�hme uut serva valima *)

(* j�rjekorrast/magasinist j�rgmise serva valimine ja koos vastava tipuga valituks m�rkimine *)
let servaValik(servad) =
	let lisatavServ = List.hd !j2rgmisedServad in 					(* valime j�rjekorra algusest j�rgmise serva *)
	j2rgmisedServad := List.tl !j2rgmisedServad;						(* eemaldame selle serva j�rgmiste servade j�rjekorrast *)
	valiServ(lisatavServ);																	(* m�rgime serva ja vastava tipu valituks *)
	tekst := "Valime " ^ (if !algo = Laiuti then "j�rjekorrast j�rgmise" else "magasinist pealmise") ^
			" serva ja eemaldame selle " ^ (if !algo = Laiuti then "j�rjekorrast." else "magasinist.");
	lisatekst();
	if !(!(lisatavServ.tipp1).tv) = Vaadeldud && !(!(lisatavServ.tipp2).tv) = Vaadeldud
		then i := SobimatuServ 
	else i := ServaLisamine;;

(* serva ei saa lisada, sest �hendab juba t��deldud tippe *)
let sobimatuServ(servad) =
	let sobimatuServ = List.find (fun s -> !(s.sv) = Valitud) servad in
	sobimatuServ.sv := Sobimatu;
	tekst := "Valitud serv �hendab juba t��deldud tippe, nii et �htki tippu ei t��tle.";
	if List.length !j2rgmisedServad = 0
		then i := Lopp
	else i := ServaValik;;

(* valitud serva ja vastava tipu vaadelduks m�rkimine ja j�rgmiste servade leidmine ja j�rjekorda/magasini lisamine *)
let servaLisamine(tipud, servad) =
	let lisatavServ = List.find (fun s -> !(s.sv) = Valitud) servad in
	let lisatavTipp = leiaLisatavTipp(lisatavServ) in				(* lisatav tipp *)
	toodeldudTipud := (!toodeldudTipud) @ [lisatavTipp];		(* lisame selle k�lastatud tippude hulka *)
	let js = leiaJ2rgServad(lisatavTipp, servad) in 				(* leiame need servad, kuhu valitud tipust viib *)
	j2rgmisedServad := if !algo = Laiuti then !j2rgmisedServad @ js else js @ !j2rgmisedServad; 						
																													(* lisame need j�rgmiste servade j�rjekorra l�ppu / magasini *)
	(*j2rgmisedServad := eemalda(!j2rgmisedServad);*) 					(* eemaldame j�rgmiste servade j�rjekorrast / magasinist need 
																														servad, mis �hendavad k�lastatud tippe *)
	lisaServ(lisatavServ);																	(* m�rgime serva ja tema tipud vaadelduks *)
	tekst := "M�rgime serva t��tlemata otstipu t��delduks ja lisame " ^ (if !algo = Laiuti then "j�rjekorra l�ppu" else "magasini") ^ 
		" �sja t��deldud tipuga seotud servad.";
	lisatekst();
	if List.length !j2rgmisedServad = 0											(* kui j�rgmiste servade j�rjekord on t�hi, l�hme l�pule *)
		then i := Lopp
	else i := ServaValik;;																	(* kui j�rgmisi servi leidub, l�hme uut serva valima *)

(* algoritmi l�pp *)
let lopp() =
	tekst := "J�rjekord on t�hi. Laiuti l�bimise algoritm l�petab, olles leidnud laiuti l�bimise puu.";
	lisatekst();
	AlgoBaas.lopp();;

(* algoritmi samm *)
let samm(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus();
		| EsimeneTipp -> esimeneTipp(algtipp, servad);
		| ServaValik -> servaValik(servad);
		| SobimatuServ -> sobimatuServ(servad);
		| ServaLisamine -> servaLisamine(tipud, servad);
		| Lopp -> lopp();
		| _ -> ();;

