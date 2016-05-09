(* moodul Laiuti teostab sammsammulist graafi laiuti läbimist *) 

open Struktuurid;;
open AlgoBaas;;

let j2rgmisedServad = ref([]);;			(* järgmised servad *)

let toodeldudTipud = ref([]);;			(* töödeldud tipud *)

(* funktsioon, mis tagastab, kas serv väljub tipust ning on vaatlemata *)
let sobivJ2rgServ tipp serv =
	match serv with 
		| {tipp1 = t1; tipp2 = t2; nool = n} ->
			match n with
				| false -> !t1 = tipp && !((!t2).tv) = Vaatlemata || !t2 = tipp && !((!t1).tv) = Vaatlemata	(* mittesuunatud graaf *)
				| true -> !t1 = tipp && !((!t2).tv) = Vaatlemata;;																					(* suunatud graaf *)

(* funktsioon, mis leiab tipu põhjal järgmised vaatlemata servad, mida külastada. Mittesuunatud graafide puhul sobivad servad, *)
(* mille esimene tipp on vastav tipp ja teine on vaatlemata või vastupidi, suunatud graafide puhul sobivad vaid servad, mille*)
(* esimene tipp on vastav tipp ja teine on vaatlemata *)
let leiaJ2rgServad(tipp, servad) =
	List.filter (sobivJ2rgServ tipp) servad;;

(* leiab serva järgi lisatava tipu (selle, mis on valitud) *)
let leiaLisatavTipp(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> if !((!t1).tv) = Valitud then !t1 else !t2;;

(* funktsioon, mis eemaldab serva järgmiste servade nimekirjast, juhul kui kumbki tema tipp pole enam vaatlemata *)
let rec eemalda(servad) =
	List.filter (fun s -> !(!(s.tipp1).tv) = Vaatlemata || !(!(s.tipp2).tv) = Vaatlemata) servad;;

(* funktsioon, mis järgib serva ja vastava vaatlemata tipu valituks *)
let valiServ(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; sv = v;} -> (
			v := Valitud;
			if !((!t1).tv) = Vaatlemata then (!t1).tv := Valitud;
			if !((!t2).tv) = Vaatlemata then (!t2).tv := Valitud; 
		);;

(* funktsioon, mis lisab serva, st märgib tema ja tema tipud vaadelduks *)
let lisaServ(lisatavServ) = 
	match lisatavServ with
		| {tipp1 = t1; tipp2 = t2; sv = v} -> (
			v := Vaadeldud;
			(!t1).tv := Vaadeldud;
			(!t2).tv := Vaadeldud;
		);;

(* funktsioon listist korduvate elementide eemaldamiseks algusest, tagades järjekorra säilimise lõpust vaadates. *)
let rec eemaldaKorduvadAlgusest(list) =
	match list with
		| x::xs -> if List.mem x xs then xs else x :: eemaldaKorduvadAlgusest(xs)
		| [] -> [];;

(* funktsioon listist korduvate elementide eemaldamiseks lõpust, tagades järjekorra säilimise algusest vaadates.*)
let eemaldaKorduvadLopust(list) = List.rev (eemaldaKorduvadAlgusest(List.rev list));;

(* funktsioon järgmiste tippude sõnena esitamiseks *)
let string_of_j2rgmisedTipud(servad) = 
	let tipud =	List.map (fun s -> if !(!(s.tipp1).tv) = Vaatlemata then !(s.tipp1) else !(s.tipp2)) servad in
	let unikaalsedTipud = eemaldaKorduvadLopust(tipud) in
	"Järgmised tipud: " ^ string_of_tipud(unikaalsedTipud);;

(* funktsioon nimekirjade uuendamiseks *)
let lisatekst() =
	nk1 := string_of_toodeldudTipud(!toodeldudTipud);
	nk2 := string_of_j2rgmisedTipud(!j2rgmisedServad);;

(* algoritmi algus, mille käigus nullime ära töödeldud tipud (täidetud graafikontrollis) ja järgmised servad *)
let algus() =
	tekst := "Laiuti läbimise algoritm alustab valitud algtipust.";
	toodeldudTipud := [];
	j2rgmisedServad := [];
	i := EsimeneTipp;;
	
(* esimese tipu vaadelduks märkimine, järgmiste servade leidmine ja järjekorda/magasini lisamine *)
let esimeneTipp(algtipp, servad) =
	algtipp.tv := Vaadeldud;
	toodeldudTipud := (!toodeldudTipud) @ [algtipp];
	let js = leiaJ2rgServad(algtipp, servad) in
	j2rgmisedServad := (!j2rgmisedServad) @ js;
	tekst := "Märgime esimese tipu külastatuks ja lisame " ^ (if !algo = Laiuti then "järjekorda" else "magasini") ^ 
		" need tipud, kuhu äsja töödeldud tipust serv viib.";
	lisatekst();
	if List.length !j2rgmisedServad = 0											(* kui järgmiste servade järjekord on tühi, lähme lõpule *)
		then i := Lopp
	else i := ServaValik;;																	(* kui järgmisi servi leidub, lähme uut serva valima *)

(* järjekorrast/magasinist järgmise serva valimine ja koos vastava tipuga valituks märkimine *)
let servaValik(servad) =
	let lisatavServ = List.hd !j2rgmisedServad in 					(* valime järjekorra algusest järgmise serva *)
	j2rgmisedServad := List.tl !j2rgmisedServad;						(* eemaldame selle serva järgmiste servade järjekorrast *)
	valiServ(lisatavServ);																	(* märgime serva ja vastava tipu valituks *)
	tekst := "Valime " ^ (if !algo = Laiuti then "järjekorrast järgmise" else "magasinist pealmise") ^
			" tipu ja eemaldame selle " ^ (if !algo = Laiuti then "järjekorrast." else "magasinist.");
	lisatekst();
	i := ServaLisamine;;

(* valitud serva ja vastava tipu vaadelduks märkimine ja järgmiste servade leidmine ja järjekorda/magasini lisamine *)
let servaLisamine(tipud, servad) =
	let lisatavServ = List.find (fun s -> !(s.sv) = Valitud) servad in
	let lisatavTipp = leiaLisatavTipp(lisatavServ) in				(* lisatav tipp *)
	toodeldudTipud := (!toodeldudTipud) @ [lisatavTipp];		(* lisame selle külastatud tippude hulka *)
	let js = leiaJ2rgServad(lisatavTipp, servad) in 				(* leiame need servad, kuhu valitud tipust viib *)
	j2rgmisedServad := if !algo = Laiuti then !j2rgmisedServad @ js else js @ !j2rgmisedServad; 						
																													(* lisame need järgmiste servade järjekorra lõppu / magasini *)
	j2rgmisedServad := eemalda(!j2rgmisedServad); 					(* eemaldame järgmiste servade järjekorrast / magasinist need 
																														servad, mis ühendavad külastatud tippe *)
	lisaServ(lisatavServ);																	(* märgime serva ja tema tipud vaadelduks *)
	tekst := "Märgime selle tipu töödelduks ja lisame " ^ (if !algo = Laiuti then "järjekorra lõppu" else "magasini") ^ 
		" need töötlemata tipud, kuhu äsja töödeldud tipust serv viib.";
	lisatekst();
	if List.length !j2rgmisedServad = 0											(* kui järgmiste servade järjekord on tühi, lähme lõpule *)
		then i := Lopp
	else i := ServaValik;;																	(* kui järgmisi servi leidub, lähme uut serva valima *)

(* algoritmi lõpp *)
let lopp() =
	tekst := "Järjekord on tühi. Laiuti läbimise algoritm lõpetab.";
	lisatekst();
	AlgoBaas.lopp();;

(* algoritmi samm *)
let samm(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus();
		| EsimeneTipp -> esimeneTipp(algtipp, servad);
		| ServaValik -> servaValik(servad);
		| ServaLisamine -> servaLisamine(tipud, servad);
		| Lopp -> lopp();
		| _ -> ();;

