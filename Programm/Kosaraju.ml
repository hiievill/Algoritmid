(* moodul Kosaraju teostab sammsammulist Kosaraju algoritmi l�bim�ngu *) 

open Struktuurid;;
open AlgoBaas;;

let sygavutiTipud = ref([]);;
let komponendid = ref([]);; 		(* k�ik sidusad komponendid. TODO: hoopis hulgana teha? *)
let komponent = ref([]);; 			(* �ks sidus komponent *)

(* p��ran servad vastupidi, st vahetan tipud �ra *)
let rec ps vanadAndmed serv =
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> (
			let (vanaX, vanaY, vanaR, vanaK) = Hashtbl.find vanadAndmed (!t1.nimi ^ ":" ^ !t2.nimi) in
			let t = !t1 in 
			serv.tipp1 := !t2;
			serv.tipp2 := t; 
			(*muudaKaareAndmeid(!(x.tipp1), !(x.tipp2));*)
			uuendaKaareAndmeid(!(serv.tipp1), !(serv.tipp2), vanaX, vanaY, vanaR, vanaK)
		);;

let pooraServad(servad) =
	let vanadAndmed = Hashtbl.create 10 in
	List.iter (fun s -> let n = !(s.tipp1).nimi ^ ":" ^ !(s.tipp2).nimi in Hashtbl.add vanadAndmed n 
			(Hashtbl.find kaareX n, Hashtbl.find kaareY n, Hashtbl.find kaareR n, Hashtbl.find kaareX n)) servad;
	List.iter (ps vanadAndmed) servad;;


(* funktsioon s�gavuti l�ppj�rjestuse s�nena esitamiseks *)
let string_of_lopp(tipud) =
	"L�ppj�rjestus: "  ^ string_of_tipud(tipud);;

(* funktsioon tagurpidi s�gavuti l�ppj�rjestuse s�nena esitamiseks *)
let string_of_tagurpidi(tipud) =
	"Tagurpidi l�ppj�rjestus: "  ^ string_of_tipud(tipud);;		(* NB! mitte List.rev, selle p��rasin juba ringi *)

(* funktsioon tugevalt sidusate komponentide s�nena esitamiseks *)
let string_of_komponendid(komponendid) =
	"Tugevalt sidusad komponendid:" ^ List.fold_left  (fun a b -> a ^ " " ^ string_of_tipud b) "" komponendid;;

let algus(tipud, servad) =
	List.iter (fun t -> TopoKahn.uuendaSisendastet (TopoKahn.leiaSisendaste(t, servad)) t) tipud;
	tekst := "Kosaraju algoritm alustab.";
	i := Sygavuti;;

(* l�bime graafi s�gavuti l�ppj�rjestuses. Kui m�ni tipp j��b l�bi k�imata, siis mitu korda *)
let sygavuti(tipud, servad) =
	sygavutiTipud := TopoLopp.l2biSygavuti(tipud, servad);				(* l�bime graafi s�gavuti l�ppj�rjestuses *)
	tekst := "L�bime graafi s�gavuti l�ppj�rjestuses ja kirjutame v�lja tekkinud l�ppj�rjestuse.";
	nk1 := string_of_lopp(!sygavutiTipud);
	i := PooratudGraaf;;

(* tekitame p��ratud kaartega graafi *)
let pooratudGraaf(tipud, servad) =
	tekst := "Tekitame p��ratud kaartega graafi. ";
	tekst := !tekst ^ "P��rame tekkinud j�rjestuse tagurpidi ja hakkame l�bima tippe alates esimesest l�bimata tipust tagurpidi j�rjestuses.";
	sygavutiTipud := List.rev(!sygavutiTipud); (*tippude j�rjestuse pidi ka �mber p��rama*)
	nk2 := string_of_tagurpidi(!sygavutiTipud);
	pooraServad(servad);
	List.iter (fun s -> s.sv := Vaatlemata) servad;
	List.iter (fun t -> t.tv := Vaatlemata) tipud;
	i := EsimeneTipp;;

(* esimese komponenditu tipu valituks m�rkimine *)
let esimeneTipp(tipud, servad) =
	List.iter (fun t -> if !(t.tv) = Vaadeldud then t.tv := Sobimatu) tipud;
	List.iter (fun s -> if !(s.sv) = Vaadeldud then s.sv := Sobimatu) servad;
	let esimeneTipp = List.find (fun t -> !(t.tv) = Vaatlemata) !sygavutiTipud in
	esimeneTipp.tv := Valitud;
	tekst := "Valime tagurpidi l�ppj�rjestusest esimese tipu, mis pole veel �heski komponendis.";
	nk1 := string_of_tagurpidi(!sygavutiTipud);
	nk2 := string_of_komponendid(!komponendid);
	i := TeisedTipud;;

(* valitud tipust alates graafi l�bimine ja k�ikide l�bitud tippude �hendamine esimesega �heks komponendiks *)
let teisedTipud(tipud, servad) =
	let esimeneTipp = List.find (fun t -> !(t.tv) = Valitud) tipud in
	i := Algus;
	while !algoL2bi = false
		do
			Laiuti.samm(esimeneTipp, tipud, servad)
		done;
	algoL2bi := false;
	List.iter (fun t -> if !(t.tv) = Valitud then t.tv := Vaadeldud) tipud;
	List.iter (fun s -> if !(s.sv) = Valitud then s.sv := Vaadeldud) servad;
	komponent := List.filter (fun t -> !(t.tv) = Vaadeldud) tipud;
	komponendid := !komponendid @ [!komponent];
	tekst := "Lisame temaga �hte sidususkomponenti k�ik tipud, millesse saab temast �mberp��ratud kaartega graafis j�uda ega kuulu veel �htegi komponenti.";
	nk1 := string_of_tagurpidi(!sygavutiTipud);
	nk2 := string_of_komponendid(!komponendid);
	if List.for_all (fun t -> !(t.tv) = Vaadeldud || !(t.tv) = Sobimatu) tipud
		then i := Lopp
	else i := EsimeneTipp;;

(* algoritmi l�pp *)
let lopp(tipud, servad) =
	List.iter (fun t -> if !(t.tv) = Vaadeldud then t.tv := Sobimatu) tipud;
	List.iter (fun s -> if !(s.sv) = Vaadeldud then s.sv := Sobimatu) servad;
	pooraServad(servad);
	tekst := "K�ik tipud on vaadeldud. Kosaraju algoritm l�petab, olles leidnud graafi tugevalt sidusad komponendid. Komponendid on esitatud topoloogilises j�rjestuses.";
	nk1 := string_of_komponendid(!komponendid);
	nk2 := "";
	nk3 := ""; 
	AlgoBaas.lopp();;

(* algoritmi samm *)
let samm(tipud, servad) = 
	match !i with
		| Algus -> algus(tipud, servad)
		| Sygavuti -> sygavuti(tipud, servad)
		| PooratudGraaf -> pooratudGraaf(tipud, servad)
		| EsimeneTipp -> esimeneTipp(tipud, servad)
		| TeisedTipud -> teisedTipud(tipud, servad)
		| Lopp -> lopp(tipud, servad)
		| L2bi -> ()
		| _ -> ();;