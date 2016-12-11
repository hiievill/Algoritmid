(* moodul Kosaraju teostab sammsammulist Kosaraju algoritmi läbimängu *) 

open Struktuurid;;
open AlgoBaas;;

let sygavutiTipud = ref([]);;
let komponendid = ref([]);; 		(* kõik sidusad komponendid. TODO: hoopis hulgana teha? *)
let komponent = ref([]);; 			(* üks sidus komponent *)

(* pööran servad vastupidi, st vahetan tipud ära *)
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


(* funktsioon sügavuti lõppjärjestuse sõnena esitamiseks *)
let string_of_lopp(tipud) =
	"Lõppjärjestus: "  ^ string_of_tipud(tipud);;

(* funktsioon tagurpidi sügavuti lõppjärjestuse sõnena esitamiseks *)
let string_of_tagurpidi(tipud) =
	"Tagurpidi lõppjärjestus: "  ^ string_of_tipud(tipud);;		(* NB! mitte List.rev, selle pöörasin juba ringi *)

(* funktsioon tugevalt sidusate komponentide sõnena esitamiseks *)
let string_of_komponendid(komponendid) =
	"Tugevalt sidusad komponendid:" ^ List.fold_left  (fun a b -> a ^ " " ^ string_of_tipud b) "" komponendid;;

let algus(tipud, servad) =
	List.iter (fun t -> TopoKahn.uuendaSisendastet (TopoKahn.leiaSisendaste(t, servad)) t) tipud;
	tekst := "Kosaraju algoritm alustab.";
	i := Sygavuti;;

(* läbime graafi sügavuti lõppjärjestuses. Kui mõni tipp jääb läbi käimata, siis mitu korda *)
let sygavuti(tipud, servad) =
	sygavutiTipud := TopoLopp.l2biSygavuti(tipud, servad);				(* läbime graafi sügavuti lõppjärjestuses *)
	tekst := "Läbime graafi sügavuti lõppjärjestuses ja kirjutame välja tekkinud lõppjärjestuse.";
	nk1 := string_of_lopp(!sygavutiTipud);
	i := PooratudGraaf;;

(* tekitame pööratud kaartega graafi *)
let pooratudGraaf(tipud, servad) =
	tekst := "Tekitame pööratud kaartega graafi. ";
	tekst := !tekst ^ "Pöörame tekkinud järjestuse tagurpidi ja hakkame läbima tippe alates esimesest läbimata tipust tagurpidi järjestuses.";
	sygavutiTipud := List.rev(!sygavutiTipud); (*tippude järjestuse pidi ka ümber pöörama*)
	nk2 := string_of_tagurpidi(!sygavutiTipud);
	pooraServad(servad);
	List.iter (fun s -> s.sv := Vaatlemata) servad;
	List.iter (fun t -> t.tv := Vaatlemata) tipud;
	i := EsimeneTipp;;

(* esimese komponenditu tipu valituks märkimine *)
let esimeneTipp(tipud, servad) =
	List.iter (fun t -> if !(t.tv) = Vaadeldud then t.tv := Sobimatu) tipud;
	List.iter (fun s -> if !(s.sv) = Vaadeldud then s.sv := Sobimatu) servad;
	let esimeneTipp = List.find (fun t -> !(t.tv) = Vaatlemata) !sygavutiTipud in
	esimeneTipp.tv := Valitud;
	tekst := "Valime tagurpidi lõppjärjestusest esimese tipu, mis pole veel üheski komponendis.";
	nk1 := string_of_tagurpidi(!sygavutiTipud);
	nk2 := string_of_komponendid(!komponendid);
	i := TeisedTipud;;

(* valitud tipust alates graafi läbimine ja kõikide läbitud tippude ühendamine esimesega üheks komponendiks *)
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
	tekst := "Lisame temaga ühte sidususkomponenti kõik tipud, millesse saab temast ümberpööratud kaartega graafis jõuda ega kuulu veel ühtegi komponenti.";
	nk1 := string_of_tagurpidi(!sygavutiTipud);
	nk2 := string_of_komponendid(!komponendid);
	if List.for_all (fun t -> !(t.tv) = Vaadeldud || !(t.tv) = Sobimatu) tipud
		then i := Lopp
	else i := EsimeneTipp;;

(* algoritmi lõpp *)
let lopp(tipud, servad) =
	List.iter (fun t -> if !(t.tv) = Vaadeldud then t.tv := Sobimatu) tipud;
	List.iter (fun s -> if !(s.sv) = Vaadeldud then s.sv := Sobimatu) servad;
	pooraServad(servad);
	tekst := "Kõik tipud on vaadeldud. Kosaraju algoritm lõpetab, olles leidnud graafi tugevalt sidusad komponendid. Komponendid on esitatud topoloogilises järjestuses.";
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