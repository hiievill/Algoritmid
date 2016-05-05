open Struktuurid;;
open AlgoBaas;;

let sygavutiTipud = ref([]);;
let komponendid = ref([]);; (* kõik sidusad komponendid. TODO: hoopis hulgana teha? *)
let komponent = ref([]);; (* üks sidus komponent *)

(* pööran servad vastupidi, st vahetan tipud ära *)
let rec pooraServad(servad) =
	match servad with
		| [] -> ()
		| x::xs -> (
			match x with
				| {tipp1 = t1; tipp2 = t2;} -> (
					let t = !t1 in 
					x.tipp1 := !t2;
    			x.tipp2 := t;
    			pooraServad(xs)
				)
		);;

(* funktsioon sügavuti lõppjärjestuse sõnena esitamiseks *)
let string_of_lopp(tipud) =
	"Lõppjärjestus: "  ^ string_of_tipud(tipud);;

(* funktsioon tagurpidi sügavuti lõppjärjestuse sõnena esitamiseks *)
let string_of_tagurpidi(tipud) =
	"Tagurpidi lõppjärjestus: "  ^ string_of_tipud(tipud);;		(* NB! mitte List.rev, selle pöörasin juba ringi *)
	
(* funktsioon ühe tugevalt sidusa komponendi sõnena esitamiseks *)
let string_of_komponent(tipud) =
	"Tugevalt sidus komponent: " ^ string_of_tipud(tipud);;

(* funktsioon tugevalt sidusate komponentide sõnena esitamiseks *)
let string_of_komponendid(komponendid) =
	"Tugevalt sidusad komponendid:" ^ List.fold_left  (fun a b -> a ^ " " ^ string_of_tipud b) "" komponendid;;

let algus(tipud, servad) =
	List.iter (fun t -> TopoKahn.uuendaSisendastet (TopoKahn.leiaSisendaste(t, servad)) t) tipud;
	tekst := "Kosaraju algoritm algustab.";
	i := Sygavuti;;

(* läbime graafi sügavuti lõppjärjestuses. Kui mõni tipp jääb läbi käimata, siis mitu korda *)
let sygavuti(algtipp, tipud, servad) =
	sygavutiTipud := TopoLopp.l2biSygavuti(algtipp, tipud, servad);				(* läbime graafi sügavuti lõppjärjestuses *)
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

let esimeneTipp(tipud, servad) =
	List.iter (fun t -> if !(t.tv) = Vaadeldud then t.tv := Sobimatu) tipud;
	List.iter (fun s -> if !(s.sv) = Vaadeldud then s.sv := Sobimatu) servad;
	let esimeneTipp = List.find (fun t -> !(t.tv) = Vaatlemata) !sygavutiTipud in
	esimeneTipp.tv := Valitud;
	tekst := "Valime tagurpidi lõppjärjestusest esimese tipu, mis pole veel üheski komponendis.";
	nk1 := string_of_tagurpidi(!sygavutiTipud);
	nk2 := string_of_komponent(!komponent);
	nk3 := string_of_komponendid(!komponendid);
	i := TeisedTipud;;

let teisedTipud(tipud, servad) =
	let esimeneTipp = List.find (fun t -> !(t.tv) = Valitud) tipud in
	i := Algus;
	while !algoL2bi = false
		do
			Laiuti.laiuti(esimeneTipp, tipud, servad)	(* võiks ka sügavuti, aga pole vahet *)
		done;
	algoL2bi := false;
	List.iter (fun t -> if !(t.tv) = Valitud then t.tv := Vaadeldud) tipud;
	List.iter (fun s -> if !(s.sv) = Valitud then s.sv := Vaadeldud) servad;
	komponent := List.filter (fun t -> !(t.tv) = Vaadeldud) tipud;
	komponendid := !komponendid @ [!komponent];
	tekst := "Lisame temaga ühte sidususkomponenti kõik tipud, millesse saab temast ümberpööratud kaartega graafis jõuda.";
	nk2 := string_of_komponent(!komponent);
	nk3 := string_of_komponendid(!komponendid);
	if List.for_all (fun t -> !(t.tv) = Vaadeldud || !(t.tv) = Sobimatu) tipud
		then i := Lopp
	else i := EsimeneTipp;;

let lopp(tipud, servad) =
	List.iter (fun t -> if !(t.tv) = Vaadeldud then t.tv := Sobimatu) tipud;
	List.iter (fun s -> if !(s.sv) = Vaadeldud then s.sv := Sobimatu) servad;
	tekst := "Kõik tipud on vaadeldud. Kosaraju algoritm lõpetab, olles leidnud graafi tugevalt sidusad komponendid.";
	nk1 := string_of_komponendid(!komponendid);
	nk2 := "";
	nk3 := ""; 
	AlgoBaas.lopp();;

let kosaraju(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus(tipud, servad)
		| Sygavuti -> sygavuti(algtipp, tipud, servad)
		| PooratudGraaf -> pooratudGraaf(tipud, servad)
		| EsimeneTipp -> esimeneTipp(tipud, servad)
		| TeisedTipud -> teisedTipud(tipud, servad)
		| Lopp -> lopp(tipud, servad)
		| L2bi -> ()
		| _ -> ();;