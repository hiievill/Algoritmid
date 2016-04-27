open Struktuurid;;
open AlgoBaas;;

(* TODO: �sna palju koodikordust Sygavuti algodega. �kki SygavutiEes ka �he sammuna? *)

let sygavutiTipud = ref([]);;
let j2rgmisedServad = ref([]);;
let komponendid = ref([]);; (* k�ik sidusad komponendid. TODO: hoopis hulgana teha? *)
let komponent = ref([]);; (* �ks sidus komponent *)

(* p��ran servad vastupidi, st vahetan tipud �ra *)
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

(* funktsioon s�gavuti l�ppj�rjestuse s�nena esitamiseks *)
let string_of_lopp(tipud) =
	"L�ppj�rjestus: ["  ^ string_of_tipud(tipud) ^ "]";;

(* funktsioon tagurpidi s�gavuti l�ppj�rjestuse s�nena esitamiseks *)
let string_of_tagurpidi(tipud) =
	"Tagurpidi l�ppj�rjestus: ["  ^ string_of_tipud(tipud) ^ "]";;		(* NB! mitte List.rev, selle p��rasin juba ringi *)
	
(* funktsioon �he tugevalt sidusa komponendi s�nena esitamiseks *)
let string_of_komponent(tipud) =
	"Tugevalt sidus komponent: [" ^ string_of_tipud(tipud) ^ "]";;

(* funktsioon tugevalt sidusate komponentide s�nena esitamiseks *)
let string_of_komponendid(komponendid) =
	"Tugevalt sidusad komponendid:" ^ List.fold_left  (fun a b -> a ^ "\n" ^ string_of_komponent b) "" komponendid;;

let algus(tipud, servad) =
	List.iter (fun t -> TopoKahn.uuendaSisendastet (TopoKahn.leiaSisendaste(t, servad)) t) tipud;
	tekst := "Kosaraju algoritmi algustab.";
	i := Sygavuti;;

(* l�bime graafi s�gavuti l�ppj�rjestuses. Kui m�ni tipp j��b l�bi k�imata, siis mitu korda *)
let sygavuti(algtipp, tipud, servad) =
	let esimeneTipp = ref(algtipp) in
	while List.exists (fun t -> !(t.tv) = Vaatlemata) tipud (* kuniks leidub veel vaatlemata tippe *)
		do
			i := Algus;
			(* l�bime graafi s�gavuti l�ppj�rjestuses alates tipust esimeneTipp *)
			algoL2bi := false;
			SygavutiLopp.toodeldudTipud := [];
			while !algoL2bi = false
				do
					SygavutiLopp.sygavutiLopp(!esimeneTipp, tipud, servad)		(*l�bime s�gavuti*)
				done;
  		sygavutiTipud := !sygavutiTipud @ !(SygavutiLopp.toodeldudTipud); (*lisame j�rjestusse*)
  		if List.exists (fun t -> !(t.tv) = Vaatlemata) tipud (* kui leidub veel vaatlemata tippe *)
  			then esimeneTipp := TopoKahn.valiTipp(tipud) (* m��rame uue algtipu (sisendastmega 0) *)
  	done;
	algoL2bi := false;
	tekst := "L�bime graafi s�gavuti l�ppj�rjestuses ja kirjutame v�lja tekkinud l�ppj�rjestuse.";
	tekst := !tekst ^ "\n" ^ string_of_lopp(!sygavutiTipud);
	i := PooratudGraaf;;

(* tekitame p��ratud kaartega graafi *)
let pooratudGraaf(tipud, servad) =
	tekst := "Tekitame p��ratud kaartega graafi.";
	tekst := !tekst ^ "\n" ^ "P��rame tekkinud j�rjestuse tagurpidi ja hakkame l�bima tippe alates esimesest l�bimata tipust selles j�rjestuses.";
	tekst := !tekst ^ "\n" ^ string_of_lopp(!sygavutiTipud);
	sygavutiTipud := List.rev(!sygavutiTipud); (*tippude j�rjestuse pidi ka �mber p��rama*)
	tekst := !tekst ^ "\n" ^ string_of_tagurpidi(!sygavutiTipud);
	pooraServad(servad);
	List.iter (fun s -> s.sv := Vaatlemata) servad;
	List.iter (fun t -> t.tv := Vaatlemata) tipud;
	i := EsimeneTipp;;

let esimeneTipp(servad) =
	let esimeneTipp = List.hd !sygavutiTipud in
	esimeneTipp.tv := Valitud;
	let js = Laiuti.leiaJ2rgServad(esimeneTipp, servad) in
	j2rgmisedServad := (!j2rgmisedServad) @ js;
	tekst := "Valime j�rjestusest esimese tipu.";
	tekst := !tekst ^ "\n" ^ string_of_tagurpidi(!sygavutiTipud);
	tekst := !tekst ^ "\n" ^ string_of_komponent(!komponent);
	i := EsimeseTipuLisamine;;

let esimeseTipuLisamine() =
	let esimeneTipp = List.hd !sygavutiTipud in
	esimeneTipp.tv := Vaadeldud;
	komponent := [esimeneTipp];
	sygavutiTipud := List.filter (fun t -> t.nimi <> esimeneTipp.nimi) !sygavutiTipud;
	tekst := "Lisame selle tipu tugevalt sidusasse komponenti, m�rgime tipu vaadelduks ja kustutame j�rjestusest.";
	tekst := !tekst ^ "\n" ^ string_of_komponent(!komponent);
	tekst := !tekst ^ "\n" ^ string_of_tagurpidi(!sygavutiTipud);
	i := ServaValik;;

(* NB! sarnane kui SygavutiEes.servaValik, TODO: kokku v�tta *)
let servaValik(servad) =
	if List.length !j2rgmisedServad = 0
		then (
			tekst := "Edasi ei p��se kuhugi, seni l�bitud tipud moodustavad �he tugevalt sidusa komponendi.";
			tekst := !tekst ^ "\n" ^ string_of_komponent(!komponent);
			tekst := !tekst ^ "\n" ^ string_of_tagurpidi(!sygavutiTipud);
			komponendid := !komponendid @ [!komponent]; (*lisame sidusa komponendi komponentide hulka *)
			komponent := [];
			if List.length !sygavutiTipud > 0 (*sama kui et k�ik pole vaadeldud *)
				then i := EsimeneTipp
			else i := Lopp (*K�ik tipud said vaadeldud*)
		)
	else (
		let s = List.hd !j2rgmisedServad in
  	j2rgmisedServad := List.tl !j2rgmisedServad;
  	match s with	(* TODO: see on kuskil mujal algodes ka olemas *)
  		| {tipp1 = t1; tipp2 = t2; sv = v;} -> (
  			v := Valitud;
  			if !((!t1).tv) = Vaatlemata then (!t1).tv := Valitud;
  			if !((!t2).tv) = Vaatlemata then (!t2).tv := Valitud; 
  		);
  	let lisatavServ = List.find (fun s -> !(s.sv) = Valitud) servad in
  	let lisatavTipp = Laiuti.leiaLisatavTipp(lisatavServ) in
  	let js = Laiuti.leiaJ2rgServad(lisatavTipp, servad) in
  	j2rgmisedServad := js @ (!j2rgmisedServad);
  	j2rgmisedServad := Laiuti.eemalda(!j2rgmisedServad);
  	tekst := "Valime j�rgmise vaatlemata tipu.";
		tekst := !tekst ^ "\n" ^ string_of_komponent(!komponent);
		tekst := !tekst ^ "\n" ^ string_of_tagurpidi(!sygavutiTipud);
  	i := ServaLisamine
	);;

let servaLisamine(tipud, servad) =
	let lisatavServ = List.find (fun s -> !(s.sv) = Valitud) servad in
	let lisatavTipp = Laiuti.leiaLisatavTipp(lisatavServ) in
	komponent := !komponent @ [lisatavTipp];
	Laiuti.lisaServ(lisatavServ);
	sygavutiTipud := List.filter (fun t -> t.nimi <> lisatavTipp.nimi) !sygavutiTipud;
	tekst := "Lisame selle tipu tugevalt sidusasse komponenti, m�rgime tipu vaadelduks ja kustutame j�rjestusest.";
	tekst := !tekst ^ "\n" ^ string_of_komponent(!komponent);
	tekst := !tekst ^ "\n" ^ string_of_tagurpidi(!sygavutiTipud);
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud
		then i := Lopp
	else i := ServaValik;;

let lopp() =
	tekst := "Kosaraju algoritm l�petab, olles leidnud graafi tugevalt sidusad komponendid.";
	tekst := !tekst ^ "\n" ^ string_of_komponendid(!komponendid); 
	AlgoBaas.lopp();;
	(*TODO: komponendid v�lja printida. Siin ja mujal (igal tipulisamisel nimekiri) *)

let kosaraju(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus(tipud, servad)
		| Sygavuti -> sygavuti(algtipp, tipud, servad)
		| EsimeneTipp -> esimeneTipp(servad)
		| EsimeseTipuLisamine -> esimeseTipuLisamine()
		| PooratudGraaf -> pooratudGraaf(tipud, servad)
		| ServaValik -> servaValik(servad)
		| ServaLisamine -> servaLisamine(tipud, servad)
		| Lopp -> lopp()
		| L2bi -> ()
		| _ -> ();;