open Struktuurid;;
open AlgoBaas;;

(* TODO: üsna palju koodikordust Sygavuti algodega. Äkki SygavutiEes ka ühe sammuna? *)

let sygavutiTipud = ref([]);;
let j2rgmisedServad = ref([]);;
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

let algus(tipud, servad) =
	List.iter (fun t -> TopoKahn.uuendaSisendastet (TopoKahn.leiaSisendaste(t, servad)) t) tipud;
	tekst := "Kosaraju algoritmi algustab.";
	i := Sygavuti;;

(* läbime graafi sügavuti lõppjärjestuses. Kui mõni tipp jääb läbi käimata, siis mitu korda *)
let sygavuti(algtipp, tipud, servad) =
	let esimeneTipp = ref(algtipp) in
	while List.exists (fun t -> !(t.tv) = Vaatlemata) tipud (* kuniks leidub veel vaatlemata tippe *)
  	do
			i := Algus;
			(* läbime graafi sügavuti lõppjärjestuses alates tipust esimeneTipp *)
			while !algoL2bi = false
				do
					SygavutiLopp.sygavutiLopp(!esimeneTipp, tipud, servad)		(*läbime sügavuti*)
				done;
  		sygavutiTipud := !sygavutiTipud @ !(SygavutiLopp.kylastatudTipud); (*lisame järjestusse*)
  		if List.exists (fun t -> !(t.tv) = Vaatlemata) tipud (* kui leidub veel vaatlemata tippe *)
  			then esimeneTipp := TopoKahn.valiTipp(tipud) (* määrame uue algtipu (sisendastmega 0) *)
  	done;
	algoL2bi := false;
	tekst := "Läbime graafi sügavuti lõppjärjestuses ja kirjutame välja tekkinud lõppjärjestuse.";
	i := PooratudGraaf;;

(* tekitame pööratud kaartega graafi *)
let pooratudGraaf(tipud, servad) =
	tekst := "Tekitame pööratud kaartega graafi.";
	tekst := "Läbime tippe alates esimesest läbimata tipust tekkinud järjestuses.";
	pooraServad(servad);
	List.iter (fun s -> s.sv := Vaatlemata) servad;
	List.iter (fun t -> t.tv := Vaatlemata) tipud;
	i := EsimeneTipp;;

let esimeneTipp(servad) =
	let esimeneTipp = List.hd !sygavutiTipud in
	esimeneTipp.tv := Vaadeldud;
	komponent := [esimeneTipp];
	let js = Laiuti.leiaJ2rgServad(esimeneTipp, servad) in
	j2rgmisedServad := (!j2rgmisedServad) @ js;
	tekst := "Märgime esimese tipu külastatuks ja kustutame nimekirjast.";
	(* tekst := sygavutiTipud nimekiri. TODO: või kuvada kustutatuid lihtsalt teise värviga? *)
	sygavutiTipud := List.filter (fun t -> t.nimi <> esimeneTipp.nimi) !sygavutiTipud;
	i := ServaValik;;

(* NB! sarnane kui SygavutiEes.servaValik, TODO: kokku võtta *)
let servaValik(servad) =
	if List.length !j2rgmisedServad = 0
		then (
			tekst := "Edasi ei pääse kuhugi, seni läbitud tipud moodustavad ühe sidusa komponendi.";
			tekst := "Tugevalt sidus komponent: " ^ string_of_tipud(!komponent);
			komponendid := !komponendid @ [komponent]; (*lisame sidusa komponendi komponentide hulka *)
			komponent := [];
			if List.length !sygavutiTipud > 0 (*sama kui et kõik pole vaadeldud *)
				then (
					(*tekst := "Valime nimekirjast järgmise külastamata tipu.";*)
					i := EsimeneTipp
				)
			else (
				(*Kõik tipud said vaadeldud*)
				i := Lopp
			)
		)
	else (
		let s = List.hd !j2rgmisedServad in
  	j2rgmisedServad := List.tl !j2rgmisedServad;
  	match s with
  		| {tipp1 = t1; tipp2 = t2; sv = v;} -> (
  			v := Valitud;
  			if !((!t1).tv) = Vaatlemata then (!t1).tv := Valitud;
  			if !((!t2).tv) = Vaatlemata then (!t2).tv := Valitud; 
  		);
  	let lisatavServ = List.find (fun s -> !(s.sv) = Valitud) servad in
  	let lisatavTipp = Laiuti.leiaLisatavTipp(lisatavServ) in
  	komponent := !komponent @ [lisatavTipp];
		sygavutiTipud := List.filter (fun t -> t.nimi <> lisatavTipp.nimi) !sygavutiTipud;
  	let js = Laiuti.leiaJ2rgServad(lisatavTipp, servad) in
  	j2rgmisedServad := js @ (!j2rgmisedServad);
  	j2rgmisedServad := Laiuti.eemalda(!j2rgmisedServad);
  	tekst := "Valime järgmise tipu.";
  	i := ServaLisamine
	);;

let servaLisamine(tipud, servad) =
	let lisatavServ = List.find (fun s -> !(s.sv) = Valitud) servad in
	Laiuti.lisaServ(lisatavServ);
	tekst := "Märgime selle tipu külastatuks ja kustutame nimekirjast.";
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud
		then i := Lopp
	else i := ServaValik;;

let lopp() =
	AlgoBaas.lopp("Kosaraju algoritm lõpetab, olles leidnud graafi tugevalt sidusad komponendid.");;
	(*TODO: komponendid välja printida. Siin ja mujal (igal tipulisamisel nimekiri) *)

let kosaraju(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus(tipud, servad)
		| Sygavuti -> sygavuti(algtipp, tipud, servad)
		| EsimeneTipp -> esimeneTipp(servad)
		| PooratudGraaf -> pooratudGraaf(tipud, servad)
		| ServaValik -> servaValik(servad)
		| ServaLisamine -> servaLisamine(tipud, servad)
		| Lopp -> lopp()
		| L2bi -> ()
		| _ -> ();;