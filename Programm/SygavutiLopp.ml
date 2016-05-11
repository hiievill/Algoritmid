(* moodul SygavutiLopp teostab sammsammulist graafi sügavuti lõppjärjestuses läbimist *) 

open Struktuurid;;
open Laiuti;;
open AlgoBaas;;

let vaadeldavadTipud = ref([]);;									(* külastatud, aga töötlemata tipud *)

let toodeldudTipud = ref([]);;										(* töödeldud tipud *)

let j2rgmisedTipud : tipp list ref = ref([]);;		(* järgmised tipud, mida külastatakse *)

(* funktsioon, mis tagastab, kas antud tipust väljub vastav serv ning kas see serv ja teine tipp on vaatlemata *)
let sobiv(tipp, serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; nool = n; sv = v} -> (
			!v = Vaatlemata && (
				match n with
					| true -> !t1 = tipp && !(!t2.tv) = Vaatlemata
					| false -> !t1 = tipp && !(!t2.tv) = Vaatlemata || !t2 = tipp && !(!t1.tv) = Vaatlemata
			)
		);;

(* funktsioon, mis tagastab, kas antud tippu siseneb vastav serv ning kas see serv ja teine tipp on vaadeldavad *)
let sobiv2(tipp, serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; nool = n; sv = v} -> (
			!v = Vaadeldav && (
				match n with
					| true -> !t2 = tipp && !(!t1.tv) = Vaadeldav
					| false -> !t1 = tipp && !(!t2.tv) = Vaadeldav || !t2 = tipp && !(!t1.tv) = Vaadeldav
			)
		);;

(* funktsioon, mis võtab argumendiks vaadeldud tipu ja tagastab kõik temast väljuvad vaatlemata servad *)
let leiaVaatlemataServad(tipp, servad) =
	List.filter ((fun t s -> sobiv(t, s)) tipp) servad;;

(* märgime serva ja uue tipu vaadeldavaks ja paneme uue tipu vaadeldavate tippude etteotsa *)
let vaatleServa(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; sv = v;} -> (
			v := Vaadeldav;
			if !(!t1.tv) = Vaatlemata
				then (
					!t1.tv := Vaadeldav;
					vaadeldavadTipud := [!t1] @ !vaadeldavadTipud;
				)
			else (
				assert (!(!t2.tv) = Vaatlemata);
				!t2.tv := Vaadeldav;
				vaadeldavadTipud := [!t2] @ !vaadeldavadTipud;
			)
		);;

(* funktsioon, mis märgib tipu ja talle vastava serva valituks *)
let valiTipp(tipp, servad) =
	let vastavServ = List.find ((fun t s -> sobiv2(t, s)) tipp) servad in
	vastavServ.sv := Valitud;
	tipp.tv := Valitud;;
	
(* funktsioon, mis märgib tipu ja talle vastava serva vaadelduks *)
let lisaTipp(tipp, servad) =
	tipp.tv := Vaadeldud;
	List.iter (fun s -> if !(s.sv) = Valitud then s.sv := Vaadeldud) servad;;

(* funktsioon töötlusjärjekorra sõnena esitamiseks *)
let string_of_vaadeldavadTipud(tipud) = 
	"Töötlusmagasin: " ^ string_of_tipud(tipud);;

(* funktsioon nimekirjade (töödeldud tipud ja töötlusjärjekord) uuendamiseks *)
let lisatekst() =
	nk1 := string_of_toodeldudTipud(!toodeldudTipud);
	nk2 := string_of_vaadeldavadTipud(!vaadeldavadTipud);
	(*nk3 := Laiuti.string_of_j2rgmisedTipud(!j2rgmisedServad);;*) (* TODO *)
	nk3 := Laiuti.string_of_j2rgmisedServad(!j2rgmisedServad);;

(* algoritmi algus *)
let algus() =
	tekst := "Sügavuti lõppjärjestuses läbimise algoritm alustab valitud tipust.";
	i := EsimeneTipp;;

(* algustipu vaatlemine ja järgmiste servade leidmine *)
let esimeneTipp(algtipp, servad) =
	vaadeldavadTipud := [algtipp];															(* lisame tipu töötlusjärjekorda *)
	algtipp.tv := Vaadeldav;																		(* märgime tipu vaadeldavaks *)
	let vs = leiaVaatlemataServad(algtipp, servad) in 					(*sellest tipust väljuvad vaatlemata servad*)
	j2rgmisedServad := vs @ !j2rgmisedServad;
	tekst := "Vaatleme esimest tippu ja lisame selle töötlusmagasini. Lisame järgmiste tippude magasini need töötlemata tipud, kuhu äsja vaadeldud tipust serv viib.";
	lisatekst();
	if List.length vs = 0																				(* kui ühtegi vaatlemata serva ei välju, siis lähme töötlema *) 
		then i := ServaValik
	else i := ServaVaatlus;;																		(* kui aga väljub, siis lähme järgmist serva vaatlema *)

(* serva ja tema vaatlemata tipu vaatlemine ja järgmiste servade leidmine *)
let servaVaatlus(servad) =
	let eelmineTipp = List.hd !vaadeldavadTipud in							(* eelmine tipp *)
	let vs = leiaVaatlemataServad(eelmineTipp, servad) in 			(* sellest tipust väljuvad vaatlemata servad *)
	let j2rgmineServ = List.hd vs in														(* valime neist esimese *)
	vaatleServa(j2rgmineServ);																	(* märgime serva ja vastava tipu vaadeldavaks *)
	let vaadeldavTipp = List.hd !vaadeldavadTipud in						(* vastav vaadeldav tipp *)
	let vs = leiaVaatlemataServad(vaadeldavTipp, servad) in			(* sellest tipust väljuvad vaatlemata servad *)
	j2rgmisedServad := List.tl !j2rgmisedServad;
	j2rgmisedServad := vs @ !j2rgmisedServad;
	tekst := "Vaatleme järgmiste tippude magasini pealmist tippu, eemaldame selle magasinist ja lisame töötlusmagasini. Lisame järgmiste tippude magasini need töötlemata tipud, kuhu äsja vaadeldud tipust serv viib.";
	lisatekst();
	if List.length vs = 0																				(* kui ühtegi vaatlemata serva ei välju, siis lähme töötlema *)
		then i := ServaValik
	else i := ServaVaatlus;;																		(* kui aga väljub, siis lähme järgmist serva vaatlema *)

(* tipu ja talle vastava serva valimine *)
let servaValik(algtipp, servad) =
	let t = List.hd !vaadeldavadTipud in												(* vaadeldav tipp, mida valima hakkame *)
	if t = algtipp
		then t.tv := Valitud
	else valiTipp(t, servad);
	vaadeldavadTipud := List.tl !vaadeldavadTipud;
	tekst := "Kõik tipud, kuhu sellest tipust edasi pääseb, on töödeldud, nii et valime selle.";
	lisatekst();
	i := ServaLisamine;;

(* tipu ja talle vastava serva vaadelduks märkimine *)
let servaLisamine(algtipp, tipud, servad) =
	let t = List.find (fun t -> !(t.tv) = Valitud) tipud in
	toodeldudTipud := !toodeldudTipud @ [t];
	lisaTipp(t, servad);
	tekst := "Töötleme selle tipu ja pöördume tagasi.";
	lisatekst();
	if t = algtipp
		then
			i := Lopp
	else (
		let eelmineTipp = List.hd !vaadeldavadTipud in
		let vs = leiaVaatlemataServad(eelmineTipp, servad) in
  	if List.length vs = 0
  		then i := ServaValik
  	else i := ServaVaatlus
	);;

(* algoritmi lõpp *)
let lopp() =
	tekst := "Magasin on tühi. Sügavuti lõppjärjestuses läbimise algoritm lõpetab, olles leidnud sügavuti lõppjärjestuses läbimise puu.";
	lisatekst();
	AlgoBaas.lopp();;

(* algoritmi samm *)
let samm(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus();
		| EsimeneTipp -> esimeneTipp(algtipp, servad);
		| ServaVaatlus -> servaVaatlus(servad);
		| ServaValik -> servaValik(algtipp, servad);
		| ServaLisamine -> servaLisamine(algtipp, tipud, servad);
		| Lopp -> lopp();
		| _ -> ();;
	