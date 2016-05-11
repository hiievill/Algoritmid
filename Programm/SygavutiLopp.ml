(* moodul SygavutiLopp teostab sammsammulist graafi s�gavuti l�ppj�rjestuses l�bimist *) 

open Struktuurid;;
open Laiuti;;
open AlgoBaas;;

let vaadeldavadTipud = ref([]);;									(* k�lastatud, aga t��tlemata tipud *)

let toodeldudTipud = ref([]);;										(* t��deldud tipud *)

let j2rgmisedTipud : tipp list ref = ref([]);;		(* j�rgmised tipud, mida k�lastatakse *)

(* funktsioon, mis tagastab, kas antud tipust v�ljub vastav serv ning kas see serv ja teine tipp on vaatlemata *)
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

(* funktsioon, mis v�tab argumendiks vaadeldud tipu ja tagastab k�ik temast v�ljuvad vaatlemata servad *)
let leiaVaatlemataServad(tipp, servad) =
	List.filter ((fun t s -> sobiv(t, s)) tipp) servad;;

(* m�rgime serva ja uue tipu vaadeldavaks ja paneme uue tipu vaadeldavate tippude etteotsa *)
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

(* funktsioon, mis m�rgib tipu ja talle vastava serva valituks *)
let valiTipp(tipp, servad) =
	let vastavServ = List.find ((fun t s -> sobiv2(t, s)) tipp) servad in
	vastavServ.sv := Valitud;
	tipp.tv := Valitud;;
	
(* funktsioon, mis m�rgib tipu ja talle vastava serva vaadelduks *)
let lisaTipp(tipp, servad) =
	tipp.tv := Vaadeldud;
	List.iter (fun s -> if !(s.sv) = Valitud then s.sv := Vaadeldud) servad;;

(* funktsioon t��tlusj�rjekorra s�nena esitamiseks *)
let string_of_vaadeldavadTipud(tipud) = 
	"T��tlusmagasin: " ^ string_of_tipud(tipud);;

(* funktsioon nimekirjade (t��deldud tipud ja t��tlusj�rjekord) uuendamiseks *)
let lisatekst() =
	nk1 := string_of_toodeldudTipud(!toodeldudTipud);
	nk2 := string_of_vaadeldavadTipud(!vaadeldavadTipud);
	(*nk3 := Laiuti.string_of_j2rgmisedTipud(!j2rgmisedServad);;*) (* TODO *)
	nk3 := Laiuti.string_of_j2rgmisedServad(!j2rgmisedServad);;

(* algoritmi algus *)
let algus() =
	tekst := "S�gavuti l�ppj�rjestuses l�bimise algoritm alustab valitud tipust.";
	i := EsimeneTipp;;

(* algustipu vaatlemine ja j�rgmiste servade leidmine *)
let esimeneTipp(algtipp, servad) =
	vaadeldavadTipud := [algtipp];															(* lisame tipu t��tlusj�rjekorda *)
	algtipp.tv := Vaadeldav;																		(* m�rgime tipu vaadeldavaks *)
	let vs = leiaVaatlemataServad(algtipp, servad) in 					(*sellest tipust v�ljuvad vaatlemata servad*)
	j2rgmisedServad := vs @ !j2rgmisedServad;
	tekst := "Vaatleme esimest tippu ja lisame selle t��tlusmagasini. Lisame j�rgmiste tippude magasini need t��tlemata tipud, kuhu �sja vaadeldud tipust serv viib.";
	lisatekst();
	if List.length vs = 0																				(* kui �htegi vaatlemata serva ei v�lju, siis l�hme t��tlema *) 
		then i := ServaValik
	else i := ServaVaatlus;;																		(* kui aga v�ljub, siis l�hme j�rgmist serva vaatlema *)

(* serva ja tema vaatlemata tipu vaatlemine ja j�rgmiste servade leidmine *)
let servaVaatlus(servad) =
	let eelmineTipp = List.hd !vaadeldavadTipud in							(* eelmine tipp *)
	let vs = leiaVaatlemataServad(eelmineTipp, servad) in 			(* sellest tipust v�ljuvad vaatlemata servad *)
	let j2rgmineServ = List.hd vs in														(* valime neist esimese *)
	vaatleServa(j2rgmineServ);																	(* m�rgime serva ja vastava tipu vaadeldavaks *)
	let vaadeldavTipp = List.hd !vaadeldavadTipud in						(* vastav vaadeldav tipp *)
	let vs = leiaVaatlemataServad(vaadeldavTipp, servad) in			(* sellest tipust v�ljuvad vaatlemata servad *)
	j2rgmisedServad := List.tl !j2rgmisedServad;
	j2rgmisedServad := vs @ !j2rgmisedServad;
	tekst := "Vaatleme j�rgmiste tippude magasini pealmist tippu, eemaldame selle magasinist ja lisame t��tlusmagasini. Lisame j�rgmiste tippude magasini need t��tlemata tipud, kuhu �sja vaadeldud tipust serv viib.";
	lisatekst();
	if List.length vs = 0																				(* kui �htegi vaatlemata serva ei v�lju, siis l�hme t��tlema *)
		then i := ServaValik
	else i := ServaVaatlus;;																		(* kui aga v�ljub, siis l�hme j�rgmist serva vaatlema *)

(* tipu ja talle vastava serva valimine *)
let servaValik(algtipp, servad) =
	let t = List.hd !vaadeldavadTipud in												(* vaadeldav tipp, mida valima hakkame *)
	if t = algtipp
		then t.tv := Valitud
	else valiTipp(t, servad);
	vaadeldavadTipud := List.tl !vaadeldavadTipud;
	tekst := "K�ik tipud, kuhu sellest tipust edasi p��seb, on t��deldud, nii et valime selle.";
	lisatekst();
	i := ServaLisamine;;

(* tipu ja talle vastava serva vaadelduks m�rkimine *)
let servaLisamine(algtipp, tipud, servad) =
	let t = List.find (fun t -> !(t.tv) = Valitud) tipud in
	toodeldudTipud := !toodeldudTipud @ [t];
	lisaTipp(t, servad);
	tekst := "T��tleme selle tipu ja p��rdume tagasi.";
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

(* algoritmi l�pp *)
let lopp() =
	tekst := "Magasin on t�hi. S�gavuti l�ppj�rjestuses l�bimise algoritm l�petab, olles leidnud s�gavuti l�ppj�rjestuses l�bimise puu.";
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
	