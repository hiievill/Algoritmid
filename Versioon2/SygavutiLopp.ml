open Struktuurid;;
open Laiuti;;
open AlgoBaas;;

let vaadeldavadTipud = ref([]);;

let toodeldudTipud = ref([]);;

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
(* TODO: peaks vaatlema ka neid servi, mis viivad juba vaadeldud tippudeni? *)
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
	(* TODO: kui vastavat serva ei leidu? *)
	vastavServ.sv := Valitud;
	tipp.tv := Valitud;;
	
(* funktsioon, mis m�rgib tipu ja talle vastava serva vaadelduks *)
let lisaTipp(tipp, servad) =
	tipp.tv := Vaadeldud;
	List.iter (fun s -> if !(s.sv) = Valitud then s.sv := Vaadeldud) servad;;

(* funktsioon t��tlusj�rjekorra s�nena esitamiseks *)
let string_of_vaadeldavadTipud(tipud) = 
	"T��tlusj�rjekord: " ^ string_of_tipud(tipud);;

(* funktsioon, mis lisab teksti l�ppu t��deldud tipud ja t��tlusj�rjekorra *)
let lisatekst() =
	nk1 := string_of_toodeldudTipud(!toodeldudTipud);
	nk2 := string_of_vaadeldavadTipud(!vaadeldavadTipud);;

let algus() =
	tekst := "S�gavuti l�ppj�rjestuses l�bimise algoritm alustab.";
	i := EsimeneTipp;;

let esimeneTipp(algtipp, servad) =
	vaadeldavadTipud := [algtipp];															(* lisame tipu t��tlusj�rjekorda *)
	algtipp.tv := Vaadeldav;																		(* m�rgime tipu vaadeldavaks *)
	tekst := "Vaatleme esimest tippu ja lisame selle t��tlusj�rjekorda.";
	lisatekst();
	let vs = leiaVaatlemataServad(algtipp, servad) in 					(*sellest tipust v�ljuvad vaatlemata servad*)
	if List.length vs = 0																				(* kui �htegi vaatlemata serva ei v�lju, siis l�hme t��tlema *) 
		then i := ServaValik
	else i := ServaVaatlus;;																		(* kui aga v�ljub, siis l�hme j�rgmist serva vaatlema *)

let servaVaatlus(servad) =
	let eelmineTipp = List.hd !vaadeldavadTipud in							(* eelmine tipp *)
	let vs = leiaVaatlemataServad(eelmineTipp, servad) in 			(* sellest tipust v�ljuvad vaatlemata servad *)
	let j2rgmineServ = List.hd vs in														(* valime neist esimese *)
	vaatleServa(j2rgmineServ);																	(* m�rgime serva ja vastava tipu vaadeldavaks *)
	tekst := "Vaatleme j�rgmist tippu ja lisame selle t��tlusj�rjekorra algusesse.";
	lisatekst();
	let vaadeldavTipp = List.hd !vaadeldavadTipud in						(* vastav vaadeldav tipp *)
	let j2rgvs = leiaVaatlemataServad(vaadeldavTipp, servad) in	(* sellest tipust v�ljuvad vaatlemata servad *)
	if List.length j2rgvs = 0																		(* kui �htegi vaatlemata serva ei v�lju, siis l�hme t��tlema *)
		then i := ServaValik
	else i := ServaVaatlus;;																		(* kui aga v�ljub, siis l�hme j�rgmist serva vaatlema *)

let servaValik(algtipp, servad) =
	let t = List.hd !vaadeldavadTipud in												(* vaadeldav tipp, mida valima hakkame *)
	if t = algtipp
		then t.tv := Valitud
	else valiTipp(t, servad);
	vaadeldavadTipud := List.tl !vaadeldavadTipud;
	tekst := "K�ik j�rgnevad tipud on t��deldud, nii et valime selle."; (* TODO: paremini s�nastada *)
	lisatekst();
	i := ServaLisamine;;

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

let lopp() =
	tekst := "Algoritm l�petab, olles leidnud s�gavuti l�ppj�rjestuses otsingu otsingupuu.";
	lisatekst();
	AlgoBaas.lopp();;

let sygavutiLopp(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus();
		| EsimeneTipp -> esimeneTipp(algtipp, servad);
		| ServaVaatlus -> servaVaatlus(servad);
		| ServaValik -> servaValik(algtipp, servad);
		| ServaLisamine -> servaLisamine(algtipp, tipud, servad);
		| Lopp -> lopp();
		| _ -> ();;
	