open Struktuurid;;
open Laiuti;;
open AlgoBaas;;

let vaadeldavadTipud = ref([]);;

let kylastatudTipud = ref([]);;

(* fn, mis tagastab, kas antud tipust väljub vastav serv ning serv ja teine tipp on vaatlemata *)
let sobiv(tipp, serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; nool = n; sv = v} -> (
			!v = Vaatlemata && (
				match n with
					| true -> !t1 = tipp && !(!t2.tv) = Vaatlemata
					| false -> !t1 = tipp && !(!t2.tv) = Vaatlemata || !t2 = tipp && !(!t1.tv) = Vaatlemata
			)
		);;

(* TODO: eelmisega kokku võtta? *)
let sobiv2(tipp, serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; nool = n; sv = v} -> (
			!v = Vaadeldav && (
				match n with
					| true -> !t2 = tipp && !(!t1.tv) = Vaadeldav
					| false -> !t1 = tipp && !(!t2.tv) = Vaadeldav || !t2 = tipp && !(!t1.tv) = Vaadeldav
			)
		);;
	

(* fn, mis võtab argumendiks vaadeldud tipu ja tagastab kõik temast väljuvad vaatlemata servad *)
let leiaVaatlemataServad(tipp, servad) =
	List.filter ((fun t s -> sobiv(t, s)) tipp) servad;;

(* märgime serva ja uue tipu vaadeldavaks ja paneme uue tipu vaadeldavate tippude etteotsa *)
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

(* funktsioon, mis märgib tipu ja talle vastava serva Valituks *)
let valiTipp(tipp, servad) =
	let vastavServ = List.find ((fun t s -> sobiv2(t, s)) tipp) servad in
	(* TODO: kui vastavat serva ei leidu? *)
	vastavServ.sv := Valitud;
	tipp.tv := Valitud;;
	
(* funktsioon, mis märgib tipu ja talle vastava serva Vaadelduks ja kustutab tipu vaadeldavate hulgast *)
let lisaTipp(tipp, servad) =
	assert (List.hd !vaadeldavadTipud = tipp); (* vajalik? *)
	vaadeldavadTipud := List.tl !vaadeldavadTipud;
	tipp.tv := Vaadeldud;
	List.iter (fun s -> if !(s.sv) = Valitud then s.sv := Vaadeldud) servad;;

let algus() =
	tekst := "Sügavuti lõppjärjestuses läbimise algoritm alustab.";
	i := EsimeneTipp;;

let esimeneTipp(algtipp) =
	vaadeldavadTipud := [algtipp];
	algtipp.tv := Vaadeldav;
	tekst := "Vaatleme esimest tippu.";
	i := ServaVaatlus;;

let servaVaatlus(servad) =
	let eelmineTipp = List.hd !vaadeldavadTipud in
	let vs = leiaVaatlemataServad(eelmineTipp, servad) in (*sellest tipust väljuvad vaatlemata servad*)
	if List.length vs = 0
		then (
			tekst := "Sellest tipust enam edasi ei saa.";
			i := ServaValik;
		)
	else (
		let j2rgmineServ = List.hd vs in
		vaatleServa(j2rgmineServ);
		tekst := "Vaatleme järgmist tippu.";
		i := ServaVaatlus;
	);;

let servaValik(algtipp, servad) =
	tekst := "Kõik selle tipu järglased on töödeldud, nii et valime selle.";
	let t = List.hd !vaadeldavadTipud in
	if t = algtipp
		then t.tv := Valitud
	else valiTipp(t, servad);
	i := ServaLisamine;;

let servaLisamine(algtipp, servad) =
	tekst := "Töötleme selle tipu.";
	let t = List.hd !vaadeldavadTipud in
	kylastatudTipud := !kylastatudTipud @ [t];
	if t = algtipp
		then (
			t.tv := Vaadeldud;
			i := Lopp
		)
	else (
		lisaTipp(t, servad);
		let eelmineTipp = List.hd !vaadeldavadTipud in
		let vs = leiaVaatlemataServad(eelmineTipp, servad) in
  	if List.length vs = 0
  		then i := ServaValik
  	else i := ServaVaatlus
	);;

let lopp() =
	(*AlgoBaas.lopp("Algoritm lõpetab.");;*)
	AlgoBaas.lopp("Tippude läbimise järjekord: " ^ string_of_tipud(!kylastatudTipud));;

let sygavutiLopp(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus();
		| EsimeneTipp -> esimeneTipp(algtipp);
		| ServaVaatlus -> servaVaatlus(servad);
		| ServaValik -> servaValik(algtipp, servad);
		| ServaLisamine -> servaLisamine(algtipp, servad);
		| Lopp -> lopp();
		| _ -> ();;
	