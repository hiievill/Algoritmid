(* moodul Programm vastutab programmi põhilise toimimise eest: siit kutsutakse välja algoritmide sammud, siin käsitletakse *)
(* hiire- ja klahvisndmustele reageerimist, siin teostatakse graafikontrolli ja salidide genereerimist. *)

open Graphics;;
open Struktuurid;;
open Graafika;;
open AlgoBaas;;
open NtGraafid;;

let liigutatavTipp = ref(None);;
let liigutatavServ = ref(None);;

(* funktsioon, mis tagastab, kas arv a asub b1 ja b2 vahel. Täpsusega 5. *)
let asubVahel(a, b1, b2) =
	a <= (max b1 b2) + 5 && a >= (min b1 b2) - 5;; 

(* funktsioon, mis tagastab, kas punkt (x, y) asub sirgel y=ax+c *)
let asubSirgel(x, y, a, c) =
	abs_float (a *. x +. c -. y) <= 1.;;		(* <= 1, sest vahel teen floate intideks, seega täpne ei pruugiks sobida *)	
	
(* funktsioon, mis tagastab, kas punkt (x, y) asub sirgest y=ax+c mitte enam kui 5 ühiku kaugusel *)
let asubUmbesSirgel(x, y, a, c) =
	abs_float (a *. x +. c -. y) <= 5.;;

(* funktsioon, mis tagastab, kas punktis (hx, hy) asuv hiir asub sirgel, mis ühendab tippe t1 ja t2 *)
let hiirAsubSirgel(t1, t2, hx, hy)=
	if !(t1.x) = !(t2.x) (* horisontaalne sirge *)
		then abs (hx - !(t1.x)) <= 5 && asubVahel(hy, !(t1.y), !(t2.y))
	else (
		if !(t1.y) = !(t2.y) (* horisontaalne sirge *)
			then abs (hy - !(t1.y)) <= 5 && asubVahel(hx, !(t1.x), !(t2.x))
		else ( (* tavaline sirge *)
			let (a, c) = leiaSirge(float_of_int(!(t1.x)), float_of_int(!(t1.y)), float_of_int(!(t2.x)), float_of_int(!(t2.y))) in 
			asubUmbesSirgel(float_of_int(hx), float_of_int(hy), a, c) &&
			asubVahel(hx, !(t1.x), !(t2.x)) && asubVahel(hy, !(t1.y), !(t2.y)) 
		)
	);;

(* funktsioon, mis tagastab, kas hiir asub tipu peal *)
let hiirTipul hx hy tipp =
	match tipp with
		| {nimi = n; x = tx; y = ty} -> sqrt ((float_of_int(hx - !tx)) ** 2. +. (float_of_int(hy - !ty)) ** 2.) <= float_of_int(tipuRaadius);;

(* funktsioon, mis tagastab, kas hiir asub serva peal (+/- 5) *)
let hiirServal hx hy serv =
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> (
			let nimi = !t1.nimi ^ ":" ^ !t2.nimi in
			let r = Hashtbl.find kaareR nimi in
			if r = 0 (* sirge *)
				then hiirAsubSirgel(!t1, !t2, hx, hy)
			else ( (* kaar *)
  			let x = Hashtbl.find kaareX nimi in
  			let y = Hashtbl.find kaareY nimi in
				(* TODO: järgneva || asemel saaks täpsemini kontrollida, et tühjast kohast haarates "servale" ei satuks *)
  			(asubVahel(hx, !(!t1.x), !(!t2.x)) || (* hiir on horisontaalselt 2 tipu vahel *)
  			asubVahel(hy, !(!t1.y), !(!t2.y))) && (* hiir on vertikaalselt 2 tipu vahel *)
  			(* hiire kaugus ringjoone keskpunktist on raadius +/- 5 *) 
  			abs_float (sqrt ((float_of_int(hx - x)) ** 2. +. (float_of_int(hy - y)) ** 2.) -. float_of_int(r)) <= 5.
			)		
		);;

(* funktsioon, mis tagastab sirgete y=a1x+c2 ja y=a2x+c2 lõikumispunkti (e, f) *)
let leiaYhinePunkt(a1, c1, a2, c2) =
	if a1 = a2
		then failwith("Paralleelsed sirged.")
	else (
		let e = (c2 -. c1) /. (a1 -. a2) in
		let f = a1 *. e +. c1 in
		(e, f) 
	);;

(* funktsiooni liigutaServa jätk koodikorduse vältimiseks *)
let liigutaServaJ2tk(hx, hy, e, f, keskX, keskY, t1, t2) =
	let r = sqrt ((float_of_int(hx) -. e) ** 2. +. (float_of_int(hy) -. f) ** 2.) in
	let k = r -. leiaJoonePikkus(e, f, keskX, keskY) in		(* k = r - ringjoone keskpunkti kaugus serva keskpunktist *)
	uuendaKaareAndmeid(!t1, !t2, int_of_float e, int_of_float f, int_of_float r, int_of_float k);;

(* funktsioon, mis määrab serva ringjoonele uued parameetrid, sõltuvalt sellest, kus hiir on *)
let liigutaServa(hx, hy) =
	match !liigutatavServ with
		| None -> ()
		| Some s -> (
			match s with
				| {tipp1 = t1; tipp2 = t2;} -> (
					if hiirAsubSirgel(!t1, !t2, hx, hy)
						then uuendaKaareAndmeid(!t1, !t2, 0, 0, 0, 0)
					else (
						let keskX = float_of_int(!(!t1.x) + !(!t2.x)) /. 2. in
  					let keskY = float_of_int(!(!t1.y) + !(!t2.y)) /. 2. in
						let jp1 = leiaJoonePikkus(keskX, keskY, float_of_int(hx), float_of_int(hy)) in							(* hiire kaugus serva keskptist *)
						let jp2 = leiaJoonePikkus(keskX, keskY, float_of_int(!(!t1.x)), float_of_int(!(!t1.y))) in	(* pool servapikkust *)
						(* kui hiire kaugus serva keskpunktist on väiksem kui pool servapikkust, siis muudame *)
						if jp1 < jp2 then (
  						if !(!t1.x) = !(!t2.x)
  							then (
  								let (a3, c3) = leiaSirge(float_of_int(!(!t1.x)), float_of_int(!(!t1.y)), float_of_int(hx), float_of_int(hy)) in
									let kX = float_of_int(!(!t1.x) + hx) /. 2. in
									let kY = float_of_int(!(!t1.y) + hy) /. 2. in
									let (a4, c4) = leiaRistuvSirge(a3, c3, kX, kY) in
									let x = (keskY -. c4) /. a4 in
									let (e, f) = (x, keskY) in
									liigutaServaJ2tk(hx, hy, e, f, keskX, keskY, t1, t2)
  							)
  						else if !(!t1.y) = !(!t2.y)
  							then (
  								let (a3, c3) = leiaSirge(float_of_int(!(!t1.x)), float_of_int(!(!t1.y)), float_of_int(hx), float_of_int(hy)) in
  								let kX = float_of_int(!(!t1.x) + hx) /. 2. in
									let kY = float_of_int(!(!t1.y) + hy) /. 2. in
									let (a4, c4) = leiaRistuvSirge(a3, c3, kX, kY) in
									let y = a4 *. keskX +. c4 in
  								let (e, f) = (keskX, y) in
									liigutaServaJ2tk(hx, hy, e, f, keskX, keskY, t1, t2)
  							)
  						else (
    						let (a1, c1) = leiaSirge(float_of_int(!(!t1.x)), float_of_int(!(!t1.y)), float_of_int(!(!t2.x)), float_of_int(!(!t2.y))) in
      					let (a2, c2) = leiaRistuvSirge(a1, c1, keskX, keskY) in
      					let (a3, c3) = leiaSirge(float_of_int(!(!t1.x)), float_of_int(!(!t1.y)), float_of_int(hx), float_of_int(hy)) in
      					let (a4, c4) = leiaRistuvSirge(a3, c3, float_of_int(!(!t1.x) + hx) /. 2., float_of_int(!(!t1.y) + hy) /. 2.) in
      					let (e, f) = leiaYhinePunkt(a2, c2, a4, c4) in
      					liigutaServaJ2tk(hx, hy, e, f, keskX, keskY, t1, t2)
							)
						)
					)
				)
		);;

(* funktsioon, mis uuendab ringjoone võrrandit, jättes kauguse tippudevahelise lõigu keskpunktist samaks,*)
(* välja arvatud siis, kui tippe lähemale nihutatakse *)
let uuendaKaart(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> (
			let nimi = !t1.nimi ^ ":" ^ !t2.nimi in
			if Hashtbl.find kaareR nimi <> 0 then (
      	let tippudeVahelineKaugus = leiaJoonePikkus(float_of_int(!(!t1.x)), float_of_int(!(!t1.y)), float_of_int(!(!t2.x)), float_of_int(!(!t2.y))) in
  			if float_of_int(Hashtbl.find kaareK nimi) >= tippudeVahelineKaugus /. 2.
      		then Hashtbl.replace kaareK nimi (int_of_float(tippudeVahelineKaugus /. 2.) - 1);
  			let keskX = float_of_int(!(!t1.x) + !(!t2.x)) /. 2. in
    		let keskY = float_of_int(!(!t1.y) + !(!t2.y)) /. 2. in
  			let (a, c) = leiaSirge(float_of_int(!(!t1.x)), float_of_int(!(!t1.y)), float_of_int(!(!t2.x)), float_of_int(!(!t2.y))) in
				let (a2, c2) = leiaRistuvSirge(a, c, keskX, keskY) in
  			let ((e1, f1), (e2, f2)) = kaksPunkti(keskX, keskY, a2, c2, float_of_int(Hashtbl.find kaareK nimi)) in
  			let senineX = float_of_int(Hashtbl.find kaareX nimi) in
  			let senineY = float_of_int(Hashtbl.find kaareY nimi) in
  			(* leiame selle keskpunkti, mis on senisest ringjoone keskpunktist kaugemal *)
  			let (e, f) = if leiaJoonePikkus(senineX, senineY, e1, f1) >= leiaJoonePikkus(senineX, senineY, e2, f2) then (e1, f1) else (e2, f2) in
  			liigutatavServ := Some serv;
				liigutaServa(int_of_float(e), int_of_float(f))
			)		
		);;

(* funktsioon tipu liigutamiseks *)
let liigutaTippu(hx, hy, servad) =
	match !liigutatavTipp with
		| Some t -> (
			t.x := hx;
			t.y := hy;
			List.iter (fun s -> if !(s.tipp1) = t || !(s.tipp2) = t then uuendaKaart(s)) servad;
		)
		| _ -> ();;


(* kõik läbitud sammud salvestatud kujul *)
let seisundid = Hashtbl.create 20;;

(* sammude arv *)
let sammudKokku = ref(None);;

(* tegu on läbitud sammuga - kuvame salvestatud seisundi *)
let kuvaSeisund(tipud, servad) = 
	let seisund = Hashtbl.find seisundid !sammuNr in
	List.iter (fun t -> t.tv := Hashtbl.find (seisund.tipuvaadeldavused) t.nimi) tipud;
	List.iter (fun t -> t.hind := Hashtbl.find (seisund.hinnad) t.nimi) tipud;
	if !algo = Kosaraju && (!sammuNr = 2 || !sammuNr = 3)
		then (
			try
				List.iter (fun s -> s.sv := Hashtbl.find (seisund.servavaadeldavused) (!(s.tipp1).nimi ^ ":" ^ !(s.tipp2).nimi)) servad
			with e -> Kosaraju.pooraServad(servad)
		);
	List.iter (fun s -> s.sv := Hashtbl.find (seisund.servavaadeldavused) (!(s.tipp1).nimi ^ ":" ^ !(s.tipp2).nimi)) servad;
	if (match !sammudKokku with | None -> false | Some s -> !sammuNr = s)
		then (
			if !prindiLoppTekst
				then (
					tekst := seisund.tekst;
					prindiLoppTekst := false
				)
			else tekst := ""
		)
	else tekst := seisund.tekst;
	nk1 := seisund.nk1;
	nk2 := seisund.nk2;
	nk3 := seisund.nk3;
	kuvaPilt(tipud, servad);;

(* sammu lisamine seisundina *)
let lisaSeisund(tipud, servad) =
	let tv = Hashtbl.create 10 in	(* tippude vaadeldavused *)
	let sv = Hashtbl.create 10 in	(* servade vaadeldavused *)
	let h = Hashtbl.create 10 in (* tippude hinnad *)
	List.iter (fun t -> Hashtbl.replace tv t.nimi !(t.tv)) tipud;
	List.iter (fun s -> Hashtbl.replace sv (!(s.tipp1).nimi ^ ":" ^ !(s.tipp2).nimi) !(s.sv)) servad;
	List.iter (fun t -> Hashtbl.replace h t.nimi !(t.hind)) tipud;
	let seisund = {
		tipuvaadeldavused = tv;
		servavaadeldavused = sv;
		hinnad = h;
  	tekst = !tekst;
  	nk1 = !nk1;
  	nk2 = !nk2;
  	nk3 = !nk3;
	} in Hashtbl.replace seisundid !sammuNr seisund;;

(* algoritmi edasisamm *)
let samm(algtipp, tipud, servad) =
	if !algoL2bi && (match !sammudKokku with | None -> true | _ -> false)
		then sammudKokku := Some !sammuNr;
	if !algoL2bi = false || (match !sammudKokku with | None -> false | Some s -> !sammuNr < s) 
		then sammuNr := !sammuNr + 1;
	if Hashtbl.mem seisundid !sammuNr
		then kuvaSeisund(tipud, servad)
	else (
  	(
  		match !algo with
    		| Prim -> Prim.samm(algtipp, tipud, servad)
    		| Laiuti -> Laiuti.samm(algtipp, tipud, servad)
    		| SygavutiEes -> SygavutiEes.samm(algtipp, tipud, servad)
    		| SygavutiLopp -> SygavutiLopp.samm(algtipp, tipud, servad)
    		| Kruskal -> Kruskal.samm(tipud, servad)
    		| Dijkstra -> Dijkstra.samm(algtipp, tipud, servad)
    		| FloydWarshall -> FloydWarshall.samm(tipud, servad)
    		| TopoKahn -> TopoKahn.samm(tipud, servad)
    		| TopoLopp -> TopoLopp.samm(algtipp, tipud, servad)
    		| Eeldusgraaf -> Eeldusgraaf.samm(tipud, servad)
    		| Kosaraju -> Kosaraju.samm(algtipp, tipud, servad)
		);
		kuvaPilt(tipud, servad);
		lisaSeisund(tipud, servad)
	);;

(* algoritmi tagasisamm, mille käigus kuvatakse salvestatud seisund *)
let tagasisamm(tipud, servad) =
	if !sammuNr > 0
		then sammuNr := !sammuNr - 1;
	if !prindiLoppTekst = false
		then prindiLoppTekst := true;
	kuvaSeisund(tipud, servad);;

(* failist sisu voogu lugemise funktsioon *)
(* allikas: https://ocaml.org/learn/tutorials/if_statements_loops_and_recursion.html - read_whole_chan *)
let loeFailiSisu(ic) =
  let puhver = Buffer.create 4096 in
  try
    while true 
  		do
        let rida = input_line(ic) in
        Buffer.add_string puhver rida;
        Buffer.add_char puhver '\n'
      done;
    assert false
  with
    End_of_file -> Buffer.contents puhver;;
  
(* funktsioon, mis loeb failist kogu sisu ja tagastab selle sõnena *)
let failiSisu(fail) =
  let ic = open_in fail in
	let sisu = loeFailiSisu ic in
	close_in ic;
	sisu;;

(* funktsioon, mis kirjutab faili sisu väljundvoogu (teise faili) *)
let kirjutaFaili(fail, oc) =
	let sisu = failiSisu(fail) in
	Printf.fprintf oc "%s" sisu;;

(* funktsioon, mis kustutab antud faili, kui see leidub *)
let kustutaFail(failinimi) =
	if Sys.file_exists(failinimi) then Sys.remove(failinimi);;

(* kustutame slaidide genereerimise käigus tekkinud failid *)
let kustutaFailid() =
	kustutaFail("temp.mp");
	kustutaFail("temp.log");
	for i = 1 to !(MetaPost.nr) - 1
	do
		let f = "temp" ^ string_of_int(i) ^ ".mps" in
		kustutaFail(f);
		let f2 = "temp" ^ string_of_int(i) ^ ".pdf" in
		kustutaFail(f2)
	done;
	kustutaFail(string_of_algo(!algo) ^ ".log");
	kustutaFail(string_of_algo(!algo) ^ ".aux");;

(* funktsioon, mis kirjutab kõikide tekkinud failide (temp1.mps, temp2.mps jne) sisud kokku ühte faili psFail *)
let koondaYhteFaili(psFail) =
	let oc = open_out psFail in
	let loendur = ref(1) in
	while !loendur < !(MetaPost.nr)
	do
		let f = "temp" ^ string_of_int(!loendur) ^ ".mps" in
		kirjutaFaili(f, oc);
		loendur := !loendur + 1
	done;
	close_out oc;;

(* funktsioon, mis teostab algoritmi läbimängu ja kirjutab slaidide sisu MetaPosti faili *)
let l2bim2ngSlaidideks(algtipp, tipud, servad, oc) =
	while !algoL2bi = false || (match !sammudKokku with | None -> false | Some sk -> !sammuNr < sk)
	do
		samm(algtipp, tipud, servad);
		Printf.fprintf oc "%s" (MetaPost.slaidiTekst(tipud, servad));
		MetaPost.nr := !(MetaPost.nr) + 1;
	done;;

(* funktsioon, mis teeb PostScripti failist PDF faili *)
let teePDF(fail, loendur) =
	let pdfTulemus = Sys.command("epstopdf " ^ fail ^ " --outfile=temp" ^ string_of_int(loendur) ^ ".pdf") in
	if pdfTulemus <> 0
		then failwith("PDFi tegemine ebaõnnestus.");;

(* funktsioon, mis teeb PostScripti failidest PDF failid ja liidab need üheks PDF failiks kokku *)
let teeKoondPDF() =
	print_endline(string_of_int(!(MetaPost.nr)));
	let loendur = ref(1) in
	while !loendur < !(MetaPost.nr)														(* itereerime üle PostScripti failide *)
	do
		print_endline(string_of_int(!loendur) ^ "/" ^ string_of_int(!(MetaPost.nr) - 1));
		let f = "temp" ^ string_of_int(!loendur) ^ ".mps" in
		teePDF(f, !loendur);																		(* teeme PostScripti failist PDFi *)
		kustutaFail(f);																					(* kustutame PostScripti faili *)
		loendur := !loendur + 1
	done;
	let pdfid = ref("") in
	let loendur = ref(1) in
	while !loendur < !(MetaPost.nr)
	do
		let f = " temp" ^ string_of_int(!loendur) ^ ".pdf" in		(* kogume kõik tekkinud PDF failide nimed sõneks kokku *)
		pdfid := !pdfid ^ f;
		loendur := !loendur + 1
	done;
	print_endline("PDFi koostamine.");												(* liidame kõik PDF failid üheks PDF failiks *)
	Sys.command("pdftk " ^ !pdfid ^ " cat output " ^ string_of_algo(!algo) ^ ".pdf");;

(* loome faili ja kirjutame sinna MetaPosti koodi *)
let looMPfail(fail, algtipp, tipud, servad) =
	let oc = open_out fail in																	(* loome väljundvoo *)
	Printf.fprintf oc "%s" (MetaPost.failiAlgus(tipud));			(* kirjutame faili alguse *)
	l2bim2ngSlaidideks(algtipp, tipud, servad, oc);						(* kirjutame läbimängu slaididena faili *)
	Printf.fprintf oc "%s" (MetaPost.failiLopp());						(* kirjutame faili lõpu *)
	close_out oc;;																						(* sulgeme väljundvoo *)
	
let looPSfailid() =
	let mpostTulemus = Sys.command("mpost temp.mp") in				
	if mpostTulemus <> 0
		(*then failwith("MetaPostist PostScriptiks tegemine ebaõnnestus.");	*)
	then ();; 			(* TODO: kodeering korda saada ja eelnev rida tagasi sisse kommenteerida *)
	
(* funktsioon, mis koostab ja tagastab LaTeXi teksti *)
let latexiTekst() =
	let mpsFailid = ref("") in
	for i = 1 to !(MetaPost.nr) - 1
	do
		let f = "temp" ^ string_of_int(i) ^ ".mps" in
		mpsFailid := !mpsFailid ^ "\n" ^ "\\includegraphics[scale=" ^ (if !algo = FloydWarshall then "0.7" else "1") ^ "]{" ^ f ^ "}"
	done;
	"\\documentclass{article}" ^ "\n" ^
	"\\usepackage{graphicx}" ^ "\n" ^
	"\\DeclareGraphicsRule{*}{mps}{*}{}" ^ "\n" ^
	"\\begin{document}" ^ "\n" ^
	!mpsFailid ^ "\n" ^
	"\\end{document}";;

(* teeme Postscripti failid ükshaaval PDFideks ja liidame PDFid kokku *)
let variant2() =
	let koondPDFTulemus = teeKoondPDF() in
	if koondPDFTulemus <> 0
		then failwith("KoondPDFi tegemine ebaõnnestus.");;

(* koostame LaTeX faili ja teeme pdflatex abil sellest PDFi *)
let variant3() =
	let texFail = string_of_algo(!algo) ^ ".tex" in 
	let oc = open_out texFail in
	Printf.fprintf oc "%s" (latexiTekst());
	close_out oc;
	let texTulemus = Sys.command("pdflatex " ^ texFail) in
	if texTulemus <> 0
		(*then failwith("LaTeXist PDFi tegemine ebaõnnestus.");;*)
		then variant2();;

(* proovime esmalt kõik tekkinud PostScripti failid üheks koondada ja sellest otse PDFi teha, ebaõnnestumise korral*)
(* proovime varianti 2 *) 	
let variant1() =		
	let psFail = string_of_algo(!algo) ^ ".1" in 							(* tekitame ühe PostScripti faili *)
	koondaYhteFaili(psFail);																	(* kirjutame kõik tekkinud PostScripti failid sinna kokku *)
	let pdfTulemus = Sys.command("epstopdf " ^ psFail) in			(* teeme EPS failist PDF faili *)
	kustutaFail(psFail);
	
	if pdfTulemus <> 0
		then variant2();;
	
(* põhiline funktsioon slaidide loomiseks *)
let looSlaidid(algtipp, tipud, servad) =
	looMPfail("temp.mp", algtipp, tipud, servad);							(* tekitame MetaPosti faili *)
	looPSfailid();																						(* tekitame MetaPosti failist PostScripti failid *)
	
	(*variant1();*)				(* kõik PS failid kokku üheks, sellest PDF. Häda: slaidid jäävad nihkes *)
	variant2();						(* igast PS failist eraldi PDF, need kokku liita. Häda: aeglane *)
	(*variant3();*)				(* PS failid LaTeXisse. Häda: pilt ei kata tervet slaidi *)
	
	kustutaFailid();																					(* kustutame slaidide genereerimise käigus tekkinud failid *)
	
	print_endline("\nPDF valmis.");
	programmK2ib := false;;																		(* sulgeme akna *)


(* funktsioon klahvivajutustele reageerimiseks *)
let klahvisyndmus(klahv, algtipp, tipud, servad) = 
	match klahv with
		| 'q' -> programmK2ib := false (*close_graph() *)
		| 's' -> looSlaidid(algtipp, tipud, servad)
		| 'n' -> samm(algtipp, tipud, servad)
		| 'b' -> tagasisamm(tipud, servad)
		| _ -> ();;

let hiirVajutatud = ref(false);;		(* bool - kas hiire kumbki klahv on alla vajutatud või mitte *)

(* funktsioon hiiresündmustele reageerimiseks *)
let hiiresyndmus(e, tipud, servad) =
	if e.button (* kui hiir on alla vajutatud *)
		then (
			if !hiirVajutatud
				then (
					if !liigutatavTipp <> None
						then (
							liigutaTippu(piiridesX(e.mouse_x), piiridesY(e.mouse_y), servad);
							tekst := "";
							kuvaPilt(tipud, servad)
						)
					else (
						if !liigutatavServ <> None
							then (
								liigutaServa(piiridesX(e.mouse_x), piiridesY(e.mouse_y));
								tekst := "";
								kuvaPilt(tipud, servad)
							)
					)
				) 
			else (	(* kui hiire alla vajutame ja parajasti mõne tipu või serva peal oleme, siis valime selle *)
				hiirVajutatud := true;
				try
					let valitudTipp = List.find (hiirTipul e.mouse_x e.mouse_y) tipud in
					liigutatavTipp := Some valitudTipp
				with
					| Not_found -> (
						try
							let valitudServ = List.find (hiirServal e.mouse_x e.mouse_y) servad in
							liigutatavServ := Some valitudServ
						with
							| Not_found -> ()
					)
			)
		)
	else (* hiirt vabastades vabastame ka valitud tipu/serva *)
		if !hiirVajutatud
			then (
				hiirVajutatud := false;
				liigutatavTipp := None;
				liigutatavServ := None
			);;

(* funktsioon hiire- ja klahvisündmuste käsitlemiseks *)
let syndmused(algtipp, tipud, servad) =
	while !programmK2ib do
		let e = wait_next_event [Button_down; Button_up; Mouse_motion; Key_pressed] in
		if e.keypressed 
			then klahvisyndmus(e.key, algtipp, tipud, servad)
		else hiiresyndmus(e, tipud, servad)
	done;; (* TODO: peaks close_graph'iga sulgema, aga Windowsis ei õnnestu *)

(* funktsioon, mis tagastab vastava nimega tipu, kui see leidub, vastasel juhul esimese tipu *)
let valiAlgtipp(nimi, tipud) =
	try
		List.find (fun t -> t.nimi = nimi) tipud
	with
		| Not_found -> List.hd tipud;;	(* TODO: erind, et uuesti prooviks? *)

(* funktsioon, mis kontrollib, et graafi servad oleks kas kõik suunatud või kõik ilma suunata *)
let kontrolliSuunatust(servad, suunatus) = 
	try
		let sobimatu = List.find (fun s -> s.nool <> suunatus) servad in
		failwith ("Graafi kõik servad peavad olema " ^ (if suunatus then "suunatud." else "mittesuunatud.") ^ 
			"\nVigane serv: " ^ string_of_serv(sobimatu))
	with
		| Not_found -> ();;

(* funktsioon, mis kontrollib, et graafi servad oleks kas kõik kaaludega või kõik ilma kaaludeta *)
let kontrolliKaale(servad, kaaludega) =
		try
			match kaaludega with
				| true -> (
					let sobimatu = List.find (fun s -> match s.kaal with | None -> true | _ -> false) servad in
  				failwith ("Graafi kõik servad peavad olema kaaludega." ^ "\nVigane serv: " ^ string_of_serv(sobimatu))
				)
				| false -> (
					let sobimatu = List.find (fun s -> match s.kaal with | Some k -> true | _ -> false) servad in
  				failwith ("Graafi kõik servad peavad olema kaaludeta." ^ "\nVigane serv: " ^ string_of_serv(sobimatu))
				)
  	with
  		| Not_found -> ();;

(* funktsioon, mis kontrollib, et graafi tipud oleks kas kõik hindadega või kõik ilma hidnadeta *)
let kontrolliHindu(tipud, hind) =
	try
			match hind with
				| true -> (
					let sobimatu = List.find (fun t -> match !(t.hind) with | None -> true | _ -> false) tipud in
  				failwith ("Graafi kõik tipud peavad olema hindadega." ^ "\nVigane tipp: " ^ string_of_tipp(sobimatu))
				)
				| false -> (
					let sobimatu = List.find (fun t -> match !(t.hind) with | Some h -> true | _ -> false) tipud in
  				failwith ("Graafi kõik servad peavad olema hindadeta." ^ "\nVigane tipp: " ^ string_of_tipp(sobimatu))
				)
  	with
  		| Not_found -> ();;

(* funktsioon, mis kontrollib, kas kõik servad on suunaga või kõik servad on suunata *)
let samadSuunad(servad) =
	List.for_all (fun s -> s.nool) servad || List.for_all (fun s -> s.nool = false) servad;;

(* funktsioon, mis kontrollib, et graaf sidus oleks *)
let kontrolliSidusust(algtipp, tipud, servad) =
	while !algoL2bi = false
		do
			Laiuti.samm(algtipp, tipud, servad)
		done;
	algoL2bi := false;							(* taastame algseisundi *)
	tekst := "";
	nk1 := "";
	nk2 := "";
	Laiuti.toodeldudTipud := [];		(* nullime uuesti andmestruktuurid ära *)
	Laiuti.j2rgmisedServad := [];
	i := Algus;
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud = false
		then failwith("Graaf peab sidus olema. Tippu " ^ (List.find (fun t -> !(t.tv) <> Vaadeldud) tipud).nimi ^ " algtipust ei pääse.")
	else (
		List.iter (fun t -> t.tv := Vaatlemata) tipud;
		List.iter (fun s -> s.sv := Vaatlemata) servad;
	);;

(* funktsioon, mis kontrollib, et graafi servade kõik kaalud oleks positiivsed *)
let kontrolliKaaluPositiivsust(servad) =
	try
		let sobimatu = List.find (fun s -> match s.kaal with | None -> false | Some k -> k < 0) servad in
  	failwith ("Graafi servade kõik kaalud peavad olema mittenegatiivsed." ^ "\nVigane serv: " ^ string_of_serv(sobimatu))
	with
		| Not_found -> ();;

(* funktsioon, mis kontrollib graafi sobivust *)
let graafiKontroll(algtipp, tipud, servad, suunatus, kaaludega, hindadega, sidusus) =
	kontrolliKaale(servad, kaaludega);
	if kaaludega && !algo <> FloydWarshall then kontrolliKaaluPositiivsust(servad);
	kontrolliHindu(tipud, hindadega);
	if sidusus then kontrolliSidusust(algtipp, tipud, servad);
	match suunatus with
		| None -> if samadSuunad(servad) = false then failwith("Graafis esineb nii suunatud kui mittesuunatud servi.")
		| Some b -> kontrolliSuunatust(servad, b);;
	
(* funktsioon, mis kontrollib, et samanimelisi tippe ei esineks ning et tippude hinnad oleks positiivsed ning 
tippude nimed oleks pikkusega 1 tähemärk *)
let tippudeKontroll(tipud) =
	for i = 0 to (List.length tipud) - 1
	do
		let nimi = (List.nth tipud i).nimi in
		if List.length (List.filter (fun t -> t.nimi = nimi) tipud) > 1
			then failwith("Tippu nimega " ^ nimi ^ " esineb mitu korda.")
	done;
	try
		let sobimatuHinnaga = List.find (fun t -> match !(t.hind) with | None -> false | Some h -> h < 1) tipud in
		failwith("Tippude hinnad peavad olema positiivsed. Vigane tipp: " ^ string_of_tipp(sobimatuHinnaga))
	with
		| Not_found -> ();
	try
		let sobimatuNimega = List.find (fun t -> String.length t.nimi <> 1) tipud in
		failwith("Tippude nimed peavad olema pikkusega 1 tähemärk. Vigane tipp: " ^ string_of_tipp(sobimatuNimega))
	with
		| Not_found -> ();;

(* funktsioon, mis kontrollib, et tippude vahel ei esineks kahekordseid servi, välja arvatud juhul, kui tegu on suunatud*)
(* graafiga ning servad on vastupidiste suundadega. Samuti kontrollib, et serv ei läheks tipust iseendasse ning et servade*)
(* kaalud jääksid lõiku [-99, 99] *)
let servadeKontroll(servad) =
	for i = 0 to List.length servad - 1
	do
		let nimi1 = !((List.nth servad i).tipp1).nimi in
		let nimi2 = !((List.nth servad i).tipp2).nimi in
		if nimi1 = nimi2
			then failwith("Tipust " ^ nimi1 ^ " ei tohiks minna serv iseendasse.");
		if (List.hd servad).nool			(* kui tegu on suunatud graafiga *)
			then (
				if List.length (List.filter (fun s -> !(s.tipp1).nimi = nimi1 && !(s.tipp2).nimi = nimi2) servad) > 1
					then failwith("Tippude " ^ nimi1 ^ " ja " ^ nimi2 ^ " vahel esineb rohkem kui üks samasuunaline serv.")
			)
		else (												(* kui tegu on mittesuunatud graafiga *)
			if List.length (List.filter (fun s -> !(s.tipp1).nimi = nimi1 && !(s.tipp2).nimi = nimi2 || 
				!(s.tipp1).nimi = nimi2 && !(s.tipp2).nimi = nimi1) servad) > 1
				then failwith("Tippude " ^ nimi1 ^ " ja " ^ nimi2 ^ " vahel esineb rohkem kui üks serv.")
		)
	done;
	try
		let sobimatu = List.find (fun s -> match s.kaal with | None -> false | Some k -> k < (-99) || k > 999) servad in
		failwith("Graafi kaalud peavad jääma lõiku [-99, 99]. Vigane serv: " ^ string_of_serv(sobimatu))
	with
		| Not_found -> ();;

(* funktsioon, mis kontrollib graafi sobivust algoritmile ning et ei leiduks samanimelisi tippe ega kahekordseid servi *)
let kontrolliGraafi(algtipp, tipud, servad) =
	tippudeKontroll(tipud);
	if List.length servad > 0 then servadeKontroll(servad);
	match !algo with 										(*algtipp, tipud, servad, suunad, 			kaalud, hinnad, sidus*)
		| Laiuti -> 				graafiKontroll (algtipp, tipud, servad, None, 				false, 	false, 	true)
		| SygavutiEes -> 		graafiKontroll (algtipp, tipud, servad, None, 				false, 	false, 	true)
  	| SygavutiLopp -> 	graafiKontroll (algtipp, tipud, servad, None, 				false, 	false, 	true)
  	| Prim -> 					graafiKontroll (algtipp, tipud, servad, Some false, 	true, 	false, 	true)
  	| Kruskal -> 				graafiKontroll (algtipp, tipud, servad, Some false, 	true, 	false, 	false)
  	| Dijkstra -> 			graafiKontroll (algtipp, tipud, servad, Some true, 	true, 	false, 	true)
  	| FloydWarshall -> 	graafiKontroll (algtipp, tipud, servad, Some true, 	true, 	false, 	false)
  	| TopoLopp -> 			graafiKontroll (algtipp, tipud, servad, Some true, 	false, 	false, 	false)
  	| TopoKahn -> 			graafiKontroll (algtipp, tipud, servad, Some true, 	false, 	false, 	false)
  	| Eeldusgraaf -> 		graafiKontroll (algtipp, tipud, servad, Some true, 	false, 	true, 	false)
  	| Kosaraju -> 			graafiKontroll (algtipp, tipud, servad, Some true, 	false, 	false, 	false);;

(* funktsioon, mis tagastab, kas tegu on algtippu nõudva algoritmiga *)
let algtipugaAlgoritm() =
	let algtipugaAlgoritmid = [Laiuti; SygavutiEes; SygavutiLopp; Prim; Dijkstra; TopoLopp; Kosaraju]  in
	List.mem !algo algtipugaAlgoritmid;;

(* funktsioon, mis tagastab listi kõikidest topeltservadest (igast topeltservast ühe serva) *)
let rec leiaTopeltServad(servad) =
	match servad with
		| [] -> []
		| x::xs -> (
			match x with
				| {tipp1 = t1; tipp2 = t2;} -> if List.exists (fun s -> !(s.tipp1).nimi = !t2.nimi && !(s.tipp2).nimi = !t1.nimi) xs then x :: leiaTopeltServad xs else leiaTopeltServad xs 
		);;

let kumerdaTopeltServa servad serv1 =
	let serv2 = List.find (fun s -> !(serv1.tipp1).nimi = !(s.tipp2).nimi && !(serv1.tipp2).nimi = !(s.tipp1).nimi) servad in
	liigutatavServ := Some serv1;
	let (a, c) = leiaSirge(float_of_int(!(!(serv1.tipp1).x)), float_of_int(!(!(serv1.tipp1).y)), float_of_int(!(!(serv1.tipp2).x)), float_of_int(!(!(serv1.tipp2).y))) in
	let keskX = float_of_int(!(!(serv1.tipp1).x) + !(!(serv1.tipp2).x)) /. 2. in
	let keskY = float_of_int(!(!(serv1.tipp1).y) + !(!(serv1.tipp2).y)) /. 2. in
	let (a2, c2) = leiaRistuvSirge(a, c, keskX, keskY) in
		(* kümnega jagamine omavoliline. Igatahes vaja jagada vähemalt kahega *)
	let r = leiaJoonePikkus(float_of_int(!(!(serv1.tipp1).x)), float_of_int(!(!(serv1.tipp1).y)), float_of_int(!(!(serv1.tipp2).x)), float_of_int(!(!(serv1.tipp2).y))) /. 10. in
	let ((e1, f1), (e2, f2)) = kaksPunkti(keskX, keskY, a2, c2, r) in
	liigutaServa(int_of_float(e1), int_of_float(f1));
	liigutatavServ := Some serv2;
	liigutaServa(int_of_float(e2), int_of_float(f2));
	liigutatavServ := None;;

(* kui graafis esineb topeltservi, siis kuvatakse need kohe alguses kumerdatult *)
let kumerdaTopeltServad(tipud, servad) =
	let ts = leiaTopeltServad(servad) in
	List.iter (kumerdaTopeltServa servad) ts;;
	

(* programmi algustoimingud *)
let alusta(graaf, algtipuNimi) =
	let algtipp = valiAlgtipp(algtipuNimi, graaf.tipud) in
	let tipud = graaf.tipud in
	let servad = graaf.servad in
	kontrolliGraafi(algtipp, tipud, servad);
	let x = (if !algo = FloydWarshall then laiusLisa else 0) in
	open_graph (" " ^ string_of_int(aknaLaius + x) ^ "x" ^ string_of_int(aknaKorgus + korgusLisa));
	set_window_title ("Graafialgoritmid - " ^ string_of_algo(!algo));
	if algtipugaAlgoritm()
		then algtipp.tv := Valitud; (* märgime algtipu valituks, et seda juba 1. slaidil kuvada *)
	kumerdaTopeltServad(tipud, servad);
	kuvaPilt(tipud, servad);
	lisaSeisund(tipud, servad);
	syndmused(algtipp, tipud, servad);;

(* funktsioon, mis määrab algoritmi *)
let m22raAlgo(algorida) =
	let jupid = Str.split (Str.regexp " *: *") algorida in
	let algoSone = List.nth jupid 1 in
	algo := List.find (fun a -> string_of_algo(a) = algoSone) algoList;;

(* funktsioon sõnest tipuandmete kättesaamiseks ja nende abil tipu loomiseks *)
let looTippFailist(tipurida, hindadega) = 
	let tipuJupid = Str.split (Str.regexp " *, *") tipurida in
	let nimi = List.nth tipuJupid 0 in
	let hind = ref(1 + Random.int 15) in			(* valime suvalise hinna lõigust [1, 15] *)
	let x = ref(Random.int aknaLaius) in			(* valime suvalise x-koordinaadi poollõigust [0, aknaLaius) *)
	let y = ref(Random.int aknaKorgus) in			(* valime suvalise y-koordinaadi poollõigust [0, aknaKorgus) *)
	if List.length tipuJupid >= 3
		then (
			if (List.nth tipuJupid 1) <> "-" then x := int_of_string(List.nth tipuJupid 1);
			if (List.nth tipuJupid 2) <> "-" then y := int_of_string(List.nth tipuJupid 2);
		);
	if hindadega && List.length tipuJupid = 4
		then if (List.nth tipuJupid 3) <> "-" then hind := int_of_string(List.nth tipuJupid 3);
	looTipp(nimi, !x, !y, if hindadega then Some(!hind) else None);;

(* funktsioon sõnest servaandmete kättesaamiseks ja nende abil serva loomiseks *)
let looServFailist(servarida, kaaludega, suundadega, tipud) = 
	let servaJupid = Str.split (Str.regexp " *, *") servarida in
	let t1nimi = List.nth servaJupid 0 in
	let t2nimi = List.nth servaJupid 1 in
	let kaal = ref(1 + Random.int 20) in			(* valime suvalise kaalu lõigust [1, 20] *)
	if kaaludega && List.length servaJupid = 3
		then if (List.nth servaJupid 2) <> "-" then kaal := int_of_string(List.nth servaJupid 2);
	looServ((t1nimi, t2nimi, (if kaaludega then Some(!kaal) else None), (if suundadega then true else false)), tipud);;

(* funktsioon sisendandmete failist lugemiseks *)
let sisendandmedFailist(fail) = 
	let ic = open_in fail in
	try
		let algorida = input_line ic in
		m22raAlgo(algorida);
		let tippuderida = input_line ic in
		let tippudeJupid = Str.split (Str.regexp " *, *") tippuderida in
		let tippudeArv = int_of_string(List.nth (Str.split (Str.regexp " *: *") (List.nth tippudeJupid 0)) 1) in
		let hindadega = List.nth (Str.split (Str.regexp " *: *") (List.nth tippudeJupid 1)) 1 = "true" in
		let servaderida = input_line ic in
		let servadeJupid = Str.split (Str.regexp " *, *") servaderida in
		let servadeArv = int_of_string(List.nth (Str.split (Str.regexp " *: *") (List.nth servadeJupid 0)) 1) in
		let kaaludega = List.nth (Str.split (Str.regexp " *: *") (List.nth servadeJupid 1)) 1 = "true" in
		let suundadega = List.nth (Str.split (Str.regexp " *: *") (List.nth servadeJupid 2)) 1 = "true" in
		let tipud = ref([]) in
		let servad = ref([]) in
		Random.self_init();
		for i = 0 to tippudeArv - 1
		do
			let tipurida = input_line ic in
			let tipp = looTippFailist(tipurida, hindadega) in
			tipud := !tipud @ [tipp]
		done;
		for i = 0 to servadeArv - 1
		do
			let servarida = input_line ic in
			let serv = looServFailist(servarida, kaaludega, suundadega, !tipud) in
			servad := !servad @ [serv]
		done;
		let algtipuNimi = (List.nth !tipud 0).nimi in
		let graaf = {
			tipud = !tipud;
			servad = !servad;
		} in
		close_in ic;
		(graaf, algtipuNimi);
	with e -> failwith("Ei õnnestunud failist sisendandmeid lugeda.");;
	
(* kogu programmi peameetod, kus saab sisendandmeid muuta *)
let main() =
	if Array.length Sys.argv > 1					(* kui käsurealt anti sisendfail, loeme sisendandmeid sealt *)
		then (
			let (graaf, algtipuNimi) = sisendandmedFailist(Sys.argv.(1)) in 
			alusta(graaf, algtipuNimi)
		)
	else (																(* vastasel korral määrame sisendandmed siin ise *)
  	algo := Dijkstra;
  	let graaf = ntDijkstra4 in
  	let algtipuNimi = "A" in						(* peab olema ka siis, kui algoritm algtippu ei nõua *)
		alusta(graaf, algtipuNimi)
	);;


main();;