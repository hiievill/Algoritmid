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
  			asubVahel(hx, !(!t1.x), !(!t2.x)) && (* hiir on horisontaalselt 2 tipu vahel *)
  			asubVahel(hy, !(!t1.y), !(!t2.y)) && (* hiir on vertikaalselt 2 tipu vahel *)
  			(* hiire kaugus ringjoone keskpunktist on raadius +/- 5 *) 
  			abs_float (sqrt ((float_of_int(hx - x)) ** 2. +. (float_of_int(hy - y)) ** 2.) -. float_of_int(r)) <= 5.
			)		
		);;

(* funktsioon, mis tagastab sirgete y=a1x+c2 ja y=a2x+c2 lõikumispunkti (e, f) *)
let leiaYhinePunkt(a1, c1, a2, c2) =
	if a1 = a2
		then failwith("Sirge.") (* TODO: sirge kontroll kuskile *)
	else (
		let e = (c2 -. c1) /. (a1 -. a2) in
		let f = a1 *. e +. c1 in
		(e, f) 
	);;

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
						(* TODO: bugi - kui raadius on täpselt serva keskpunktis(?), siis ei kuva kaart/ringi, vaid sirge *)
						let keskX = float_of_int(!(!t1.x) + !(!t2.x)) /. 2. in
  					let keskY = float_of_int(!(!t1.y) + !(!t2.y)) /. 2. in
						let jp1 = leiaJoonePikkus(keskX, keskY, float_of_int(hx), float_of_int(hy)) in							(* hiire kaugus serva keskptist *)
						let jp2 = leiaJoonePikkus(keskX, keskY, float_of_int(!(!t1.x)), float_of_int(!(!t1.y))) in	(* pool servapikkust *)
						(* kui hiire kaugus serva keskpunktist on väiksem kui pool servapikkust, siis muudame *)
						if jp1 < jp2 then (
    					(* TODO: horisontaalne ja vertikaalne on bugised! *)
  						if !(!t1.x) = !(!t2.x)
  							then (
  								let (a3, c3) = leiaSirge(float_of_int(!(!t1.x)), float_of_int(!(!t1.y)), float_of_int(hx), float_of_int(hy)) in
									let x = (keskY -. c3) /. a3 in
									let (e, f) = (x, keskY) in
									let r = sqrt ((float_of_int(hx) -. e) ** 2. +. (float_of_int(hy) -. f) ** 2.) in
    							let k = r -. leiaJoonePikkus(e, f, keskX, keskY) in		(* k = r - ringjoone keskpunkti kaugus serva keskpunktist *)
									(*print_endline("Siin: " ^ string_of_float(r));*)
      						uuendaKaareAndmeid(!t1, !t2, int_of_float e, int_of_float f, int_of_float r, int_of_float k)
  							)
  						else if !(!t1.y) = !(!t2.y)
  							then (
  								let (a3, c3) = leiaSirge(float_of_int(!(!t1.x)), float_of_int(!(!t1.y)), float_of_int(hx), float_of_int(hy)) in
  								let y = a3 *. keskX +. c3 in
  								let (e, f) = (keskX, y) in
									let r = sqrt ((float_of_int(hx) -. e) ** 2. +. (float_of_int(hy) -. f) ** 2.) in
    							let k = r -. leiaJoonePikkus(e, f, keskX, keskY) in		(* k = r - ringjoone keskpunkti kaugus serva keskpunktist *)
      						uuendaKaareAndmeid(!t1, !t2, int_of_float e, int_of_float f, int_of_float r, int_of_float k)
  							)
  						else (
    						let (a1, c1) = leiaSirge(float_of_int(!(!t1.x)), float_of_int(!(!t1.y)), float_of_int(!(!t2.x)), float_of_int(!(!t2.y))) in
      					let (a2, c2) = leiaRistuvSirge(a1, c1, keskX, keskY) in
      					let (a3, c3) = leiaSirge(float_of_int(!(!t1.x)), float_of_int(!(!t1.y)), float_of_int(hx), float_of_int(hy)) in
      					let (a4, c4) = leiaRistuvSirge(a3, c3, float_of_int(!(!t1.x) + hx) /. 2., float_of_int(!(!t1.y) + hy) /. 2.) in
      					let (e, f) = leiaYhinePunkt(a2, c2, a4, c4) in
      					let r = sqrt ((float_of_int(hx) -. e) ** 2. +. (float_of_int(hy) -. f) ** 2.) in
  							let k = r -. leiaJoonePikkus(e, f, keskX, keskY) in		(* k = r - ringjoone keskpunkti kaugus serva keskpunktist *)
    						uuendaKaareAndmeid(!t1, !t2, int_of_float e, int_of_float f, int_of_float r, int_of_float k)
							)
						)
					)
				)
		);;

(* funktsioon, mis uuendab ringjoone võrrandit, jättes kauguse tippudevahelise lõigu keskpunktist samaks,*)
(* välja arvatud siis, kui tippe lähemale nihutatakse *)
(*let uuendaKaart(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> (
			let nimi = !t1.nimi ^ ":" ^ !t2.nimi in
			if Hashtbl.find kaareR nimi <> 0 then (
      	let tippudeVahelineKaugus = leiaJoonePikkus(float_of_int(!(!t1.x)), float_of_int(!(!t1.y)), float_of_int(!(!t2.x)), float_of_int(!(!t2.y))) in
  			if float_of_int(Hashtbl.find kaareK nimi) >= tippudeVahelineKaugus
      		then Hashtbl.replace kaareK nimi (int_of_float(tippudeVahelineKaugus) - 1);
  			let keskX = float_of_int(!(!t1.x) + !(!t2.x)) /. 2. in
    		let keskY = float_of_int(!(!t1.y) + !(!t2.y)) /. 2. in
  			let (a, c) = leiaSirge(!(!t1.x), !(!t1.y), !(!t2.x), !(!t2.y)) in
				let (a2, c2) = leiaRistuvSirge(a, c, keskX, keskY) in
  			let ((e1, f1), (e2, f2)) = Graafika.kaksPunkti(keskX, keskY, a2, c2, float_of_int(Hashtbl.find kaareK nimi)) in
  			let senineX = float_of_int(Hashtbl.find kaareX nimi) in
  			let senineY = float_of_int(Hashtbl.find kaareY nimi) in
  			(* leiame selle keskpunkti, mis on tippudevahelise lõigu keskpunktist kaugemal *)
  			let (e, f) = if leiaJoonePikkus(senineX, senineY, e1, f1) >= leiaJoonePikkus(senineX, senineY, e2, f2) then (e1, f1) else (e2, f2) in
  			liigutaServa(int_of_float(e), int_of_float(f))
			)		
		);;*)

let liigutaTippu(hx, hy, servad) =
	match !liigutatavTipp with
		| Some t -> (
			t.x := hx;
			t.y := hy;
			(*List.iter (fun s -> if !(s.tipp1) = t || !(s.tipp2) = t then uuendaKaart(s)) servad;*)
			List.iter (fun s -> if !(s.tipp1) = t || !(s.tipp2) = t then uuendaKaareAndmeid(!(s.tipp1), !(s.tipp2), 0, 0, 0, 0)) servad;
			(* TODO: valib millegipärast ainult ühe serva *)
		)
		| _ -> print_endline("Tippu pole valitud, nii ei tohiks juhtuda.");;
		
(* TODO: Prim.samm, Laiuti.samm jne *)
let samm(algtipp, tipud, servad) =
	match !algo with
		| Prim -> Prim.prim(algtipp, tipud, servad)
		| Laiuti -> Laiuti.laiuti(algtipp, tipud, servad)
		| SygavutiEes -> SygavutiEes.sygavutiEes(algtipp, tipud, servad)
		| SygavutiLopp -> SygavutiLopp.sygavutiLopp(algtipp, tipud, servad)
		| Kruskal -> Kruskal.kruskal(tipud, servad)
		| Dijkstra -> Dijkstra.dijkstra(algtipp, tipud, servad)
		| FloydWarshall -> FloydWarshall.floydWarshall(tipud, servad)
		| TopoKahn -> TopoKahn.topoKahn(tipud, servad)
		| TopoLopp -> TopoLopp.topoLopp(algtipp, tipud, servad)
		| Eeldusgraaf -> Eeldusgraaf.eeldusgraaf(tipud, servad)
		| Kosaraju -> Kosaraju.kosaraju(algtipp, tipud, servad);;

let programmK2ib = ref(true);;

(* failist sisu voogu lugemise funktsioon *)
(* allikas: https://ocaml.org/learn/tutorials/if_statements_loops_and_recursion.html *)
let read_whole_chan(chan) =
  let buf = Buffer.create 4096 in
  try
    while true do
      let rida = input_line(chan) in
      Buffer.add_string buf rida;
      Buffer.add_char buf '\n'
    done;
    assert false
  with
    End_of_file -> Buffer.contents buf;;
  
(* funktsioon, mis loeb failist kogu sisu ja tagastab selle sõnena *)
let failiSisu(fail) =
  let chan = open_in fail in
	let sisu = read_whole_chan chan in
	close_in chan;
	sisu;;

(* funktsioon, mis kirjutab faili sisu väljundvoogu (teise faili) *)
let kirjutaFaili(fail, oc) =
	let sisu = failiSisu(fail) in
	Printf.fprintf oc "%s" sisu;;

let kustutaFail(failinimi) =
	Sys.remove(failinimi);;

(* funktsioon, mis kirjutab kõikide tekkinud failide (temp.1, temp.2 jne) sisud kokku ühte faili psFail *)
let koondaYhteFaili(psFail) =
	let oc = open_out psFail in
	let loendur = ref(1) in
	while !loendur < !(MetaPost.nr)
	do
		let f = "temp." ^ string_of_int(!loendur) in
		kirjutaFaili(f, oc);
		kustutaFail(f);
		loendur := !loendur + 1
	done;
	close_out oc;;

(* funktsioon, mis teostab algoritmi läbimängu ja kirjutab slaidide sisu MetaPosti faili *)
let l2bim2ngSlaidideks(algtipp, tipud, servad, oc) =
	while !algoL2bi = false
	do
		samm(algtipp, tipud, servad);
		kuvaPilt(tipud, servad);
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
	let loendur = ref(1) in
	while !loendur < !(MetaPost.nr)														(* itereerime üle PostScripti failide *)
	do
		print_endline(string_of_int(!loendur) ^ "/" ^ string_of_int(!(MetaPost.nr) - 1));
		let f = "temp." ^ string_of_int(!loendur) in
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
	done;																											(* liidame kõik PDF failid üheks PDF failiks *)
	Sys.command("pdftk " ^ !pdfid ^ " cat output " ^ string_of_algo(!algo) ^ ".pdf");;

(* funktsioon, mis tagastab, kas tegu on algtippu nõudva algoritmiga *)
let algtipugaAlgoritm() =
	let algtipugaAlgoritmid = [Laiuti; SygavutiEes; SygavutiLopp; Prim; Dijkstra; TopoLopp; Kosaraju]  in
	List.mem !algo algtipugaAlgoritmid;;

(* põhiline funktsioon slaidide loomiseks *)
let looSlaidid(algtipp, tipud, servad) =
	let fail = "temp.mp" in																		(* loome faili, kuhu MetaPosti koodi lisama hakkame *)
	let oc = open_out fail in																	(* loome väljundvoo *)
	Printf.fprintf oc "%s" (MetaPost.failiAlgus(tipud));			(* kirjutame faili alguse *)
	l2bim2ngSlaidideks(algtipp, tipud, servad, oc);						(* kirjutame läbimängu slaididena faili *)
	Printf.fprintf oc "%s" (MetaPost.failiLopp());						(* kirjutame faili lõpu *)
	close_out oc;																							(* sulgeme väljundvoo *)
	
	let mpostTulemus = Sys.command("mpost temp.mp") in				(* tekitame MetaPosti failist PostScripti failid *)
	if mpostTulemus <> 0
		(*then failwith("MetaPostist PostScriptiks tegemine ebaõnnestus.");	*)
	then (); 			(* TODO: kodeering korda saada ja eelnev rida tagasi sisse kommenteerida *)	
	
	(* proovime kõik tekkinud PostScripti failid üheks koondada ja sellest otse PDFi teha *)
		
	let psFail = string_of_algo(!algo) ^ ".1" in 							(* tekitame ühe PostScripti faili *)
	koondaYhteFaili(psFail);																	(* kirjutame kõik tekkinud PostScripti failid sinna kokku *)
	let pdfTulemus = Sys.command("epstopdf " ^ psFail) in			(* teeme EPS failist PDF faili *)
	
	(*kustutaFail("temp.mp");*)																		(* kustutame tekkinud failid *)
	kustutaFail("temp.log");
	kustutaFail(psFail);
	
	(* kui ei õnnestunud, siis teeme Postscripti failid ükshaaval PDFideks ja liidame PDFid kokku *)
	
	if pdfTulemus <> 0
		then (
			let koondPDFTulemus = teeKoondPDF() in
			if koondPDFTulemus <> 0
				then failwith("KoondPDFi tegemine ebaõnnestus.")
		);
	
	print_endline("\nPDF valmis.");
	programmK2ib := false;;																		(* sulgeme akna *)


(* funktsioon klahvivajutustele reageerimiseks *)
let klahvisyndmus(klahv, algtipp, tipud, servad) = 
	match klahv with
		| 'q' -> programmK2ib := false (*close_graph() *)
		| 's' -> if !algoL2bi = false then looSlaidid(algtipp, tipud, servad)
		| 'n' -> if !algoL2bi = false then (
			samm(algtipp, tipud, servad);
			kuvaPilt(tipud, servad)
		)
		| 'f' -> failwith("Lõpp.") (* ajutine lahendus akna "ilusaks" sulgemiseks Windowsis *) (* TODO: ebavajalik *)
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
							tekst := ""; (* TODO: ajutine, et läbimängul üleliigseid väljatrükkimisi poleks *)
							kuvaPilt(tipud, servad)
						)
					else (
						if !liigutatavServ <> None
							then (
								liigutaServa(piiridesX(e.mouse_x), piiridesY(e.mouse_y)); (* tegelt on võimalik välja nihutada. TODO: fix *)
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

let syndmused(algtipp, tipud, servad) =
	while !programmK2ib do
		let e = wait_next_event [Button_down; Button_up; Mouse_motion; Key_pressed] in
		if e.keypressed 
			then klahvisyndmus(e.key, algtipp, tipud, servad)
		else hiiresyndmus(e, tipud, servad)
	done;; (* peaks close_graph'iga sulgema, aga Windowsis ei õnnestu *)

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
  				failwith ("Graafi kõik servad peavad olema kaaludega." ^ "\nVigane serv: " ^ string_of_serv(sobimatu))
				)
  	with
  		| Not_found -> ();;

(* funktsioon, mis kontrollib, et graafi tipud oleks kas kõik hindadega või kõik ilma hidnadeta *)
let kontrolliHindu(tipud, hind) =
	try
			match hind with
				| true -> (
					let sobimatu = List.find (fun t -> match t.hind with | None -> true | _ -> false) tipud in
  				failwith ("Graafi kõik tipud peavad olema hindadega." ^ "\nVigane tipp: " ^ string_of_tipp(sobimatu))
				)
				| false -> (
					let sobimatu = List.find (fun t -> match t.hind with | Some h -> true | _ -> false) tipud in
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
			Laiuti.laiuti(algtipp, tipud, servad)
		done;
	algoL2bi := false;
	tekst := "";
	nk1 := "";
	nk2 := "";
	i := Algus;
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud = false
		then failwith("Graaf peab sidus olema.")
	else (
		List.iter (fun t -> t.tv := Vaatlemata) tipud;
		List.iter (fun s -> s.sv := Vaatlemata) servad;
	);;

(* funktsioon, mis kontrollib graafi sobivust *)
let graafiKontroll(algtipp, tipud, servad, suunatus, kaaludega, hindadega, sidusus) =
	match suunatus with
		| None -> if samadSuunad(servad) = false then failwith("Graafis esineb nii suunatu kui mittesuunatud servi.")
		| Some b -> kontrolliSuunatust(servad, b);
	kontrolliKaale(servad, kaaludega);
	kontrolliHindu(tipud, hindadega);
	if sidusus then kontrolliSidusust(algtipp, tipud, servad);;
	(* TODO: kaalude märk (enamikul >= 0) ja tüklilisuse kontroll ka (topodel ei või üldse, FW-l ei või neg tsükleid) *)

(* funktsioon, mis kontrollib graafi sobivust algoritmile *)
let graafiKontroll(algtipp, tipud, servad) =
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
  	| Eeldusgraaf -> 		graafiKontroll (algtipp, tipud, servad, Some true, 	false, 	true, 	true) (* TODO: sidusus õige? *)
  	| Kosaraju -> 			graafiKontroll (algtipp, tipud, servad, Some true, 	false, 	false, 	false);;

let alusta(graaf, algtipuNimi) =
	let algtipp = valiAlgtipp(algtipuNimi, graaf.tipud) in
	let tipud = graaf.tipud in
	let servad = graaf.servad in
	
	graafiKontroll(algtipp, tipud, servad);
	let x = (if !algo = FloydWarshall then laiusLisa else 0) in
	open_graph (" " ^ string_of_int(aknaLaius + x) ^ "x" ^ string_of_int(aknaKorgus + korgusLisa));
	set_window_title ("Graafialgoritmid - " ^ string_of_algo(!algo));
	if algtipugaAlgoritm()
		then algtipp.tv := Valitud; (* märgime algtipu valituks, et seda juba 1. slaidil kuvada *)
	kuvaPilt(tipud, servad);
	syndmused(algtipp, tipud, servad);;
	
let main() =
	
	if Array.length Sys.argv > 1
		then (); (* TODO! Sisendandmed failist. *)
	
	algo := Dijkstra;
	let graaf = ntDijkstra1 in
	let algtipuNimi = "A" in						(* peab olema ka siis, kui algoritm algtippu ei nõua *)
	
	alusta(graaf, algtipuNimi);;

		
main();;