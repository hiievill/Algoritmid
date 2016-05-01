open Graphics;;
open Struktuurid;;
open Graafika;;
open AlgoBaas;;
open NtGraafid;;

let liigutatavTipp = ref(None);;
(*let liigutatavServ = ref(None);;*)

(* funktsioon, mis leiab sirge y=ax+c, millel asuvad tipud (x1;y1) ja (x2;y2), ja tagastab (a,c) *)
let leiaSirge(x1, y1, x2, y2) =
	let a = float_of_int(y2 - y1) /. float_of_int(x2 - x1) in
	let c = float_of_int(y1) -. a *. float_of_int(x1) in
	(a, c);;

(* funktsioon, mis tagastab, kas arv a asub b1 ja b2 vahel *)
let asubVahel(a, b1, b2) =
	a <= max b1 b2 && a >= min b1 b2;; 

(* funktsioon, mis tagastab, kas punkt (x, y) asub sirgel y=ax+c *)
let asubSirgel(x, y, a, c) =
	abs_float (a *. x +. c -. y) <= 1.;;		(* <= 1, sest vahel teen floate intideks, seega t�pne ei pruugiks sobida *) 	

(* funktsioon, mis tagastab, kas punktis (hx, hy) asuv hiir asub sirgel, mis �hendab tippe t1 ja t2 *)
let hiirAsubSirgel(t1, t2, hx, hy)=
	if !(t1.x) = !(t2.x) (* horisontaalne sirge *)
		then hx = !(t1.x) && asubVahel(hy, !(t1.y), !(t2.y))
	else (
		if !(t1.y) = !(t2.y) (* horisontaalne sirge *)
			then hy = !(t1.y) && asubVahel(hx, !(t1.x), !(t2.x))
		else ( (* tavaline sirge *)
			let (a, c) = leiaSirge(!(t1.x), !(t1.y), !(t2.x), !(t2.y)) in 
			asubSirgel(float_of_int(hx), float_of_int(hy), a, c) &&
			asubVahel(hx, !(t1.x), !(t2.x)) && asubVahel(hy, !(t1.y), !(t2.y)) 
		)
	);;

(* funktsioon, mis tagastab, kas hiir asub tipu peal *)
let hiirTipul hx hy tipp =
	match tipp with
		| {nimi = n; x = tx; y = ty} -> sqrt ((float_of_int(hx - !tx)) ** 2. +. (float_of_int(hy - !ty)) ** 2.) <= float_of_int(tipuRaadius);;

(* funktsioon, mis tagastab, kas hiir asub serva peal (+/- 5) *)
(*let hiirServal hx hy serv =
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
		);;*)

(* funktsioon, mis tagastab sirgete y=a1x+c2 ja y=a2x+c2 l�ikumispunkti (e, f) *)
let leiaYhinePunkt(a1, c1, a2, c2) =
	if a1 = a2
		then failwith("Sirge.") (* TODO: sirge kontroll kuskile *)
	else (
		let e = (c2 -. c1) /. (a1 -. a2) in
		let f = a1 *. e +. c1 in
		(e, f) 
	);;

(* funktsioon, mis m��rab serva ringjoonele uued parameetrid, s�ltuvalt sellest, kus hiir on *)
(*let liigutaServa(hx, hy) =
	match !liigutatavServ with
		| None -> print_endline("Serva pole valitud, nii ei tohiks juhtuda.") (*TODO: printimise asemel failwith? *)
		| Some s -> (
			match s with
				| {tipp1 = t1; tipp2 = t2;} -> (
					if hiirAsubSirgel(!t2, !t2, hx, hy)
						then nulliKaareAndmed(!t1, !t2)
					else (
						let (a1, c1) = leiaSirge(!(!t1.x), !(!t1.y), !(!t2.x), !(!t2.y)) in
  					let keskX = float_of_int(!(!t1.x) + !(!t2.x)) /. 2. in
  					let keskY = float_of_int(!(!t1.y) + !(!t2.y)) /. 2. in
  					let (a2, c2) = leiaRistuvSirge(a1, c1, keskX, keskY) in
  					let (a3, c3) = leiaSirge(!(!t1.x), !(!t1.y), hx, hy) in
  					let (a4, c4) = leiaRistuvSirge(a3, c3, float_of_int(!(!t1.x) + hx) /. 2., float_of_int(!(!t1.y) + hy) /. 2.) in
  					let (e, f) = leiaYhinePunkt(a2, c2, a4, c4) in
  					let r = sqrt ((float_of_int(hx) -. e) ** 2. +. (float_of_int(hy) -. f) ** 2.) in
						let nimi = !t1.nimi ^ ":" ^ !t2.nimi in
  					Hashtbl.replace kaareX nimi (int_of_float e);
  					Hashtbl.replace kaareY nimi (int_of_float f);
  					Hashtbl.replace kaareR nimi (int_of_float r)
					)
				)
		);;*)

let liigutaTippu(hx, hy) =
	match !liigutatavTipp with
		| Some t -> (
			t.x := piiridesX(hx);
			t.y := piiridesY(hy);
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
  
(* funktsioon, mis loeb failist kogu sisu ja tagastab selle s�nena *)
let failiSisu(fail) =
  let chan = open_in fail in
	let sisu = read_whole_chan chan in
	close_in chan;
	sisu;;

(* funktsioon, mis kirjutab faili sisu v�ljundvoogu (teise faili) *)
let kirjutaFaili(fail, oc) =
	let sisu = failiSisu(fail) in
	Printf.fprintf oc "%s" sisu;;

let kustutaFail(failinimi) =
	Sys.remove(failinimi);;

(* funktsioon, mis kirjutab k�ikide tekkinud failide (temp.1, temp.2 jne) sisud kokku �hte faili psFail *)
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

(* funktsioon, mis teostab algoritmi l�bim�ngu ja kirjutab slaidide sisu MetaPosti faili *)
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
		then failwith("PDFi tegemine eba�nnestus.");;

(* funktsioon, mis teeb PostScripti failidest PDF failid ja liidab need �heks PDF failiks kokku *)
let teeKoondPDF() =
	let loendur = ref(1) in
	while !loendur < !(MetaPost.nr)														(* itereerime �le PostScripti failide *)
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
		let f = " temp" ^ string_of_int(!loendur) ^ ".pdf" in		(* kogume k�ik tekkinud PDF failide nimed s�neks kokku *)
		pdfid := !pdfid ^ f;
		loendur := !loendur + 1
	done;																											(* liidame k�ik PDF failid �heks PDF failiks *)
	Sys.command("pdftk " ^ !pdfid ^ " cat output " ^ string_of_algo(!algo) ^ ".pdf");;

(* p�hiline funktsioon slaidide loomiseks *)
let looSlaidid(algtipp, tipud, servad) =
	let fail = "temp.mp" in																		(* loome faili, kuhu MetaPosti koodi lisama hakkame *)
	let oc = open_out fail in																	(* loome v�ljundvoo *)
	Printf.fprintf oc "%s" (MetaPost.failiAlgus(tipud));			(* kirjutame faili alguse *)
	l2bim2ngSlaidideks(algtipp, tipud, servad, oc);						(* kirjutame l�bim�ngu slaididena faili *)
	Printf.fprintf oc "%s" (MetaPost.failiLopp());						(* kirjutame faili l�pu *)
	close_out oc;																							(* sulgeme v�ljundvoo *)
	
	let mpostTulemus = Sys.command("mpost temp.mp") in				(* tekitame MetaPosti failist PostScripti failid *)
	if mpostTulemus <> 0
		(*then failwith("MetaPostist PostScriptiks tegemine eba�nnestus.");	*)
	then (); 			(* TODO: kodeering korda saada ja eelnev rida tagasi sisse kommenteerida *)	
	
	(* proovime k�ik tekkinud PostScripti failid �heks koondada ja sellest otse PDFi teha *)
		
	let psFail = string_of_algo(!algo) ^ ".1" in 							(* tekitame �he PostScripti faili *)
	koondaYhteFaili(psFail);																	(* kirjutame k�ik tekkinud PostScripti failid sinna kokku *)
	let pdfTulemus = Sys.command("epstopdf " ^ psFail) in			(* teeme EPS failist PDF faili *)
	
	kustutaFail("temp.mp");																		(* kustutame tekkinud failid *)
	kustutaFail("temp.log");
	kustutaFail(psFail);
	
	(* kui ei �nnestunud, siis teeme Postscripti failid �kshaaval PDFideks ja liidame PDFid kokku *)
	
	if pdfTulemus <> 0
		then (
			let koondPDFTulemus = teeKoondPDF() in
			if koondPDFTulemus <> 0
				then failwith("KoondPDFi tegemine eba�nnestus.")
		);
	
	print_endline("PDF valmis.");
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
		| 'f' -> failwith("L�pp.") (* ajutine lahendus akna "ilusaks" sulgemiseks Windowsis *) (* TODO: ebavajalik *)
		| _ -> ();;

let syndmused(algtipp, tipud, servad) =
	let hiirVajutatud = ref(false) in
	while !programmK2ib do
		let e = wait_next_event [Button_down; Button_up; Mouse_motion; Key_pressed] in
		if e.keypressed 
			then klahvisyndmus(e.key, algtipp, tipud, servad)
		else
			if e.button (* kui hiir on alla vajutatud *)
				then (
					if !hiirVajutatud
						then (
							if !liigutatavTipp <> None
								then (
									liigutaTippu(e.mouse_x, e.mouse_y);
									tekst := ""; (* TODO: ajutine, et l�bim�ngul �leliigseid v�lhatr�kkimisi poleks *)
									kuvaPilt(tipud, servad)
								)
							(*else (
								if !liigutatavServ <> None
									then (
										liigutaServa(e.mouse_x, e.mouse_y);
										kuvaPilt(tipud, servad)
									)
							)*)
						) 
					else (	(* kui hiire alla vajutame ja parajasti m�ne tipu v�i serva peal oleme, siis valime selle *)
						hiirVajutatud := true;
						try
							let valitudTipp = List.find (hiirTipul e.mouse_x e.mouse_y) tipud in
							liigutatavTipp := Some valitudTipp
						with
							| Not_found -> () (*(
								try
  								let valitudServ = List.find (hiirServal e.mouse_x e.mouse_y) servad in
									print_endline("SIIN!");
  								liigutatavServ := Some valitudServ
								with
									| Not_found -> ()
							)*)
					)
				)
			else (* hiirt vabastades vabastame ka valitud tipu/serva *)
				if !hiirVajutatud
					then (
						hiirVajutatud := false;
						liigutatavTipp := None
						(*liigutatavServ := None*)
					);
	done;; (* peaks close_graph'iga sulgema, aga Windowsis ei �nnestu *)

(* funktsioon, mis tagastab vastava nimega tipu, kui see leidub, vastasel juhul esimese tipu *)
let valiAlgtipp(nimi, tipud) =
	try
		List.find (fun t -> t.nimi = nimi) tipud
	with
		| Not_found -> List.hd tipud;;	(* TODO: erind, et uuesti prooviks? *)

(* funktsioon, mis kontrollib, et graafi servad oleks kas k�ik suunatud v�i k�ik ilma suunata *)
let kontrolliSuunatust(servad, suunatus) = 
	try
		let sobimatu = List.find (fun s -> s.nool <> suunatus) servad in
		failwith ("Graafi k�ik servad peavad olema " ^ (if suunatus then "suunatud." else "mittesuunatud.") ^ 
			"\nVigane serv: " ^ string_of_serv(sobimatu))
	with
		| Not_found -> ();;

(* funktsioon, mis kontrollib, et graafi servad oleks kas k�ik kaaludega v�i k�ik ilma kaaludeta *)
let kontrolliKaale(servad, kaaludega) = 
		try
			match kaaludega with
				| true -> (
					let sobimatu = List.find (fun s -> match s.kaal with | None -> true | _ -> false) servad in
  				failwith ("Graafi k�ik servad peavad olema kaaludega." ^ "\nVigane serv: " ^ string_of_serv(sobimatu))
				)
				| false -> (
					let sobimatu = List.find (fun s -> match s.kaal with | Some k -> true | _ -> false) servad in
  				failwith ("Graafi k�ik servad peavad olema kaaludega." ^ "\nVigane serv: " ^ string_of_serv(sobimatu))
				)
  	with
  		| Not_found -> ();;

(* funktsioon, mis kontrollib, et graafi tipud oleks kas k�ik hindadega v�i k�ik ilma hidnadeta *)
let kontrolliHindu(tipud, hind) =
	try
			match hind with
				| true -> (
					let sobimatu = List.find (fun t -> match t.hind with | None -> true | _ -> false) tipud in
  				failwith ("Graafi k�ik tipud peavad olema hindadega." ^ "\nVigane tipp: " ^ string_of_tipp(sobimatu))
				)
				| false -> (
					let sobimatu = List.find (fun t -> match t.hind with | Some h -> true | _ -> false) tipud in
  				failwith ("Graafi k�ik servad peavad olema hindadeta." ^ "\nVigane tipp: " ^ string_of_tipp(sobimatu))
				)
  	with
  		| Not_found -> ();;

(* funktsioon, mis kontrollib, kas k�ik servad on suunaga v�i k�ik servad on suunata *)
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
	(* TODO: kaalude m�rk (enamikul >= 0) ja t�klilisuse kontroll ka (topodel ei v�i �ldse, FW-l ei v�i neg ts�kleid) *)

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
  	| Eeldusgraaf -> 		graafiKontroll (algtipp, tipud, servad, Some true, 	false, 	true, 	true) (* TODO: sidusus �ige? *)
  	| Kosaraju -> 			graafiKontroll (algtipp, tipud, servad, Some true, 	false, 	false, 	false);;

(* funktsioon, mis tagastab, kas tegu on algtippu n�udva algoritmiga *)
let algtipugaAlgoritm() =
	let algtipugaAlgoritmid = [Laiuti; SygavutiEes; SygavutiLopp; Prim; Dijkstra; TopoLopp; Kosaraju]  in
	List.mem !algo algtipugaAlgoritmid;;

let alusta(graaf, algtipuNimi) =
	let algtipp = valiAlgtipp(algtipuNimi, graaf.tipud) in
	let tipud = graaf.tipud in
	let servad = graaf.servad in
	
	graafiKontroll(algtipp, tipud, servad);
	let x = (if !algo = FloydWarshall then Graafika.fwLaius else 0) in
	open_graph (" " ^ string_of_int(aknaLaius + x) ^ "x" ^ string_of_int(aknaKorgus + 110));	(* TODO: 110 ajutine *)
	set_window_title ("Graafialgoritmid - " ^ string_of_algo(!algo));
	if algtipugaAlgoritm()
		then algtipp.tv := Valitud; (* m�rgime algtipu valituks, et seda juba 1. slaidil kuvada *)
	kuvaPilt(tipud, servad);
	syndmused(algtipp, tipud, servad);;
	
let main() =
	
	algo := TopoLopp;
	let graaf = ntTopoLopp3 in
	let algtipuNimi = "A" in						(* peab olema ka siis, kui algoritm algtippu ei n�ua *)
	
	alusta(graaf, algtipuNimi);;

		
main();;