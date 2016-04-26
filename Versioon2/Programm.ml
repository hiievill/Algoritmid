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
	abs_float (a *. x +. c -. y) <= 1.;;		(* <= 1, sest vahel teen floate intideks, seega täpne ei pruugiks sobida *) 	

(* funktsioon, mis tagastab, kas punktis (hx, hy) asuv hiir asub sirgel, mis ühendab tippe t1 ja t2 *)
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

(*let prindiNumbrid(oo, pildimaatriks, laius, pikkus) =
	Printf.fprintf oo "%d %d\n" laius pikkus;
	Array.iter (fun a -> Array.iter (...) a) pilt;;
	(*tekst ka lisada iga pildi juurde!*)

let kirjutaFaili(pilt) =
	let l = Array.length pilt in (*kuigi sama mis laius?*)
	let p = Array.length (Array.get pilt 0) in (*sama mis pikkus? size_y vms *)
	let oo = open_out "pildid.txt" in
  prindiNumbrid(oo, pilt, l, p);
  close_out oo;;*)


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
		| Eeldusgraaf -> Eeldusgraaf.eeldusgraaf(algtipp, tipud, servad)
		| Kosaraju -> Kosaraju.kosaraju(algtipp, tipud, servad);;

let programmK2ib = ref(true);;

(* funktsioon klahvivajutustele reageerimiseks *)
let klahvisyndmus(klahv, algtipp, tipud, servad) = 
	match klahv with
		| 'q' -> programmK2ib := false (*close_graph() *)
		| 's' -> (
			while !algoL2bi = false
				do
					samm(algtipp, tipud, servad);
					kuvaPilt(tipud, servad)
					(* TODO: peab mingit pausi ka pidama? *)
					(*let pilt = dump_image (get_image 0 0 aknaLaius aknaKorgus) in
					kirjutaFaili(pilt)*)
				done
		)
		| 'n' -> if !algoL2bi = false then (
			samm(algtipp, tipud, servad);
			kuvaPilt(tipud, servad)
			(*let pilt = dump_image (get_image 0 0 aknaLaius aknaKorgus) in
			kirjutaFaili(pilt)*)
		)
		| 'f' -> failwith("Lõpp.") (* ajutine lahendus akna "ilusaks" sulgemiseks Windowsis *) (* TODO: ebavajalik *)
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
									tekst := ""; (* TODO: ajutine, et läbimängul üleliigseid välhatrükkimisi poleks *)
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
					else (	(* kui hiire alla vajutame ja parajasti mõne tipu või serva peal oleme, siis valime selle *)
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
	done;; (* peaks close_graph'iga sulgema, aga Windowsis ei õnnestu *)

(* funktsioon, mis tagastab vastava nimega tipu, kui see leidub, vastasel juhul esimese tipu *)
let valiAlgtipp(nimi, tipud) =
	try
		List.find (fun t -> t.nimi = nimi) tipud
	with
		| Not_found -> List.hd tipud;;	(* TODO: erind, et uuesti prooviks? *)

let alusta(graaf, algtipp) =
	(*try*)
		let tipud = graaf.tipud in
		let servad = graaf.servad in
		open_graph (" " ^ string_of_int(aknaLaius) ^ "x" ^ string_of_int(aknaKorgus));
		set_window_title ("Graafialgoritmid - " ^ string_of_algo(!algo));
		kuvaPilt(tipud, servad);
		syndmused(algtipp, tipud, servad);;
	(*with
		| Graphic_failure("fatal I/O error") -> ();
		| Graphic_failure("graphic screen not opened") -> ();;*)
	
let main() =
	algo := Eeldusgraaf;
	let graaf = ntEeldusgraaf1 in
	let algtipp = valiAlgtipp("A", graaf.tipud) in
	alusta(graaf, algtipp);;

		
main();;