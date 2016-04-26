open Graphics;;
open Struktuurid;;

let aknaLaius = 600;;
let aknaKorgus = 550;;
let kirjaaknaKorgus = 150;; (*ebavajalik *)

let tipuRaadius = 20;;

let nooleLaius = 7.;; (* õigupoolest pool laiust *)
let noolePikkus = 20.;;

(* järgmised on mapid serva tippude nimed : int, hoiustamaks ringjoone võrrandit (x+a)^2 + (y+b)^2 = r^2 *)
(*let kaareX = Hashtbl.create 10;;
let kaareY = Hashtbl.create 10;;
let kaareR = Hashtbl.create 10;;
let kaareK = Hashtbl.create 10;;*) (* serva keskpunkti kaugus kaarest (90* all) *)

let radiaanKraadideks(rad) =
	let pi = 3.1415926535897 in
	180. /. pi *. rad;;

(* funktsioon, mis tagastab kahe punkti koordinaadid, mis asuvad sirgel y=ax+c ning on samal sirgel asuvast punktist (x1, y1) kaugusel r*)
let kaksPunkti(x1, y1, a, c, r) =
	let ruutKordaja = a *. a +. 1. in
	let lineaarKordaja = 2. *. (a *. (c -. y1) -. x1) in
	let vabaliige = x1 ** 2. +. y1 ** 2. +. c ** 2. -. r ** 2. -. 2. *. y1 *. c in
	let e1 = (sqrt(lineaarKordaja ** 2. -. 4. *. ruutKordaja *. vabaliige) -. lineaarKordaja) /. (2. *. ruutKordaja) in
	let f1 = a *. e1 +. c in
	let e2 = ((-1.) *. (sqrt(lineaarKordaja ** 2. -. 4. *. ruutKordaja *. vabaliige) +. lineaarKordaja)) /. (2. *. ruutKordaja) in
	let f2 = a *. e2 +. c in
	((e1, f1), (e2, f2));;


(* leiame kaalu jaoks punkti (i, j) nii, et need oleks tippe tipp1 ja tipp2 ühendava serva keskpunktist 90 kraadi all kaugusel "kaugus" ning ei läheks akna äärtest välja *)
(* TODO: väike koodikordus, kasutada ära noole kuvamise abimeetodeid *)
let kaaluKoordinaadid(tipp1, tipp2, kaugus) =
	let x1 = float_of_int(!(!tipp1.x)) in
	let y1 = float_of_int(!(!tipp1.y)) in
	let x2 = float_of_int(!(!tipp2.x)) in
	let y2 = float_of_int(!(!tipp2.y)) in
	let k = float_of_int(kaugus) in
	let e = (x1 +. x2) /. 2. in 			(* serva keskpunkti x *)
	let f = (y1 +. y2) /. 2. in 			(* serva keskpunkti y *)
	let a = (y2 -. y1) /. (x2 -. x1) in 	(* serva tõus *)
	let vahe = a *. k /. sqrt(a ** 2. +. 1.) in
		(*if klauslid vajalikud, sest kui on horisontaalne või vertikaalne serv, siis a = 0 või +- inf ning asi ei töötaks *)
	let imin = if x1 = x2 then (x1 -. k) else min (e -. vahe) (e +. vahe) in
	let jmin = if y1 = y2 then (y1 -. k) else f +. (e -. imin) /. a in
	let imax = if x1 = x2 then (x1 +. k) else max (e -. vahe) (e +. vahe) in
	let jmax = if y1 = y2 then (y1 +. k) else f +. (e -. imax) /. a in (* jmax pole tingimata suurem kui jmin, aga kindlasti imax > imin *)
	if imin >= 0. && jmin >= 0. && jmin < float_of_int(aknaKorgus)
		then (int_of_float(imin), int_of_float(jmin))
	else if imax < float_of_int(aknaLaius) && jmin >= 0. && jmin < float_of_int(aknaKorgus)
		then (int_of_float(imax), int_of_float(jmax))
	else (-1, -1) (*TODO: mingi kontroll/erind siia? Kuigi kui kaugus < (min aknaKorgus aknaLaius) / 2, siis ei tohiks kunagi juhtuda. *)
	
	(*funktsioon, mis leiab sirgega y=ax+c punktis (e, f) ristuva sirge tõusu ja vabaliikme *)
let leiaRistuvSirge(a, c, e, f) = 
	let a2 = (-1.) /. a in
	let c2 = f +. e /. a in
	(a2, c2);;
	
(* Funktsioon, mis tagastab kas p asub t1x ja t2x vahel*)
let kaheTipuVahel(p, t1x, t2x) =
	p > min t1x t2x && p < max t1x t2x;;


(*let nulliKaareAndmed(tipp1, tipp2) =
	let nimi = tipp1.nimi ^ ":" ^ tipp2.nimi in
	Hashtbl.replace kaareX nimi 0;
	Hashtbl.replace kaareY nimi 0;
	Hashtbl.replace kaareR nimi 0;;*)

(*TODO: kirjaaknaKorgust ka arvestada. NB! aknaLaius - tipuRaadius pole päris täpne, vaja kuidagi katsetamisega leida *)
let piiridesX(x) = if x < tipuRaadius then tipuRaadius else if x > aknaLaius - tipuRaadius then aknaLaius - tipuRaadius else x;;
let piiridesY(y) = 
	if y < tipuRaadius (*+ kirjaaknaKorgus*) then tipuRaadius (*+ kirjaaknaKorgus*) 
	else if y > aknaKorgus - tipuRaadius then aknaKorgus - tipuRaadius 
	else y;;


let looTipp(tipuandmed) = 
	match tipuandmed with
		| (tipuNimi, tipuX, tipuY, tipuHind) -> {
			nimi = tipuNimi; (*TODO: peab tagama, et oleks ühekohaline?*)
			x = ref(piiridesX(tipuX));
			y = ref(piiridesY(tipuY (*+ kirjaaknaKorgus*))); (* + kirjaaknaKorgus jätta või mitte? *)
			tv = ref(Vaatlemata);
			hind = tipuHind;
		};;

let looTipud(tipuandmeteList) =
	List.map looTipp tipuandmeteList;;

let leiaVastavaNimegaTipp n tipp = (n = tipp.nimi);;

let sobivKaal(kaal) =
	match kaal with
		| None -> true
		| Some k -> k < max_int && k > min_int;; 
		(* tegelt peaks kuvamise kenaduse huvides ka 2-kohaliseks piirama? Või kaugust kaarest suurendama? *)
	
let looServ(servaandmed, tipud) =
	(*TODO: peaks kontrollima ka seda, et sellist serva juba pole?*)
	match servaandmed with
		| (t1nimi, t2nimi, k, noolTipust) -> ( 
			try
				let t1 = List.find (leiaVastavaNimegaTipp t1nimi) tipud in
				let t2 = List.find (leiaVastavaNimegaTipp t2nimi) tipud in
				if sobivKaal(k) = false
					then failwith("Kaal peab olema väiksem kui maksimaalne ja suurem kui minimaalne täisarv.")
				else (
					(*nulliKaareAndmed(t1, t2);*)
  				Some {
  					tipp1 = ref(t1);
  					tipp2 = ref(t2);
  					kaal = k;
  					nool = noolTipust;
  					sv = ref(Vaatlemata);
  				}
				)
			with
				| Not_found -> (
					failwith("Sellise nimega tippu pole.") (*TODO: kummagi tipu jaoks eraldi?*)
				)
		);;

let rec looServad(servaandmeteList, tipud) =
	match servaandmeteList with
		| x::xs -> (
			match looServ(x, tipud) with
				| Some serv -> serv :: looServad(xs, tipud)
				| None -> looServad(xs, tipud)
		)
		| [] -> [];;

let kuvaTipp(tipp) = 
	set_color (
		match !(tipp.tv) with
			| Vaatlemata -> rgb 252 22 22
			| Vaadeldav -> rgb 253 253 122
			| Valitud -> rgb 253 159 37
			| Vaadeldud -> rgb 22 137 9
			| Sobimatu -> rgb 148 148 148
	);
	fill_circle !(tipp.x) !(tipp.y) tipuRaadius;;
	
(*let kuvaKaar(tipp1, tipp2) = 
	let nimi = tipp1.nimi ^ ":" ^ tipp2.nimi in
	let x = Hashtbl.find kaareX nimi in
	let y = Hashtbl.find kaareY nimi in
	let r = Hashtbl.find kaareR nimi in
	let xvahe1 = abs(!(tipp1.x) - x) in
	let yvahe1 = abs(!(tipp1.y) - y) in
	let xvahe2 = abs(!(tipp2.x) - x) in
	let yvahe2 = abs(!(tipp2.y) - y) in
	let alfa = int_of_float (radiaanKraadideks(acos (float_of_int(yvahe1) /. float_of_int(xvahe1)))) in
	let beeta = int_of_float (radiaanKraadideks(acos (float_of_int(yvahe2) /. float_of_int(xvahe2)))) in
	draw_arc x y r r alfa (90 + beeta);;*)
		
let kuvaServ(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; sv = v} -> (
				(*if Hashtbl.find kaareR (!t1.nimi ^ ":" ^ !t2.nimi) = 0
					then ( (*kui raadius on 0, siis järelikult sirgjoon, mitte kaar*)*)
  					moveto !(!t1.x) !(!t1.y);
  					lineto !(!t2.x) !(!t2.y)
					(* )
				else kuvaKaar(!t1, !t2)*)
		);;
		
let kuvaNimi(tipp) =
	moveto (!(tipp.x)-3) (!(tipp.y)-7); (*TODO: panna mahalahutamine raadiusest sõltuma *)
	draw_string tipp.nimi;;
	
let kuvaKaal(serv) =
	match serv with
		| {tipp1 = a; tipp2 = b; kaal = k; sv = v} -> (
			match k with
				| Some ka -> (
					let koordinaadid = kaaluKoordinaadid(a, b, 20) in (*TODO: see kaugus 20 on omavoliline *)
					match koordinaadid with
						| (i, j) -> moveto i j;
					draw_string (string_of_int(ka))
				)
				| None -> () (* kas see haru on üldse vajalik? OCamlis pole exhaustive probleeme? *)
		);;
		
let kuvaNool(serv) =
	let fromTipp = serv.tipp1 in
	let toTipp = serv.tipp2 in
	let x1 = float_of_int(!(!fromTipp.x)) in
	let y1 = float_of_int(!(!fromTipp.y)) in
	let x2 = float_of_int(!(!toTipp.x)) in
	let y2 = float_of_int(!(!toTipp.y)) in
	let tr = float_of_int(tipuRaadius) in
	if x1 = x2 (* erijuht vertikaalse serva jaoks *)
		then
			let i = if kaheTipuVahel(y2 +. tr, y1, y2) then y2 +. tr else y2 -. tr in
			let j = if kaheTipuVahel(y2 +. tr +. noolePikkus, y1, y2) then y2 +. tr +. noolePikkus else y2 -. tr -. noolePikkus in
			fill_poly [|int_of_float(x2), int_of_float(i); int_of_float(x2 -. nooleLaius), int_of_float(j); int_of_float(x2 +. nooleLaius), int_of_float(j)|]
	else if y1 = y2 (* erijuht horisontaalse serva jaoks *)
		then 
			let k = if kaheTipuVahel(x2 +. tr, x1, x2) then x2 +. tr else x2 -. tr in
			let l = if kaheTipuVahel(x2 +. tr +. noolePikkus, x1, x2) then x2 +. tr +. noolePikkus else x2 -. tr -. noolePikkus in
			fill_poly [|int_of_float(k), int_of_float(y2); int_of_float(l), int_of_float(y2 -. nooleLaius); int_of_float(l), int_of_float(y2 +. nooleLaius)|]
	else
	let a = (y2 -. y1) /. (x2 -. x1) in
	let c = y1 -. a *. x1 in
	match kaksPunkti(x2, y2, a, c, float_of_int(tipuRaadius) +. noolePikkus) with
		| ((e1, f1), (e2, f2)) -> 
			let e = if kaheTipuVahel(e1, x1, x2) then e1 else e2 in
			let f = if kaheTipuVahel(e1, x1, x2) then f1 else f2 in
			let ristuvSirge =  leiaRistuvSirge(a, c, e, f) in
			match ristuvSirge with
				| (a2, c2) -> let punktid = kaksPunkti(e, f, a2, c2, nooleLaius) in
					match punktid with ((p1, p2), (p3, p4)) -> let nooletipp = kaksPunkti(x2, y2, a, c, float_of_int(tipuRaadius)) in
						match nooletipp with
							| ((nx1, ny1), (nx2, ny2)) -> 
								let nx = if kaheTipuVahel(nx1, x1, x2) then nx1 else nx2 in
								let ny = if kaheTipuVahel(e1, x1, x2) then ny1 else ny2 in
								fill_poly [|int_of_float(p1), int_of_float(p2); int_of_float(p3), int_of_float(p4); int_of_float(nx), int_of_float(ny)|];;
		
let kuvaServJaNool(serv) =
	set_color (
		match !(serv.sv) with
			| Vaatlemata -> rgb 150 4 4
			| Vaadeldav -> rgb 240 240 3
			| Valitud -> rgb 253 144 1
			| Vaadeldud -> rgb 78 247 59
			| Sobimatu -> rgb 148 148 148
	);
	kuvaServ(serv);
	if serv.nool = true then kuvaNool(serv);;

let kuvaHind(tipp) =
	match tipp.hind with
		| None -> ()
		| Some h -> (
			moveto (!(tipp.x) - 5) (!(tipp.y) + 20); (* TODO: 5 ja 20 suvalt võetud, panna sõltuma *)
			draw_string (string_of_int(h))
		);;
		
let kuvaTipud(tipud) = 
	List.iter kuvaTipp tipud;;
	
(*let kuvaServad(servad) =
	set_line_width 3;
	List.iter kuvaServ servad;;*)
	
let kuvaNimed(tipud) =
	set_color black;
	set_text_size 5;
	set_font "Courier-Bold";
	List.iter kuvaNimi tipud;;
	
let kuvaKaalud(servad) =
	set_color black;
	List.iter kuvaKaal servad;;
	
let kuvaHinnad(tipud) =
	set_color black;
	List.iter kuvaHind tipud;;
	
let kuvaServadJaNooled(servad) =
	set_line_width 3;
	List.iter kuvaServJaNool(servad);;
	
(*ebavajalik*)
let kuvaVahe() = (*kirjaaken ja graafiakent lahutav joon *)
	set_color black;
	set_line_width 1;
	moveto 0 kirjaaknaKorgus;
	lineto aknaLaius kirjaaknaKorgus;;
	
let kuvaTekst() =
	(*TODO: alles slaidile *)
	set_color black;
	set_font "Courier-Bold";
	(*moveto 0 100;
	draw_string !(AlgoBaas.tekst);;*)
	if !(AlgoBaas.tekst) <> ""
		then print_endline(!(AlgoBaas.tekst) ^ "\n");;
	
let kuvaPilt(tipud, servad) =
	clear_graph();
	kuvaServadJaNooled(servad);
	kuvaTipud(tipud);
	kuvaNimed(tipud);
	kuvaKaalud(servad);
	kuvaHinnad(tipud);
	(*kuvaVahe();*)
	kuvaTekst();;

(*TODO: eraldi fn randomilt tippude loomiseks, ainult etteantud arvuga? *)