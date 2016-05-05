open Graphics;;
open Struktuurid;;

let aknaLaius = 600;;
let aknaKorgus = 450;;
let korgusLisa = 110;; 	(* nimekirjade kuvamiseks *)
let laiusLisa = 400;; 	(* Floyd-Warshalli tabeli kuvamiseks *)
let tekstiLisa = 100;; 	(* slaididel pildi all teksti kuvamiseks *)

let tipuRaadius = 20;;

let nooleLaius = 7.;; (* õigupoolest pool laiust *)
let noolePikkus = 20.;;

let jooneLaius = 3;;

(* järgmised on mapid serva tippude nimed : int, hoiustamaks ringjoone võrrandit (x+a)^2 + (y+b)^2 = r^2 *)
let kaareX = Hashtbl.create 10;;
let kaareY = Hashtbl.create 10;;
let kaareR = Hashtbl.create 10;;
let kaareK = Hashtbl.create 10;; (* tippe ühendava lõigu keskpunkti minimaalne kaugus kaarest (90 kraadi all) *)

let radiaanKraadideks(rad) =
	let pi = 3.1415926535897 in
	180. /. pi *. rad;;

(* funktsioon, mis tagastab kahe punkti koordinaadid, mis asuvad sirgel y=ax+c ning on samal sirgel asuvast punktist (x1, y1) kaugusel r*)
let kaksPunkti(x1, y1, a, c, r) =
	(* TODO: jätta? assert (abs(a *. x1 +. c - y) < 2.); *)
	let ruutKordaja = a *. a +. 1. in
	let lineaarKordaja = 2. *. (a *. (c -. y1) -. x1) in
	let vabaliige = x1 ** 2. +. y1 ** 2. +. c ** 2. -. r ** 2. -. 2. *. y1 *. c in
	let e1 = (sqrt(lineaarKordaja ** 2. -. 4. *. ruutKordaja *. vabaliige) -. lineaarKordaja) /. (2. *. ruutKordaja) in
	let f1 = a *. e1 +. c in
	let e2 = ((-1.) *. (sqrt(lineaarKordaja ** 2. -. 4. *. ruutKordaja *. vabaliige) +. lineaarKordaja)) /. (2. *. ruutKordaja) in
	let f2 = a *. e2 +. c in
	((e1, f1), (e2, f2));;

(* funktsioon, mis tagastab kahe punkti vahelise kauguse *)
let leiaJoonePikkus(x1, y1, x2, y2) =
	sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.);;


(* leiame kaalu jaoks punkti (i, j) nii, et need oleks tippe tipp1 ja tipp2 ühendava serva keskpunktist 90 kraadi all kaugusel "kaugus" ning ei läheks akna äärtest välja *)
(* TODO: väike koodikordus, kasutada ära noole kuvamise abimeetodeid *)
let leiaKaaluKoordinaadid(tipp1, tipp2, kaugus) =
	let kaareKaugus = Hashtbl.find kaareK (!tipp1.nimi ^ ":" ^ !tipp2.nimi) in
	let x1 = float_of_int(!(!tipp1.x)) in
	let y1 = float_of_int(!(!tipp1.y)) in
	let x2 = float_of_int(!(!tipp2.x)) in
	let y2 = float_of_int(!(!tipp2.y)) in
	let k = float_of_int(kaugus + kaareKaugus) in
	let e = (x1 +. x2) /. 2. in 			(* serva keskpunkti x *)
	let f = (y1 +. y2) /. 2. in 			(* serva keskpunkti y *)
	let a = (y2 -. y1) /. (x2 -. x1) in 	(* serva tõus *)
	let vahe = a *. k /. sqrt(a ** 2. +. 1.) in
		(*if klauslid vajalikud, sest kui on horisontaalne või vertikaalne serv, siis a = 0 või +- inf ning asi ei töötaks *)
	let imin = if x1 = x2 then (x1 -. k) else min (e -. vahe) (e +. vahe) in
	let jmin = if y1 = y2 then (y1 -. k) else f +. (e -. imin) /. a in
	let imax = if x1 = x2 then (x1 +. k) else max (e -. vahe) (e +. vahe) in
	let jmax = if y1 = y2 then (y1 +. k) else f +. (e -. imax) /. a in (* jmax pole tingimata suurem kui jmin, aga kindlasti imax > imin *)
	
	if kaareKaugus <> 0
		then (		(* valime selle punkti, mis on ringjoone keskpunktist kaugemal, et kaare juurde paigutuks *)
			let xr = float_of_int(Hashtbl.find kaareX (!tipp1.nimi ^ ":" ^ !tipp2.nimi)) in
			let yr = float_of_int(Hashtbl.find kaareY (!tipp1.nimi ^ ":" ^ !tipp2.nimi)) in
			if leiaJoonePikkus(imin, jmin, xr, yr) >= leiaJoonePikkus(imax, jmax, xr, yr)
				then  (int_of_float(imin), int_of_float(jmin))
			else (int_of_float(imax), int_of_float(jmax))
		)
	else if imin >= 0. && jmin >= 0. && jmin < float_of_int(aknaKorgus)
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

(* funktsioon, mis uuendab kaare kõiki andmeid *)
let uuendaKaareAndmeid(tipp1, tipp2, x, y, r, k) =
	let nimi = tipp1.nimi ^ ":" ^ tipp2.nimi in
	Hashtbl.replace kaareX nimi x;
	Hashtbl.replace kaareY nimi y;
	Hashtbl.replace kaareR nimi r;
	Hashtbl.replace kaareK nimi k;;
	

(*TODO: kirjaaknaKorgust ka arvestada. NB! aknaLaius - tipuRaadius pole päris täpne, vaja kuidagi katsetamisega leida *)
let piiridesX(x) = if x < tipuRaadius then tipuRaadius else if x > aknaLaius - tipuRaadius then aknaLaius - tipuRaadius else x;;
let piiridesY(y) = if y < tipuRaadius then tipuRaadius else if y > aknaKorgus - tipuRaadius then aknaKorgus - tipuRaadius else y;;

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
	if List.length tipuandmeteList = 0
		then failwith("Graafis peab olema vähemalt üks tipp.");
	List.map looTipp tipuandmeteList;;

let leiaVastavaNimegaTipp n tipp = (n = tipp.nimi);;

let sobivKaal(kaal) =
	match kaal with
		| None -> true
		| Some k -> k < max_int && k > min_int;; 
		(* TODO: tegelt peaks kuvamise kenaduse huvides ka 2-kohaliseks piirama? Või kaugust kaarest suurendama? *)
	
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
					uuendaKaareAndmeid(t1, t2, 0, 0, 0, 0);
  				{
  					tipp1 = ref(t1);
  					tipp2 = ref(t2);
  					kaal = k;
  					nool = noolTipust;
  					sv = ref(Vaatlemata);
  				}
				)
			with
				| Not_found -> (
					failwith("Serva ei saa luua, sellise nimega tippu pole.")
				)
		);;

let rec looServad(servaandmeteList, tipud) =
	List.map (fun sa -> looServ(sa, tipud)) servaandmeteList;;

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

(* funktsioon, mis leiab sirge y=ax+c, millel asuvad tipud (x1;y1) ja (x2;y2), ja tagastab (a,c) *)
(* TODO: argumendid float'id, mitte int'id *)
let leiaSirge(x1, y1, x2, y2) =
	let a = (y2 -. y1) /. (x2 -. x1) in
	let c = (y1) -. a *. (x1) in
	(a, c);;
	
let leiaKaareAndmed(tipp1, tipp2) =
	let nimi = tipp1.nimi ^ ":" ^ tipp2.nimi in
	let xr = float_of_int(Hashtbl.find kaareX nimi) in
	let yr = float_of_int(Hashtbl.find kaareY nimi) in
	let r = float_of_int(Hashtbl.find kaareR nimi) in
	let minX = float_of_int(min !(tipp1.x) !(tipp2.x)) in
	let maxX = float_of_int(max !(tipp1.x) !(tipp2.x)) in
	let minY = float_of_int(min !(tipp1.y) !(tipp2.y)) in
	let maxY = float_of_int(max !(tipp1.y) !(tipp2.y)) in
	let (a, c) = leiaSirge(float_of_int(!(tipp1.x)), float_of_int(!(tipp1.y)), float_of_int(!(tipp2.x)), float_of_int(!(tipp2.y))) in
	if minX = maxX				(* tipud asuvad vertikaalselt *)
		then (
			let alfa = int_of_float(radiaanKraadideks(asin ((maxY -. yr) /. r))) in
			let algusKraad = if xr < minX then 0 else 180 in	(* kui ringjoone keskpunkt on tippudest vasakul, siis 0, else 180 *)
			(int_of_float(xr), int_of_float(yr), int_of_float(r), int_of_float(r), (algusKraad - alfa), (algusKraad + alfa))
		)
	else if minY = maxY		(* tipud asuvad horisontaalselt *)
		then (
			let alfa = int_of_float(radiaanKraadideks(asin ((maxX -. xr) /. r))) in
			let algusKraad = if yr < minY then 90 else 270 in	(* kui ringjoone keskpunkt on tippudest all, siis 90, else 270 *)
			(int_of_float(xr), int_of_float(yr), int_of_float(r), int_of_float(r), (algusKraad - alfa), (algusKraad + alfa))
		)
	else if a > 0. && a *. xr +. c > yr				(* tipud tõusval sirgel, ringi keskpunkt joonest all *)
		then (
			let alfa = int_of_float(radiaanKraadideks(asin ((xr -. maxX) /. r))) in
			let beeta = int_of_float(radiaanKraadideks(asin ((minY -. yr) /. r))) in
			(int_of_float(xr), int_of_float(yr), int_of_float(r), int_of_float(r), (90 + alfa), (180 - beeta))
		)
	else if a > 0. && a *. xr +. c < yr		(* tipud tõusval sirgel, ringi keskpunkt joonest üleval *)
		then (
			let alfa = int_of_float(radiaanKraadideks(asin ((yr -. maxY) /. r))) in
			let beeta = int_of_float(radiaanKraadideks(asin ((minX -. xr) /. r))) in
			(int_of_float(xr), int_of_float(yr), int_of_float(r), int_of_float(r), (270 + beeta), (360 - alfa))
		)
	else if a < 0. && a *. xr +. c < yr		(* tipud langeval sirgel, ringi keskpunkt joonest üleval *)
		then (
			let alfa = int_of_float(radiaanKraadideks(asin ((yr -. maxY) /. r))) in
			let beeta = int_of_float(radiaanKraadideks(asin ((xr -. maxX) /. r))) in
			(int_of_float(xr), int_of_float(yr), int_of_float(r), int_of_float(r), (180 + alfa), (270 - beeta))
		)
	else if a < 0. && a *. xr +. c > yr		(* tipud langeval sirgel, ringi keskpunkt joonest all *)
		then (
			let alfa = int_of_float(radiaanKraadideks(asin ((minY -. yr) /. r))) in
			let beeta = int_of_float(radiaanKraadideks(asin ((minX -. xr) /. r))) in
			(int_of_float(xr), int_of_float(yr), int_of_float(r), int_of_float(r), alfa, (90 - beeta))
		)
	else (0, 0, 0, 0, 0, 0);; (* ei tohiks siia jõuda *)

let kuvaKaar(tipp1, tipp2) =
	let (xr, yr, r1, r2, nurk1, nurk2) = leiaKaareAndmed(tipp1, tipp2) in
	draw_arc xr yr r1 r2 nurk1 nurk2;;
		
let kuvaServ(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; sv = v} -> (
				if Hashtbl.find kaareR (!t1.nimi ^ ":" ^ !t2.nimi) = 0
					then ( (*kui raadius on 0, siis järelikult sirgjoon, mitte kaar*)
  					moveto !(!t1.x) !(!t1.y);
  					lineto !(!t2.x) !(!t2.y)
					 )
				else kuvaKaar(!t1, !t2)
		);;
		
let kuvaNimi(tipp) =
	moveto (!(tipp.x)-3) (!(tipp.y)-7); (*TODO: panna mahalahutamine raadiusest sõltuma *)
	draw_string tipp.nimi;;
	
let kuvaKaal(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; kaal = k; sv = v} -> (
			match k with
				| Some ka -> (
					let (x, y) = leiaKaaluKoordinaadid(t1, t2, 20) in (*TODO: see kaugus 20 on omavoliline *)
					moveto x y;
					draw_string (string_of_int(ka))
				)
				| None -> ()
		);;

(* funktsioon, mis tagastab noole koordinaadid sirge serva puhul *)
let leiaNooleKoordinaadidSirge(serv) =
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
			(int_of_float(x2), int_of_float(i), int_of_float(x2 -. nooleLaius), int_of_float(j), int_of_float(x2 +. nooleLaius), int_of_float(j))
	else if y1 = y2 (* erijuht horisontaalse serva jaoks *)
		then 
			let k = if kaheTipuVahel(x2 +. tr, x1, x2) then x2 +. tr else x2 -. tr in
			let l = if kaheTipuVahel(x2 +. tr +. noolePikkus, x1, x2) then x2 +. tr +. noolePikkus else x2 -. tr -. noolePikkus in
			(int_of_float(k), int_of_float(y2), int_of_float(l), int_of_float(y2 -. nooleLaius), int_of_float(l), int_of_float(y2 +. nooleLaius))
	else
	let a = (y2 -. y1) /. (x2 -. x1) in
	let c = y1 -. a *. x1 in
	let ((e1, f1), (e2, f2)) = kaksPunkti(x2, y2, a, c, float_of_int(tipuRaadius) +. noolePikkus) in
	let e = if kaheTipuVahel(e1, x1, x2) then e1 else e2 in
	let f = if kaheTipuVahel(e1, x1, x2) then f1 else f2 in
	let (a2, c2) =  leiaRistuvSirge(a, c, e, f) in																						(* ristuv sirge *)
	let ((p1, p2), (p3, p4)) = kaksPunkti(e, f, a2, c2, nooleLaius) in												(* punktid *)
	let ((nx1, ny1), (nx2, ny2)) = kaksPunkti(x2, y2, a, c, float_of_int(tipuRaadius)) in			(* nooletipp *)
	let nx = if kaheTipuVahel(nx1, x1, x2) then nx1 else nx2 in
	let ny = if kaheTipuVahel(e1, x1, x2) then ny1 else ny2 in
	(int_of_float(p1), int_of_float(p2), int_of_float(p3), int_of_float(p4), int_of_float(nx), int_of_float(ny));;

(* funktsioon, mis tagastab noole koordinaadid kaardus serva puhul *)
let leiaNooleKoordinaadidKaar(serv) =
	(*let nimi = !(serv.tipp1).nimi ^ ":" ^ !(serv.tipp2).nimi in
	let x1 = float_of_int(!(!(serv.tipp2).x)) in
	let x2 = float_of_int(Hashtbl.find kaareX nimi) in
	let r1 = float_of_int(tipuRaadius + noolePikkus) in
	let y1 = float_of_int(!(!(serv.tipp2).y)) in
	let y2 = float_of_int(Hashtbl.find kaareY nimi) in
	let r2 = float_of_int(Hashtbl.find kaareR nimi) in
	(* ringjoonte võrrandid: (x-x1)^2 + (y-y1)^2 = (r1)^2 ja (x-x2)^2 + (y-y2)^2 = (r2)^2 *)
	(* allikas: http://math.stackexchange.com/a/256123 *)
	let v = (r1 ** 2. -. r2 ** 2.) -. (x1 ** 2. -. x2 ** 2.) -. (y1 ** 2. -. y2 ** 2.) in
	let y = ((-0.5) *. v) -. x  (*TODO *)*)
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> (
			let nimi = !t1.nimi ^ ":" ^ !t2.nimi in
			let x1 = float_of_int(!(!t1.x)) in
			let x2 = float_of_int(!(!t1.y)) in
			let y1 = float_of_int(!(!t2.x)) in
			let y2 = float_of_int(!(!t2.y)) in
			let rx = float_of_int(Hashtbl.find kaareX nimi) in
			let ry = float_of_int(Hashtbl.find kaareY nimi) in
			let k = float_of_int(Hashtbl.find kaareK nimi) in
			let keskX = (x1 +. x2) /. 2. in
			let keskY = (y1 +. y2) /. 2. in
			let (a, c) = leiaSirge(keskX, keskY, rx, ry) in						(* leiame 2 tipu vahelise lõigu keskristsirge *)
			let ((e1, f1), (e2, f2)) = kaksPunkti(keskX, keskY, a, c, 2. *. k) in (* TODO: 2 * k on omavoliliselt valitud *)
			let (e, f) = if leiaJoonePikkus(e1, f1, rx, ry) >= leiaJoonePikkus(e2, f2, rx, ry) then (e1, f1) else (e2, f2) in	(* valime ringjoone keskpunktist kaugema punkti *)
			let (a2, c2) = leiaSirge(e, f, float_of_int(!(!t2.x)), float_of_int(!(!t2.y))) in
			let ((e3, f3), (e4, f4)) = kaksPunkti(float_of_int(!(!t2.x)), float_of_int(!(!t2.y)), a2, c2, float_of_int(tipuRaadius)) in
			let (p1, p2) = if leiaJoonePikkus(e3, f3, keskX, keskY) <= leiaJoonePikkus(e4, f4, keskX, keskY) then (e3, f3) else (e4, f4) in (* noole tipp *)
			let ((e5, f5), (e6, f6)) = kaksPunkti(float_of_int(!(!t2.x)), float_of_int(!(!t2.y)), a2, c2, float_of_int(tipuRaadius) +. noolePikkus) in
			let (kx, ky) = if leiaJoonePikkus(e5, f5, keskX, keskY) <= leiaJoonePikkus(e6, f6, keskX, keskY) then (e5, f5) else (e6, f6) in (* noole serva keskpunkt *)
			let (a3, c3) = leiaRistuvSirge(a2, c2, kx, ky) in
			let ((p3, p4), (p5, p6)) = kaksPunkti(kx, ky, a3, c3, nooleLaius) in
			(int_of_float(p1), int_of_float(p2), int_of_float(p3), int_of_float(p4), int_of_float(p5), int_of_float(p6))
		)
	
	

(* funktsioon, mis tagastab noole koordinaadid (3 punkti koordinaatteljestikul) *)
let leiaNooleKoordinaadid(serv) =
	let nimi = !(serv.tipp1).nimi ^ ":" ^ !(serv.tipp2).nimi in
	if Hashtbl.find kaareR nimi = 0 then leiaNooleKoordinaadidSirge(serv) else leiaNooleKoordinaadidKaar(serv);;
		
let kuvaNool(serv) =
	let (p1, p2, p3, p4, p5, p6) = leiaNooleKoordinaadid(serv) in
	fill_poly [|p1, p2; p3, p4; p5, p6|];; 
		
let kuvaServJaNool(serv) =
	set_color (
		match !(serv.sv) with
			| Vaatlemata -> rgb 150 4 4
			| Vaadeldav -> rgb 240 240 3
			| Valitud -> rgb 253 144 1
			| Vaadeldud -> rgb 78 247 59
			| Sobimatu -> rgb 67 67 67
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
	
let kuvaNimed(tipud) =
	set_color black;
	List.iter kuvaNimi tipud;;
	
let kuvaKaalud(servad) =
	set_color black;
	List.iter kuvaKaal servad;;
	
let kuvaHinnad(tipud) =
	set_color black;
	List.iter kuvaHind tipud;;
	
let kuvaServadJaNooled(servad) =
	set_line_width jooneLaius;
	List.iter kuvaServJaNool(servad);;

let kuvaNimekirjad() =
	moveto 10 (aknaKorgus + korgusLisa - 60);
	draw_string !AlgoBaas.nk1;
	moveto 10 (aknaKorgus + korgusLisa - 80);
	draw_string !AlgoBaas.nk2;
	moveto 10 (aknaKorgus + korgusLisa - 100);
	draw_string !AlgoBaas.nk3;;
	
let kuvaTekst() =
	set_color black;
	if !(AlgoBaas.tekst) <> ""
		then print_endline(!(AlgoBaas.tekst) ^ "\n");;

let kuvaTabel(tipud, servad) =
	set_line_width 2;
	(* TODO: tabeli algkoordinaadid tippude arvust sõltuma? *)
	let xAlgus = aknaLaius + 10 in	(* x algkoordinaat - tabeli vasak ülemine nurk *)
	let yAlgus = 400 in							(* y algkoordinaat *)
	let tippudeArv = List.length tipud in
	let a = 30 in (* väikse ruudu külje suurus NB! fill_rect alustab just vasakust alumisest nurgast *)
	if !(FloydWarshall.fiks) >= 0 && !(FloydWarshall.fiks) < tippudeArv 
		then (																				(* fikseeritud rea ja veeru kuvamine *)
			set_color (rgb 253 253 122);
			fill_rect xAlgus (yAlgus - (!(FloydWarshall.fiks) + 2) * a) ((tippudeArv + 1) * a) a;	(* horisontaalne *)
			fill_rect (xAlgus + (!(FloydWarshall.fiks) + 1) * a) (yAlgus - (tippudeArv + 1) * a) a ((tippudeArv + 1) * a);	(* vertikaalne *)
		);
	if !(FloydWarshall.x) >= 0 && !(FloydWarshall.y) >= 0 && !(FloydWarshall.x) <= tippudeArv - 1 
		then (																				(* vaadeldava lahtri kuvamine *)
			set_color (rgb 253 159 37);
			fill_rect (xAlgus + (!(FloydWarshall.y) + 1) * a) (yAlgus - (!(FloydWarshall.x) + 2) * a) a a;
		);
	set_color black;
	(* horisontaalsed jooned *)
	let x = ref(xAlgus) in
	let y = ref(yAlgus) in
	for i = 0 to (tippudeArv + 1)
	do
		moveto !x !y;
		lineto (!x + (tippudeArv + 1) * a) !y;
		y := !y - a
	done;
	(* vertikaalsed jooned *)
	let x = ref(xAlgus) in
	let y = ref(yAlgus) in
	for i = 0 to (tippudeArv + 1)
	do
		moveto !x !y;
		lineto !x (!y - (tippudeArv + 1) * a);
		x := !x + a
	done;
	(* horisontaalsed tähed *)
	let x = ref(xAlgus + a + 5) in
	let y = ref(yAlgus - a + 5) in
	for i = 0 to (tippudeArv - 1)
	do
		moveto !x !y;
		draw_string((List.nth tipud i).nimi);
		x := !x + a
	done;
	(* vertikaalsed tähed *)
	let x = ref(xAlgus + 5) in
	let y = ref(yAlgus - 2 * a + 5) in
	for i = 0 to (tippudeArv - 1)
	do
		moveto !x !y;
		draw_string((List.nth tipud i).nimi);
		y := !y - a
	done;
	(* tabel ise *)
	if !(FloydWarshall.tabel) <> [||]
		then (
    	let x = ref(xAlgus + a + 5) in
    	let y = ref(yAlgus - 2 * a + 5) in
    	for i = 0 to (tippudeArv - 1)
    	do
    		for j = 0 to (tippudeArv - 1)
    		do
    			moveto !x !y;
    			let kaal = !(FloydWarshall.tabel).(i).(j) in
    			draw_string(FloydWarshall.string_of_lahter(kaal));
    			x := !x + a
    		done;
    		y := !y - a;
    		x := xAlgus + a + 5
    	done
		);;

	
let kuvaPilt(tipud, servad) =
	clear_graph();
	kuvaServadJaNooled(servad);
	kuvaTipud(tipud);
	kuvaNimed(tipud);
	kuvaKaalud(servad);
	kuvaHinnad(tipud);
	kuvaNimekirjad();
	if !(AlgoBaas.algo) = FloydWarshall
		then kuvaTabel(tipud, servad);
	kuvaTekst();;

(*TODO: eraldi fn randomilt tippude loomiseks, ainult etteantud arvuga? *)