(* moodul Graafika vastutab pildi kuvamise eest programmiaknas. Siin on funktsioonid tippude, servade, kaalude, tabeli,*)
(* hindade jms kuvamiseks ning nende koordinaatide arvutamiseks. *)

open Graphics;;
open Struktuurid;;
open AlgoBaas;;

let aknaLaius = 600;;
let aknaKorgus = 450;;
let korgusLisa = 130;; 	(* nimekirjade kuvamiseks *)
let laiusLisa = 400;; 	(* Floyd-Warshalli tabeli kuvamiseks *)
let tekstiLisa = 100;; 	(* slaididel pildi all teksti kuvamiseks *)

let tipuRaadius = 20;;

let nooleLaius = 7.;;		(* õigupoolest pool laiust *)
let noolePikkus = 20.;;

let jooneLaius = 3;;

(* funktsioon, mis teisendab radiaanid kraadideks *)
let radiaanKraadideks(rad) =
	let pi = 3.1415926535897 in
	180. /. pi *. rad;;

(* funktsioon, mis leiab sirge y=ax+c, millel asuvad tipud (x1, y1) ja (x2, y2), ja tagastab (a,c) *)
let leiaSirge(x1, y1, x2, y2) =
	let a = (y2 -. y1) /. (x2 -. x1) in
	let c = (y1) -. a *. (x1) in
	(a, c);;

(* funktsioon, mis tagastab kahe punkti vahelise kauguse *)
let leiaJoonePikkus(x1, y1, x2, y2) =
	sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.);;

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
	let imin = if x1 = x2 then (x1 -. k) else min (e -. vahe) (e +. vahe) in
	let jmin = if y1 = y2 then (y1 -. k) else f +. (e -. imin) /. a in
	let imax = if x1 = x2 then (x1 +. k) else max (e -. vahe) (e +. vahe) in
	let jmax = if y1 = y2 then (y1 +. k) else f +. (e -. imax) /. a in
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

(* funktsioon, mis võtab argumendiks x-koordinaadi ja tagab, et see oleks pildi piirides *)
let piiridesX(x) = if x < tipuRaadius then tipuRaadius else if x > aknaLaius - tipuRaadius then aknaLaius - tipuRaadius else x;;

(* funktsioon, mis võtab argumendiks y-koordinaadi ja tagab, et see oleks pildi piirides *)
let piiridesY(y) = if y < tipuRaadius then tipuRaadius else if y > aknaKorgus - tipuRaadius then aknaKorgus - tipuRaadius else y;;

(* funktsioon tipu loomiseks *)
let looTipp(tipuandmed) = 
	match tipuandmed with
		| (tipuNimi, tipuX, tipuY, tipuHind) -> {
			nimi = tipuNimi; (*TODO: peab tagama, et oleks ühekohaline?*)
			x = ref(piiridesX(tipuX));
			y = ref(piiridesY(tipuY (*+ kirjaaknaKorgus*))); (* + kirjaaknaKorgus jätta või mitte? *)
			tv = ref(Vaatlemata);
			hind = ref(tipuHind);
		};;

(* funktsioon tippude loomiseks *)
let looTipud(tipuandmeteList) =
	if List.length tipuandmeteList = 0
		then failwith("Graafis peab olema vähemalt üks tipp.");
	List.map looTipp tipuandmeteList;;

(* funktsioon, mis tagastab, kas nimi n ja ja tipu nimi langevad kokku *)
let leiaVastavaNimegaTipp n tipp = (n = tipp.nimi);;

(* funktsioon, mis tagastab, kas serva kaal on sobivates piirides ehk vahemikus (min_int, max_int) *)
let sobivKaal(kaal) =
	match kaal with
		| None -> true
		| Some k -> k < max_int && k > min_int;; 
		(* TODO: tegelt peaks kuvamise kenaduse huvides ka 2-kohaliseks piirama? Või kaugust kaarest suurendama? *)
	
(* funktsioon serva loomiseks *)
let looServ(servaandmed, tipud) =
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

(* funktsioon servade loomiseks *)
let rec looServad(servaandmeteList, tipud) =
	List.map (fun sa -> looServ(sa, tipud)) servaandmeteList;;

(* funktsioon graafi loomiseks *)
let looGraaf(tipud, servad) = {
	tipud = tipud;
	servad = servad;
}

(* funktsioon tipu kuvamiseks *)
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

(* funktsioon, mis tagastab kaare ringjoone x-koordinaadi, y-koordinaadi, raadiuse, algusnurga ja lõppnurga *)
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
			(int_of_float(xr), int_of_float(yr), int_of_float(r), (algusKraad - alfa), (algusKraad + alfa))
		)
	else if minY = maxY		(* tipud asuvad horisontaalselt *)
		then (
			let alfa = int_of_float(radiaanKraadideks(asin ((maxX -. xr) /. r))) in
			let algusKraad = if yr < minY then 90 else 270 in	(* kui ringjoone keskpunkt on tippudest all, siis 90, else 270 *)
			(int_of_float(xr), int_of_float(yr), int_of_float(r), (algusKraad - alfa), (algusKraad + alfa))
		)
	else if a > 0. && a *. xr +. c > yr		(* tipud tõusval sirgel, ringi keskpunkt joonest all *)
		then (
			let alfa = int_of_float(radiaanKraadideks(asin ((xr -. maxX) /. r))) in
			let beeta = int_of_float(radiaanKraadideks(asin ((minY -. yr) /. r))) in
			(int_of_float(xr), int_of_float(yr), int_of_float(r), (90 + alfa), (180 - beeta))
		)
	else if a > 0. && a *. xr +. c < yr		(* tipud tõusval sirgel, ringi keskpunkt joonest üleval *)
		then (
			let alfa = int_of_float(radiaanKraadideks(asin ((yr -. maxY) /. r))) in
			let beeta = int_of_float(radiaanKraadideks(asin ((minX -. xr) /. r))) in
			(int_of_float(xr), int_of_float(yr), int_of_float(r), (270 + beeta), (360 - alfa))
		)
	else if a < 0. && a *. xr +. c < yr		(* tipud langeval sirgel, ringi keskpunkt joonest üleval *)
		then (
			let alfa = int_of_float(radiaanKraadideks(asin ((yr -. maxY) /. r))) in
			let beeta = int_of_float(radiaanKraadideks(asin ((xr -. maxX) /. r))) in
			(int_of_float(xr), int_of_float(yr), int_of_float(r), (180 + alfa), (270 - beeta))
		)
	else if a < 0. && a *. xr +. c > yr		(* tipud langeval sirgel, ringi keskpunkt joonest all *)
		then (
			let alfa = int_of_float(radiaanKraadideks(asin ((minY -. yr) /. r))) in
			let beeta = int_of_float(radiaanKraadideks(asin ((minX -. xr) /. r))) in
			(int_of_float(xr), int_of_float(yr), int_of_float(r), alfa, (90 - beeta))
		)
	else (0, 0, 0, 0, 0);; (* ei tohiks siia jõuda *)

(* funktsioon kaardus serva kuvamiseks *)
let kuvaKaar(tipp1, tipp2) =
	let (xr, yr, r, nurk1, nurk2) = leiaKaareAndmed(tipp1, tipp2) in
	draw_arc xr yr r r nurk1 nurk2;;
		
(* funktsioon serva kuvamiseks *)
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
		
(* funktsioon tipu nime kuvamiseks *)
let kuvaNimi(tipp) =
	moveto (!(tipp.x)-3) (!(tipp.y)-7); (*TODO: panna mahalahutamine raadiusest sõltuma *)
	draw_string tipp.nimi;;
	
(* funktsioon serva kaalu kuvamiseks *)
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
	if x1 = x2 			(* erijuht vertikaalse serva jaoks *)
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
	let nimi = !(serv.tipp1).nimi ^ ":" ^ !(serv.tipp2).nimi in
	let x1 = float_of_int(!(!(serv.tipp2).x)) in
	let y1 = float_of_int(!(!(serv.tipp2).y)) in
	let r1 = float_of_int(tipuRaadius) +. noolePikkus in
	let x2 = float_of_int(Hashtbl.find kaareX nimi) in
	let y2 = float_of_int(Hashtbl.find kaareY nimi) in
	let r2 = float_of_int(Hashtbl.find kaareR nimi) in
	(* ringjoonte võrrandid: (x-x1)^2 + (y-y1)^2 = (r1)^2 ja (x-x2)^2 + (y-y2)^2 = (r2)^2 *)
	(* ringjoonte lõikepunktide leidmine, vt: http://math.stackexchange.com/a/256123 *)
	let v = ((r1 ** 2.) -. (r2 ** 2.)) -. ((x1 ** 2.) -. (x2 ** 2.)) -. ((y1 ** 2.) -. (y2 ** 2.)) in
	(* a - ruutliikme kordaja, b - lineaarliikme kordaja, c - vabaliige *)
	let a = ((x1 -. x2) /. (y2 -. y1)) ** 2. +. 1. in
	let b = v *. (x1 -. x2) /. (y2 -. y1) ** 2. +. 2. *. y1 *. (x2 -. x1) /. (y2 -. y1) -. 2. *. x1 in
	let c = 0.25 *. (v ** 2.) /. (y2 -. y1) ** 2. -. (y1 *. v) /. (y2 -. y1) +. y1 ** 2. +. x1 ** 2. -. r1 ** 2. in  
	let ruutjuur = sqrt ((b ** 2.) -. (4. *. a *. c)) in
	let lahx1 = (ruutjuur -. b) /. (2. *. a) in
	let lahx2 = ((-1.) *. b -. ruutjuur) /. (2. *. a) in
	let lahy1 = ((v /. 2.) +. (lahx1 *. (x1 -. x2))) /. (y2 -. y1) in
	let lahy2 = ((v /. 2.) +. lahx2 *. (x1 -. x2)) /. (y2 -. y1) in
	let tipp1x = float_of_int(!(!(serv.tipp1).x)) in
	let tipp1y = float_of_int(!(!(serv.tipp1).y)) in
	let (x, y) = if leiaJoonePikkus(tipp1x, tipp1y, lahx1, lahy1) < leiaJoonePikkus(tipp1x, tipp1y, lahx2, lahy2) 
		then (lahx1, lahy1) else (lahx2, lahy2) in
	let (a2, c2) = leiaSirge(x, y, x1, y1) in															(* noole telge läbiv sirge *)
	let ((nx1, ny1), (nx2, ny2)) = kaksPunkti(x, y, a2, c2, noolePikkus) in
	let (nx, ny) = if leiaJoonePikkus(nx1, ny1, x1, y1) < leiaJoonePikkus(nx2, ny2, x1, y1) 
		then (nx1, ny1) else (nx2, ny2) in 																	(* nooletipu koordinaadid *)  
	let (a3, c3) = leiaRistuvSirge(a2, c2, x, y) in
	let ((e1, f1), (e2, f2)) = kaksPunkti(x, y, a3, c3, nooleLaius) in		(* ülejäänud 2 punkti koordinaadid *)
	(int_of_float(nx), int_of_float(ny), int_of_float(e1), int_of_float(f1), int_of_float(e2), int_of_float(f2));;	

(* funktsioon, mis tagastab noole koordinaadid (3 punkti koordinaatteljestikul) *)
let leiaNooleKoordinaadid(serv) =
	let nimi = !(serv.tipp1).nimi ^ ":" ^ !(serv.tipp2).nimi in
	if Hashtbl.find kaareR nimi = 0 then leiaNooleKoordinaadidSirge(serv) else leiaNooleKoordinaadidKaar(serv);;
		
(* funktsioon noole kuvamiseks *)
let kuvaNool(serv) =
	let (p1, p2, p3, p4, p5, p6) = leiaNooleKoordinaadid(serv) in
	fill_poly [|p1, p2; p3, p4; p5, p6|];; 
		
(* funktsioon serva ja noole kuvamiseks *)
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

(* funktsioon tipu hinna kuvamiseks *)
let kuvaHind(tipp) =
	match !(tipp.hind) with
		| None -> ()
		| Some h -> (
			moveto (!(tipp.x) - 5) (!(tipp.y) + 20); (* TODO: 5 ja 20 suvalt võetud, panna sõltuma *)
			draw_string (if h = max_int then "inf" else string_of_int(h))
		);;
		
(* funktsioon tippude kuvamiseks *)
let kuvaTipud(tipud) = 
	List.iter kuvaTipp tipud;;
	
(* funktsioon tippude nimede kuvamiseks *)
let kuvaNimed(tipud) =
	set_color black;
	List.iter kuvaNimi tipud;;
	
(* funktsioon servade kaalude kuvamiseks *)
let kuvaKaalud(servad) =
	set_color black;
	List.iter kuvaKaal servad;;
	
(* funktsioon tippude hindade kuvamiseks *)
let kuvaHinnad(tipud) =
	set_color black;
	List.iter kuvaHind tipud;;
	
(* funktsioon servade ja noolte kuvamiseks *)
let kuvaServadJaNooled(servad) =
	set_line_width jooneLaius;
	List.iter kuvaServJaNool(servad);;

(* funktsioon nimekirjade kuvamiseksgraafi kohal *)
let kuvaNimekirjad() =
	moveto 10 (aknaKorgus + korgusLisa - 60);
	draw_string !AlgoBaas.nk1;
	moveto 10 (aknaKorgus + korgusLisa - 80);
	draw_string !AlgoBaas.nk2;
	moveto 10 (aknaKorgus + korgusLisa - 100);
	draw_string !AlgoBaas.nk3;
	moveto 10 (aknaKorgus + korgusLisa - 120);
	draw_string !AlgoBaas.nk4;;
	
(* funktsioon algoritmi sammu kirjelduse väljaprinitmiseks *)
let kuvaTekst() =
	set_color black;
	if !(AlgoBaas.tekst) <> ""
		then print_endline(!(AlgoBaas.tekst) ^ "\n");;

(* funktsioon Floyd-Warshalli tabeli kuvamiseks *)
let kuvaTabel(tipud, servad) =
	set_line_width 2;
	let xAlgus = aknaLaius + 10 in									(* x algkoordinaat - tabeli vasak ülemine nurk *)
	let yAlgus = 400 in															(* y algkoordinaat *)
	let tippudeArv = List.length tipud in
	let a = 30 in 																	(* väikse ruudu külje suurus NB! fill_rect alustab vasakust alumisest nurgast *)
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

(* funktsioon pildi ümber kasti kuvamiseks *)
let kuvaKast() =
	set_color black;
	set_line_width 2;
	moveto 0 0;
	lineto 0 aknaKorgus;
	lineto aknaLaius aknaKorgus;
	lineto aknaLaius 0;
	lineto 0 0;;
	
(* põhiline funktsioon kogu pildi kuvamiseks *)
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
	kuvaKast();
	kuvaTekst();;
