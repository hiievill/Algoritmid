open Graphics;;

let aknaLaius = 600;;
let aknaKorgus = 650;;
let kirjaaknaKorgus = 150;;

let tipuRaadius = 20;;

let nooleLaius = 7.;; (* õigupoolest pool laiust *)
let noolePikkus = 20.;;

let tekst = ref("");;	(* algoritmi sammudel kuvatav tekst *)

type vaadeldavus = Vaatlemata	(* tähistab tipu/serva vaadeldavust, oluline kuvamisel värvi valikuks *)
	| Vaadeldav
	| Valitud
	| Vaadeldud;;

type tipp = {
	nimi : string;
	x : int ref;
	y : int ref;
	tv : vaadeldavus ref;
}

let string_of_tipp(tipp) =
	"Tipp {" ^ tipp.nimi ^ ", " ^ string_of_int(!(tipp.x)) ^ ", " ^ string_of_int(!(tipp.y)) ^ "}";;

type noolesuund = Puudub
	| EsimesestTeise
	| TeisestEsimesse
	| Kahesuunaline;;

type serv = {
	tipp1 : tipp ref;
	tipp2 : tipp ref;
	kaal : int option;
	nool : noolesuund; (* NB! Ei arvesta praegu kahesuunalisusega - neil võivad ju eri kaalud olla, TODO. *)
	sv : vaadeldavus ref;
}

(*ei kasuta veel *)
type graaf = {
	tipud : tipp list;
	servad : serv list;
}
	
let string_of_vaadeldavus(v) =
	match v with
		| Vaatlemata -> "Vaatlemata"
		| Vaadeldav -> "Vaadeldav"
		| Valitud -> "Valitud"
		| Vaadeldud -> "Vaadeldud";;
	
	


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

(*TODO: kirjaaknaKorgust ka arvestada. NB! aknaLaius - tipuRaadius pole päris täpne, vaja kuidagi katsetamisega leida *)
let piiridesX(x) = if x < tipuRaadius then tipuRaadius else if x > aknaLaius - tipuRaadius then aknaLaius - tipuRaadius else x;;
let piiridesY(y) = 
	if y < tipuRaadius + kirjaaknaKorgus then tipuRaadius + kirjaaknaKorgus 
	else if y > aknaKorgus - tipuRaadius then aknaKorgus - tipuRaadius 
	else y;;


let looTipp(tipuandmed) = 
	match tipuandmed with
		| (tipuNimi, tipuX, tipuY) -> {
			nimi = tipuNimi;
			x = ref(piiridesX(tipuX));
			y = ref(piiridesY(tipuY + kirjaaknaKorgus)); (* + kirjaaknaKorgus jätta või mitte? *)
			tv = ref(Vaatlemata);
		};;

let looTipud(tipuandmeteList) =
	List.map looTipp tipuandmeteList;;
	
let leiaVastavaNimegaTipp n tipp = (n = tipp.nimi);;
	
let looServ(servaandmed, tipud) =
	match servaandmed with
		| (t1nimi, t2nimi, k, noolTipust) -> ( 
			try
				let t1 = List.find (leiaVastavaNimegaTipp t1nimi) tipud in
				let t2 = List.find (leiaVastavaNimegaTipp t2nimi) tipud in
				Some {
					tipp1 = ref(t1);
					tipp2 = ref(t2);
					kaal = k;
					nool = noolTipust;
					sv = ref(Vaatlemata);
				}
			with
				| Not_found -> (
					print_endline("Sellise nimega tippu pole."); (*TODO: kummagi tipu jaoks eraldi?*)
					None
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


(*TODO: eraldi fn randomilt tippude loomiseks, ainult etteantud arvuga? *)


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
	

let kuvaTipp(tipp) = 
	set_color (
		match !(tipp.tv) with
			| Vaatlemata -> rgb 252 22 22
			| Vaadeldav -> rgb 253 253 122
			| Valitud -> rgb 253 159 37
			| Vaadeldud -> rgb 22 137 9
	);
	fill_circle !(tipp.x) !(tipp.y) tipuRaadius;;
	
let algoL2bi = ref(false);; (* või selle asemel algo lõpus ebavajalikud servad ära kustutada? *)
		
let kuvaServ(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; sv = v} -> (
			if (!algoL2bi && !v = Vaadeldud || !algoL2bi = false)
				then (
					set_color (
						match !v with
							| Vaatlemata -> rgb 150 4 4
							| Vaadeldav -> rgb 240 240 3
							| Valitud -> rgb 253 144 1
							| Vaadeldud -> rgb 78 247 59
					);
					moveto !(!t1.x) !(!t1.y);
					lineto !(!t2.x) !(!t2.y)
				)
			else ()
		);;
		
let kuvaNimi(tipp) =
	moveto (!(tipp.x)-3) (!(tipp.y)-7); (*TODO: panna mahalahutamine raadiusest sõltuma *)
	draw_string tipp.nimi;;
	
let kuvaKaal(serv) =
	match serv with
		| {tipp1 = a; tipp2 = b; kaal = k; sv = v} -> (
			if !algoL2bi && !v = Vaadeldud || !algoL2bi = false
				then (
					match k with
						| Some ka -> (
							let koordinaadid = kaaluKoordinaadid(a, b, 20) in (*TODO: see kaugus 20 on omavoliline *)
							match koordinaadid with
								| (i, j) -> moveto i j;
							draw_string (string_of_int(ka))
						)
						| None -> () (* kas see haru on üldse vajalik? OCamlis pole exhaustive probleeme? *)
				)
		);;
		
(*funktsioon, mis leiab sirgega y=ax+c punktis (e, f) ristuva sirge tõusu ja vabaliikme *)
let leiaRistuvSirge(a, c, e, f) = 
	let a2 = (-1.) /. a in
	let c2 = f +. e /. a in
	(a2, c2);;
	
(* Funktsioon, mis tagastab kas p asub t1x ja t2x vahel*)
let kaheTipuVahel(p, t1x, t2x) =
	p > min t1x t2x && p < max t1x t2x;;
		
let kuvaNool(fromTipp, toTipp) =
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
		
let kuvaEriNool(serv) =
	match serv with
		| {tipp1 = a; tipp2 = b; nool = n} -> 
			match n with
				| Puudub -> ()
				| EsimesestTeise -> kuvaNool(a, b)
				| TeisestEsimesse -> kuvaNool(b, a)
				| Kahesuunaline -> (
					kuvaNool(a, b);
					kuvaNool(b, a)
				);;
		
let kuvaTipud(tipud) = 
	List.iter kuvaTipp tipud;;
	
let kuvaServad(servad) =
	set_line_width 3;
	List.iter kuvaServ servad;;
	
let kuvaNimed(tipud) =
	set_color black;
	set_text_size 5;
	List.iter kuvaNimi tipud;;
	
let kuvaKaalud(servad) =
	set_color black;
	List.iter kuvaKaal servad;;
	
let kuvaNooled(servad) = 
	set_color black; (* TODO: värv analoogiliselt servadega *)
	List.iter kuvaEriNool servad;;
	
let kuvaVahe() = (*kirjaaken ja graafiakent lahutav joon *)
	set_color black;
	set_line_width 1;
	moveto 0 kirjaaknaKorgus;
	lineto aknaLaius kirjaaknaKorgus;;
	
let kuvaTekst() =
	set_color black;
	moveto 0 100;
	draw_string !tekst;;
	
let kuvaPilt(tipud, servad) =
	clear_graph();
	kuvaServad(servad);
	kuvaNooled(servad);
	kuvaTipud(tipud);
	kuvaNimed(tipud);
	kuvaKaalud(servad);
	kuvaVahe();
	kuvaTekst();;

let liigutatavTipp = ref(None);;

let hiirTipul hx hy tipp =
	match tipp with
		| {nimi = n; x = tx; y = ty} -> sqrt ((float_of_int(hx - !tx)) ** 2. +. (float_of_int(hy - !ty)) ** 2.) <= float_of_int(tipuRaadius);;
	



type primiSamm = Algus
	| EsimeneTipp
	| ServaVaatlus		(*vaatleme võimalikke servi, mida lisada*)
	| ServaValik		(*valime neist lühima *)
	| ServaLisamine		(*lisame selle ja talle vastava tipu puusse *)
	| Lopp				(*kuvame tekkinud minimaalse toesepuu*)
	| L2bi;;

let i = ref(Algus);; (* counter algoritmi sammude jaoks*)

let algus() =
	tekst := "Algoritm alustab.";
	i := EsimeneTipp;;
	
(* kui serva üks tipp on vaadeldud ja teine mitte, märgime serva ja vaatlemata tipu vaadeldavateks*)
let vaatle(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> 
			if !((!t1).tv) = Vaadeldud && !((!t2).tv) <> Vaadeldud
				then (
					(!t2).tv := Vaadeldav;
					serv.sv := Vaadeldav;
				)
			else if !((!t2).tv) = Vaadeldud && !((!t1).tv) <> Vaadeldud
				then (
					(!t1).tv := Vaadeldav;
					serv.sv := Vaadeldav
				);;
				
let tippVaadeldud(tipp) =	(*tagastab true, kui vastav tipp on vaadeldud*)
	!(tipp.tv) = Vaadeldud;;
	
let rec leiaLyhimServ(servad) =
	match servad with
		| x::xs -> (
			let lyhim = leiaLyhimServ(xs) in
			match lyhim with
				| Some lyhimServ -> if x.kaal < lyhimServ.kaal then Some x else lyhim
				| None -> Some x
		)
		| [] -> None;;

(* tagastab true, kui serv on vaadeldav *)
let servVaadeldav(serv) = !(serv.sv) = Vaadeldav;;
		
let valiServ(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> (
			if !((!t1).tv) = Vaadeldav then (!t1).tv := Valitud;
			if !((!t2).tv) = Vaadeldav then (!t2).tv := Valitud;
			serv.sv := Valitud;
		);;
		
let rec lisaServ(servad) =
	match servad with
		| x::xs -> (
			match x with
				| {tipp1 = t1; tipp2 = t2; sv = v} -> (
					if !v = Valitud 
						then (
							v := Vaadeldud;
							(!t1).tv := Vaadeldud;
							(!t2).tv := Vaadeldud
						)
					else lisaServ(xs)
				);
		)
		| [] -> print_endline("Ühtegi serva ei lisatud. Ei tohiks juhtuda.");;
	
	
let lopetaTipuVaatlus(tipp) =
	if !(tipp.tv) = Vaadeldav then tipp.tv := Vaatlemata;;
		
let lopetaServaVaatlus(serv) =
	if !(serv.sv) = Vaadeldav then serv.sv := Vaatlemata;;
		
let esimeneTipp(algtipp) =
	tekst := "Märgime algtipu külastatuks. Sellest hakkame toesepuud ehitama.";
	algtipp.tv := Vaadeldud;
	i := ServaVaatlus;;

let servaVaatlus(servad) = 
	List.iter vaatle servad;
	tekst := "Vaatleme kõiki servi, mis ühendavad külastatud tippe külastamata tippudega."; (* TODO: konkreetsed servad? *)
	i := ServaValik;;
				
let servaValik(servad) = 
	let vaadeldavadServad = List.filter servVaadeldav servad in
	let lyhim = leiaLyhimServ(vaadeldavadServad) in
	match lyhim with
		| None -> print_endline("Midagi on viga.");
		| Some lyhimServ ->	(
			valiServ(lyhimServ);
			tekst := "Valime lühima serva.";
		);
	i := ServaLisamine;;

let servaLisamine(tipud, servad) =
	lisaServ(servad);
	List.iter lopetaTipuVaatlus tipud;
	List.iter lopetaServaVaatlus servad;
	tekst := "Loeme serva ja vastava tipu külastatuks ning ühendame tekkinud puuga.";
	if List.for_all tippVaadeldud tipud
		then i := Lopp
	else i := ServaVaatlus;;

let lopp() = 
	tekst := "Algoritm lõpetab, olles leidnud minimaalse toesepuu.";
	algoL2bi := true;
	i := L2bi;;
	
(*TODO: peaks kontrollima ka seda, et graaf ikka sidus oleks? Ja et kõikidel servadel (mitteneg) kaalud oleks. *)
(*tipp - tipp, millest läbimängu alustame; tipud - kõik graafis esinevad tipud; servad - kõik graafis esinevad servad *)
let samm(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus()
		| EsimeneTipp -> esimeneTipp(algtipp)
		| ServaVaatlus -> servaVaatlus(servad)
		| ServaValik -> servaValik(servad)
		| ServaLisamine -> servaLisamine(tipud, servad)
		| Lopp -> lopp()
		| L2bi -> ();;
		


let syndmused(algtipp, tipud, servad) =
	let hiirVajutatud = ref(false) in
	while true do
		let e = Graphics.wait_next_event [Graphics.Button_down; Graphics.Button_up; Graphics.Mouse_motion; Graphics.Key_pressed] in
		if e.Graphics.keypressed 
			then 
				match e.key with
					| 'q' -> Graphics.close_graph()
					| 'n' -> (
						samm(algtipp, tipud, servad); (*Prim.samm*)
						kuvaPilt(tipud, servad)
					)
					| _ -> ()
		else
			if e.Graphics.button
				then 
					if !hiirVajutatud
						then (
							if !liigutatavTipp <> None
								then 		(* liigutame valitud tippu *)
									match !liigutatavTipp with
										| Some t -> (
											t.x := piiridesX(e.Graphics.mouse_x);
											t.y := piiridesY(e.Graphics.mouse_y);
											kuvaPilt(tipud, servad)
										)
										| _ -> print_endline("Midagi viga.")
						)
					else (	(* kui hiire alla vajutame ja parajasti mõne tipu peal oleme, siis valime selle *)
						hiirVajutatud := true;
						try
							let valitudTipp = List.find (hiirTipul e.Graphics.mouse_x e.Graphics.mouse_y) tipud in
							liigutatavTipp := Some valitudTipp
						with
							| Not_found -> ()
					)
			else (* hiirt vabastades vabastame ka valitud tipu *)
				if !hiirVajutatud
					then (
						hiirVajutatud := false;
						liigutatavTipp := None
					);
	done;;
	
try
	(* Näide 1 - lihtsalt graaf, mis kogu graafika võimalusi kujutab *)
	(*let tipud = looTipud([
		("A", 200, 200);
		("B", 250, 40);
		("C", 30, 300);
		("D", 540, 410)
	]) in
	let servad = looServad([
		("A", "B", Some(20), EsimesestTeise);
		("B", "C", Some(5), Puudub);
		("C", "D", None, Kahesuunaline);
		("A", "C", Some(17), TeisestEsimesse);
	], tipud) in*)



	(* Näide 2 - Prim *)
	(*let tipud = looTipud([
		("A", 100, 300);
		("B", 300, 300);
		("C", 100, 100);
		("D", 300, 100);
		("E", 500, 200);
	]) in
	let servad = looServad([
		("A", "B", Some(1), Puudub);
		("B", "D", Some(2), Puudub);
		("B", "E", Some(3), Puudub);
		("C", "D", Some(4), Puudub);
		("A", "C", Some(5), Puudub);
		("D", "E", Some(6), Puudub);
	], tipud) in*)
	
	(* Näide 3 - Prim *)
	let tipud = looTipud([
		("A", 200, 200);
		("B", 250, 40);
		("C", 30, 300);
		("D", 540, 410);
		("E", 10, 400);
		("F", 400, 70)
	]) in
	let servad = looServad([
		("A", "B", Some(6), Puudub);
		("B", "C", Some(5), Puudub);
		("C", "D", Some(8), Puudub);
		("A", "C", Some(3), Puudub);
		("A", "F", Some(8), Puudub);
		("D", "F", Some(14), Puudub);
		("E", "D", Some(9), Puudub);
	], tipud) in
	let algtipp = List.hd tipud in (* ajutine - peab saama ise valida *)
	open_graph (" " ^ string_of_int(aknaLaius) ^ "x" ^ string_of_int(aknaKorgus));
	set_window_title "Graafialgoritmid";
	kuvaPilt(tipud, servad);
	syndmused(algtipp, tipud, servad);
with (* TODO: et Windowsis ei hanguks, Linuxis ka paremini handle'ida.*)
	| Graphic_failure("fatal I/O error") -> ();
	| Graphic_failure("graphic screen not opened") -> ();;