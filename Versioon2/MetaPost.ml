open Struktuurid;;

let nr = ref(1);; (* slaidi number *)

(* TODO: servade ja noolte eraldi joonistamise asemel drawarrow *)

(* funktsioon, mis asendab sõnes kõik täpitähed. Ajutine funktsioon edukaks kuvamiseks *)
let att(tekst) =
	let s1 = Str.global_replace (Str.regexp "[Õõ]") "6" tekst in
	let s2 = Str.global_replace (Str.regexp "[Ää]") "2" s1 in
	let s3 = Str.global_replace (Str.regexp "[Öö]") "o" s2 in
	let s4 = Str.global_replace (Str.regexp "[Üü]") "y" s3 in
	s4;;

(* funktsioon, mis jagab oma argumendi 255-ga (vajalik värvi RGB-st lõiku [0,1] teisendamiseks) ja tagastab selle sõnena *)
let v2rviTeisendus(v2rv) =
	string_of_float(float_of_int(v2rv) /. 255.);;

(* funktsioon, mis tagastab RGB koodis värvi sõnena, kus numbrid on 255-ga jagatud, et nad oleks lõigus [0,1] *)
let vStr(v1, v2, v3) =
	"(" ^ v2rviTeisendus(v1) ^ ", " ^ v2rviTeisendus(v2) ^ ", " ^ v2rviTeisendus(v3) ^ ")";;

let tipuDef(tipp) =
	tipp.nimi ^ " := (" ^ string_of_int(!(tipp.x)) ^ "u," ^ string_of_int(!(tipp.y)) ^ "u);";;

let tippudeDefid(tipud) =
	String.concat "\n" (List.map tipuDef tipud) ^ "\n";; 

let tipuTekst(tipp) =
		"draw " ^ tipp.nimi ^ " withpen pencircle scaled " ^ string_of_int(2 * Graafika.tipuRaadius) ^ 
			"u withcolor " ^ string_of_vaadeldavus(!(tipp.tv)) ^ "Punkt;";;

let tippudeTekst(tipud) =
	String.concat "\n" (List.map tipuTekst tipud) ^ "\n";; 

let servaTekst(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> "draw " ^ !t1.nimi ^ "--" ^ !t2.nimi 
				^ " withcolor " ^ string_of_vaadeldavus(!(serv.sv)) ^ "Serv;";;

let servadeTekst(servad) = 
	"pickup pencircle scaled " ^ string_of_int(Graafika.jooneLaius) ^ "pt;\n" ^
	String.concat "\n" (List.map servaTekst servad) ^ "\n";;

let nooleTekst(serv) =
	let (p1, p2, p3, p4, p5, p6) = Graafika.leiaNooleKoordinaadid(serv) in
	"fill (" ^ string_of_int(p1) ^ "u," ^ string_of_int(p2) ^ "u)--(" ^
		string_of_int(p3) ^ "u," ^ string_of_int(p4) ^ "u)--(" ^
		string_of_int(p5) ^ "u," ^ string_of_int(p6) ^ "u)--cycle withcolor " ^ string_of_vaadeldavus(!(serv.sv)) ^ "Serv;";;

let noolteTekst(servad) =
	String.concat "\n" (List.map nooleTekst servad) ^ "\n";;

let nimeTekst(tipp) =
	"label(\"" ^ tipp.nimi ^ "\" infont defaultfont, " ^ tipp.nimi ^ ") scaled defaultscale withcolor black;";;

let nimedeTekst(tipud) = 
	String.concat "\n" (List.map nimeTekst tipud) ^ "\n";;

let kaaluTekst(serv) =
	match serv.kaal with
		| None -> ""
		| Some k -> (
			let (x, y) = Graafika.leiaKaaluKoordinaadid(serv.tipp1, serv.tipp2, 10) in
			"label(\"" ^ string_of_int(k) ^ "\" infont defaultfont, (" ^ string_of_int(x) ^ "u," ^ string_of_int(y) ^ "u)) scaled defaultscale withcolor black;"
		);;

let kaaludeTekst(servad) = 
	if List.length servad = 0
		then ""																																(* kui on servadeta graaf, tagastame tühisõne *)
	else match (List.hd servad).kaal with
		| None -> ""																													(* kui on kaaludeta graaf, tagastame tühisõne *)
		| Some k -> String.concat "\n" (List.map kaaluTekst servad) ^ "\n";;	(* kui on kaaludega, kuvame need *)

let hinnaTekst(tipp) =
	match tipp.hind with
		| None -> ""
		| Some h -> "label(\"" ^ string_of_int(h) ^ "\" infont defaultfont, (" ^ string_of_int(!(tipp.x)) ^ "u," ^ string_of_int(!(tipp.y) + 20) ^ "u)) scaled defaultscale withcolor black;"

let hindadeTekst(tipud) = 
	if List.length tipud = 0	(* TODO: see kontroll varem. Ei tohiks kunagi juhtuda. *)
		then ""																																(* kui on tippudeta graaf, tagastame tühisõne *)
	else match (List.hd tipud).hind with
		| None -> ""																													(* kui on hindadeta graaf, tagastame tühisõne *)
		| Some h ->	String.concat "\n" (List.map hinnaTekst tipud) ^ "\n";;		(* kui on hindadega graaf, kuvame need *)

let kirjeldusTekst() =
	"label.rt(\"" ^ att(!(AlgoBaas.tekst)) ^ "\" infont defaultfont, (20u,10u)) scaled defaultscale withcolor black;\n";;

let nimekirjadeTekst() =
	let ak = Graafika.aknaKorgus in
	"label.rt(\"" ^ att(!(AlgoBaas.nk1)) ^ "\" infont defaultfont, (20u," ^ string_of_int(ak - 20) ^ "u)) scaled defaultscale withcolor black;\n" ^
	"label.rt(\"" ^ att(!(AlgoBaas.nk2)) ^ "\" infont defaultfont, (20u," ^ string_of_int(ak - 40) ^ "u)) scaled defaultscale withcolor black;\n" ^
	"label.rt(\"" ^ att(!(AlgoBaas.nk3)) ^ "\" infont defaultfont, (20u," ^ string_of_int(ak - 60) ^ "u)) scaled defaultscale withcolor black;\n";;

let tabeliTekst(tipud, servad) = 		(* TODO: koodikordus Graafika.kuvatabeliga, osa kokku võtta? *)
	let tulem = ref("") in
	let xAlgus = Graafika.aknaLaius + 10 in	(* x algkoordinaat - tabeli vasak ülemine nurk *)
	let yAlgus = 400 in							(* y algkoordinaat *)
	let tippudeArv = List.length tipud in
	let a = 30 in
	if !(FloydWarshall.fiks) >= 0 && !(FloydWarshall.fiks) < tippudeArv 
		then (																				(* fikseeritud rea ja veeru kuvamine *)
			let kastiLaius = a in												(* horisontaalne kast *)
			let kastiPikkus = (tippudeArv + 1) * a in
			let x0 = xAlgus in
			let y0 = yAlgus - (!(FloydWarshall.fiks) + 2) * a in
			tulem := !tulem ^ "fill (" ^ string_of_int(x0) ^ "u," ^ string_of_int(y0) ^ "u)--(" ^ 
				string_of_int(x0 + kastiPikkus) ^ "u," ^ string_of_int(y0) ^ "u)--(" ^
				string_of_int(x0 + kastiPikkus) ^ "u," ^ string_of_int(y0 + kastiLaius) ^ "u)--(" ^
				string_of_int(x0) ^ "u," ^ string_of_int(y0 + kastiLaius) ^ "u)--cycle withcolor VaadeldavPunkt;\n";
				
			let x0 = xAlgus + (!(FloydWarshall.fiks) + 1) * a in
			let y0 = yAlgus - (tippudeArv + 1) * a in
			tulem := !tulem ^ "fill (" ^ string_of_int(x0) ^ "u," ^ string_of_int(y0) ^ "u)--(" ^ 
				string_of_int(x0 + kastiLaius) ^ "u," ^ string_of_int(y0) ^ "u)--(" ^
				string_of_int(x0 + kastiLaius) ^ "u," ^ string_of_int(y0 + kastiPikkus) ^ "u)--(" ^
				string_of_int(x0) ^ "u," ^ string_of_int(y0 + kastiPikkus) ^ "u)--cycle withcolor VaadeldavPunkt;\n";
		);
	if !(FloydWarshall.x) >= 0 && !(FloydWarshall.y) >= 0 && !(FloydWarshall.x) <= tippudeArv - 1 
		then (																				(* vaadeldava lahtri kuvamine *)
			let x0 = xAlgus + (!(FloydWarshall.y) + 1) * a in
			let y0 = yAlgus - (!(FloydWarshall.x) + 2) * a in
			tulem := !tulem ^ "fill (" ^ string_of_int(x0) ^ "u," ^ string_of_int(y0) ^ "u)--(" ^ 
				string_of_int(x0 + a) ^ "u," ^ string_of_int(y0) ^ "u)--(" ^
				string_of_int(x0 + a) ^ "u," ^ string_of_int(y0 + a) ^ "u)--(" ^
				string_of_int(x0) ^ "u," ^ string_of_int(y0 + a) ^ "u)--cycle withcolor ValitudPunkt;\n";
		);	
	(* horisontaalsed jooned *)
	let x = ref(xAlgus) in
	let y = ref(yAlgus) in
	for i = 0 to (tippudeArv + 1)
	do
		tulem := !tulem ^ "draw (" ^ string_of_int(!x) ^ "u," ^ string_of_int(!y) ^ "u)--(" ^ 
				string_of_int(!x + (tippudeArv + 1) * a) ^ "u," ^ string_of_int(!y) ^ "u);\n";
		y := !y - a
	done;
	(* vertikaalsed jooned *)
	let x = ref(xAlgus) in
	let y = ref(yAlgus) in
	for i = 0 to (tippudeArv + 1)
	do
		tulem := !tulem ^ "draw (" ^ string_of_int(!x) ^ "u," ^ string_of_int(!y) ^ "u)--(" ^ 
				string_of_int(!x) ^ "u," ^ string_of_int(!y - (tippudeArv + 1) * a) ^ "u);\n";
		x := !x + a
	done;
	(* horisontaalsed tähed *)
	let x = ref(xAlgus + a + 5) in
	let y = ref(yAlgus - a + 5) in
	for i = 0 to (tippudeArv - 1)
	do
		tulem := !tulem ^ "label.urt(\"" ^ (List.nth tipud i).nimi ^ "\" infont defaultfont, (" ^ string_of_int(!x) ^ "u," ^ 
				string_of_int(!y) ^ "u)) scaled defaultscale withcolor black;\n";
		x := !x + a
	done;
	(* vertikaalsed tähed *)
	let x = ref(xAlgus + 5) in
	let y = ref(yAlgus - 2 * a + 5) in
	for i = 0 to (tippudeArv - 1)
	do
		tulem := !tulem ^ "label.urt(\"" ^ (List.nth tipud i).nimi ^ "\" infont defaultfont, (" ^ string_of_int(!x) ^ "u," ^ 
				string_of_int(!y) ^ "u)) scaled defaultscale withcolor black;\n";
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
    			let kaal = !(FloydWarshall.tabel).(i).(j) in
					tulem := !tulem ^ "label.urt(\"" ^ FloydWarshall.string_of_lahter(kaal) ^ "\" infont defaultfont, (" ^ string_of_int(!x) ^ "u," ^ 
							string_of_int(!y) ^ "u)) scaled defaultscale withcolor black;\n";
    			x := !x + a
    		done;
    		y := !y - a;
    		x := xAlgus + a + 5
    	done
		);
	!tulem;;
	
	
let slaidiTekst(tipud, servad) =
	let alStr = string_of_int(Graafika.aknaLaius + if !(AlgoBaas.algo) = FloydWarshall then Graafika.fwLaius else 0) in
	let akStr = string_of_int(Graafika.aknaKorgus) in
	"beginfig(" ^ string_of_int(!nr) ^ ")\n" ^
		"draw (0,0)--(" ^ alStr ^ "u,0)--(" ^ alStr ^ "u," ^ akStr ^ "u)--(0," ^ akStr ^ "u)--cycle;\n" ^ (* slaidi ümbritsev kast *)
		tippudeDefid(tipud) ^
		servadeTekst(servad) ^
		noolteTekst(servad) ^
		tippudeTekst(tipud) ^
		nimedeTekst(tipud) ^
		kaaludeTekst(servad) ^
		hindadeTekst(tipud) ^
		kirjeldusTekst() ^
		nimekirjadeTekst() ^				(* TODO: kodeering/font korda ja need tagasi sisse *)
		(if !(AlgoBaas.algo) = FloydWarshall then tabeliTekst(tipud, servad) else "\n") ^
	"endfig;\n\n";;

let failiAlgus(tipud) = (* TODO: siia noolte, kaalude jms arvutamine, et mitu korda ei peaks *)
	"u := 0.5mm;\n" ^
	"defaultscale := 1.0;\n" ^
	"defaultfont := \"cmr10\";\n" ^ 
	"prologues := 3;\n" ^
	"color VaatlemataPunkt, VaadeldavPunkt, ValitudPunkt, VaadeldudPunkt, SobimatuPunkt;\n" ^
	"color VaatlemataServ, VaadeldavServ, ValitudServ, VaadeldudServ, SobimatuServ;\n" ^ 
	
	"VaatlemataPunkt := " ^ vStr(255, 22, 22) ^ ";\n" ^
	"VaadeldavPunkt := " ^ vStr(253, 253, 122) ^ ";\n" ^
	"ValitudPunkt := " ^ vStr(253, 159, 37) ^ ";\n" ^
	"VaadeldudPunkt := " ^ vStr(22, 137, 9) ^ ";\n" ^
	"SobimatuPunkt := " ^ vStr(148, 148, 148) ^ ";\n" ^
	
	"VaatlemataServ := " ^ vStr(150, 4, 4) ^ ";\n" ^
	"VaadeldavServ := " ^ vStr(240, 240, 3) ^ ";\n" ^
	"ValitudServ := " ^ vStr(253, 144, 1) ^ ";\n" ^
	"VaadeldudServ := " ^ vStr(78, 247, 59) ^ ";\n" ^
	"SobimatuServ := " ^ vStr(67, 67, 67) ^ ";\n" ^
	"pair " ^ (String.concat ", " (List.map (fun t -> t.nimi) tipud)) ^ ";\n\n";;

let failiLopp() =
	"end;";;