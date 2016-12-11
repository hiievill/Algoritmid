(* moodul MetaPost tegeleb MetaPosti koodi genereerimisega ja tippude, servade ja muu kuvatava MetaPosti koodi lisamisega *)

open Struktuurid;;
open Graafika;;

let nr = ref(1);; (* slaidi number *)

(* funktsioon, mis asendab sõnes kõik täpitähed. Ajutine funktsioon edukaks kuvamiseks *)
let att(tekst) = tekst;;
	(*let s1 = Str.global_replace (Str.regexp "[Õõ]") "6" tekst in
	let s2 = Str.global_replace (Str.regexp "[Ää]") "2" s1 in
	let s3 = Str.global_replace (Str.regexp "[Öö]") "o" s2 in
	let s4 = Str.global_replace (Str.regexp "[Üü]") "y" s3 in
	s4;;*)

(* funktsioon, mis ümardab ujukomaarvu 3 komakohani *)
let ymarda(nr) =
	floor(nr *. 1000.) /. 1000.;;

(* funktsioon, mis jagab oma argumendi 255-ga (vajalik värvi RGB-st lõiku [0,1] teisendamiseks) ja tagastab selle sõnena *)
let v2rviTeisendus(v2rv) =
	string_of_float(ymarda(float_of_int(v2rv) /. 255.));;

(* funktsioon, mis tagastab RGB koodis värvi sõnena, kus numbrid on 255-ga jagatud, et nad oleks lõigus [0,1] *)
let vStr(v1, v2, v3) =
	"(" ^ v2rviTeisendus(v1) ^ ", " ^ v2rviTeisendus(v2) ^ ", " ^ v2rviTeisendus(v3) ^ ")";;

let kirjeldusBase = 2;;
let nimekirjaBase = 2;;

let tipuDef(tipp) =
	tipp.nimi ^ " := (" ^ string_of_int(!(tipp.x)) ^ "u," ^ string_of_int(!(tipp.y)) ^ "u);";;

let tippudeDefid(tipud) =
	String.concat "\n" (List.map tipuDef tipud) ^ "\n";; 

let tipuTekst(tipp) =
		"draw " ^ tipp.nimi ^ " withpen pencircle scaled " ^ string_of_int(2 * Graafika.tipuRaadius) ^ 
			"u withcolor " ^ string_of_vaadeldavus(!(tipp.tv)) ^ "Punkt;";;

let tippudeTekst(tipud) =
	String.concat "\n" (List.map tipuTekst tipud) ^ "\n";;

let kaareTekst(nool, n1, n2, r, x, y, serv) =
	"draw" ^ nool ^ " " ^ "subpath (" ^ string_of_float(n1) ^ ", " ^ string_of_float(n2) ^ 
			") of fullcircle scaled " ^ string_of_int(2 * r) ^ 
			"u shifted (" ^ string_of_int(x) ^ "u," ^ string_of_int(y) ^ "u)" ^
			" cutafter (fullcircle scaled " ^
			string_of_int(tipuRaadius * 2) ^ "u shifted (" ^ 
			string_of_int(!(!(serv.tipp2).x)) ^ "u," ^ string_of_int(!(!(serv.tipp2).y)) ^
			"u))" ^
			" withcolor " ^ string_of_vaadeldavus(!(serv.sv)) ^ "Serv;";;

let servaTekst(serv) =
	let nimi = !(serv.tipp1).nimi ^ ":" ^ !(serv.tipp2).nimi in
	let r = Hashtbl.find AlgoBaas.kaareR nimi in
	let x = Hashtbl.find AlgoBaas.kaareX nimi in
	let y = Hashtbl.find AlgoBaas.kaareY nimi in
	let nool = if serv.nool then "arrow" else "" in
	if r = 0	(* sirge serv *)
		then (
			"draw" ^ nool ^ " " ^ !(serv.tipp1).nimi ^ "--" ^ !(serv.tipp2).nimi ^ 
			" cutafter (fullcircle scaled " ^
			string_of_int(tipuRaadius * 2) ^ "u shifted (" ^ 
			string_of_int(!(!(serv.tipp2).x)) ^ "u," ^ string_of_int(!(!(serv.tipp2).y)) ^
			"u))" ^ 
			" withcolor " ^ string_of_vaadeldavus(!(serv.sv)) ^ "Serv;"
		)
	else (		(* kaardus serv *)
		let (xr, yr, r2, nurk1, nurk2) = leiaKaareAndmed(!(serv.tipp1), !(serv.tipp2)) in
		let n1 = ymarda(float_of_int(nurk1) *. 8. /. 360.) in
		let n2 = ymarda(float_of_int(nurk2) *. 8. /. 360.) in
		kaareTekst(nool, n1, n2, r, x, y, serv) ^ kaareTekst(nool, n2, n1, r, x, y, serv)
		(* TODO: eelnev rida on väga kehv lahendus. Tõsi, üks nool igal juhul joonistatakse ja teine mitte, nii et topelt*)
		(* ei tule, aga peaks kontrollima, kumba joonistada, ja joonistamagi vaid ühe *)
	);;

let servadeTekst(servad) = 
	"pickup pencircle scaled " ^ string_of_int(jooneLaius - 1) ^ "pt;\n" ^
	String.concat "\n" (List.map servaTekst servad) ^ "\n";;

let nooleTekst(serv) =
	let (p1, p2, p3, p4, p5, p6) = leiaNooleKoordinaadid(serv) in
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
			let (x, y) = leiaKaaluKoordinaadid(serv.tipp1, serv.tipp2, 10) in
			"label(\"" ^ string_of_int(k) ^ "\" infont defaultfont, (" ^ string_of_int(x) ^ "u," ^ string_of_int(y) ^ "u)) scaled defaultscale withcolor black;"
		);;

let kaaludeTekst(servad) = 
	if List.length servad = 0
		then ""																																(* kui on servadeta graaf, tagastame tühisõne *)
	else match (List.hd servad).kaal with
		| None -> ""																													(* kui on kaaludeta graaf, tagastame tühisõne *)
		| Some k -> String.concat "\n" (List.map kaaluTekst servad) ^ "\n";;	(* kui on kaaludega, kuvame need *)

let hinnaTekst(tipp) =
	match !(tipp.hind) with
		| None -> ""
		| Some h -> "label(\"" ^ (if h = max_int then "inf" else string_of_int(h)) ^ "\" infont defaultfont, (" ^ string_of_int(!(tipp.x)) ^ "u," ^ string_of_int(!(tipp.y) + tipuRaadius + 10) ^ "u)) scaled defaultscale withcolor black;"

let hindadeTekst(tipud) = 
	match !((List.hd tipud).hind) with
		| None -> ""																													(* kui on hindadeta graaf, tagastame tühisõne *)
		| Some h ->	String.concat "\n" (List.map hinnaTekst tipud) ^ "\n";;		(* kui on hindadega graaf, kuvame need *)

let kirjeldusTekst() =
	let t1 = ref(!(AlgoBaas.tekst)) in
	let t2 = ref("") in
	let t3 = ref("") in
	let rp = 75 in		(* rea pikkus *)
	if String.length !t1 > rp
		then (
			let viimaneTyhik = String.rindex_from !t1 rp ' ' in		(* viimase tühiku indeks, mis asub enne 76. tähemärki *)
			t2 := Str.string_after !t1 (viimaneTyhik + 1);
			t1 := Str.string_before !t1 viimaneTyhik;
			if String.length !t2 > rp
				then (
					let viimaneTyhik2 = String.rindex_from !t2 rp ' ' in		(* viimase tühiku indeks, mis asub enne 76. tähemärki *)
    			t3 := Str.string_after !t2 (viimaneTyhik2 + 1);
					t2 := Str.string_before !t2 viimaneTyhik2;
				)
		);
	"label.rt(\"" ^ att(!t1) ^ "\" infont defaultfont, (10u,-" ^ string_of_int(tekstiLisa - 20*(3+kirjeldusBase)) ^ "u)) scaled 1.5 withcolor black;\n" ^
	"label.rt(\"" ^ att(!t2) ^ "\" infont defaultfont, (10u,-" ^ string_of_int(tekstiLisa - 20*(2+kirjeldusBase)) ^ "u)) scaled 1.5 withcolor black;\n" ^
	"label.rt(\"" ^ att(!t3) ^ "\" infont defaultfont, (10u,-" ^ string_of_int(tekstiLisa - 20*(1+kirjeldusBase)) ^ "u)) scaled 1.5 withcolor black;\n";;
	(*"label.rt(textext (\"" ^ att(!(AlgoBaas.tekst)) ^ "\") infont defaultfont, (10u,10u)) scaled defaultscale withcolor black;\n";;*)
	(*"label.rt(textext (\"" ^ att(!(AlgoBaas.tekst)) ^ "\"), (10u,10u)) scaled defaultscale withcolor black;\n";;*)
	(*"label.rt(btex " ^ att(!(AlgoBaas.tekst)) ^ " etex, (10u,10u)) scaled defaultscale withcolor black;\n";;*)

let nimekirjadeTekst() =
	let ak = aknaKorgus + korgusLisa in
	"label.rt(\"" ^ att(!(AlgoBaas.nk1)) ^ "\" infont defaultfont, (20u," ^ string_of_int(ak - 20*(1+nimekirjaBase)) ^ "u)) scaled defaultscale withcolor black;\n" ^
	"label.rt(\"" ^ att(!(AlgoBaas.nk2)) ^ "\" infont defaultfont, (20u," ^ string_of_int(ak - 20*(2+nimekirjaBase)) ^ "u)) scaled defaultscale withcolor black;\n" ^
	"label.rt(\"" ^ att(!(AlgoBaas.nk3)) ^ "\" infont defaultfont, (20u," ^ string_of_int(ak - 20*(3+nimekirjaBase)) ^ "u)) scaled defaultscale withcolor black;\n" ^
	"label.rt(\"" ^ att(!(AlgoBaas.nk4)) ^ "\" infont defaultfont, (20u," ^ string_of_int(ak - 20*(4+nimekirjaBase)) ^ "u)) scaled defaultscale withcolor black;\n";;

let tabeliTekst(tipud, servad) =
	let tulem = ref("pickup pencircle scaled 1pt;\n") in
	let xAlgus = Graafika.aknaLaius + 10 in					(* x algkoordinaat - tabeli vasak ülemine nurk *)
	let yAlgus = 400 in															(* y algkoordinaat *)
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
    	let x = ref(xAlgus + a + 15) in
    	let y = ref(yAlgus - 2 * a + 3) in
    	for i = 0 to (tippudeArv - 1)
    	do
    		for j = 0 to (tippudeArv - 1)
    		do
    			let kaal = !(FloydWarshall.tabel).(i).(j) in
					tulem := !tulem ^ "label.top(\"" ^ FloydWarshall.string_of_lahter(kaal) ^ "\" infont defaultfont, (" ^ string_of_int(!x) ^ "u," ^ 
							string_of_int(!y) ^ "u)) scaled defaultscale withcolor black;\n";
    			x := !x + a
    		done;
    		y := !y - a;
    		x := xAlgus + a + 15
    	done
		);
	!tulem;;

let kastiTekstid() =
	let al = aknaLaius + if !(AlgoBaas.algo) = FloydWarshall then laiusLisa else 0 in
	let ak = aknaKorgus + korgusLisa in
	let tl = tekstiLisa in
	let lisa = 50 in
	"draw (0u,-" ^ string_of_int(tl) ^ "u)--(" ^ 																	(* kast pildi, teskti  ja nimekirjade ümber *)
				string_of_int(al) ^ "u,-"  ^ string_of_int(tl) ^ "u)--(" ^ 
				string_of_int(al) ^ "u," ^ string_of_int(ak) ^ "u)--(" ^ 
				"0u," ^ string_of_int(ak) ^ "u)--cycle;\n" ^
	"draw (-" ^ string_of_int(lisa) ^ "u,-" ^ string_of_int(tl + lisa) ^ "u)--(" ^ 	(* ääred *)
				string_of_int(al + lisa) ^ "u,-"  ^ string_of_int(tl + lisa) ^ "u)--(" ^ 
				string_of_int(al + lisa) ^ "u," ^ string_of_int(ak + lisa) ^ "u)--(-" ^
				string_of_int(lisa) ^ "u," ^ string_of_int(ak + lisa) ^ "u)--cycle;\n";;
	
let slaidiTekst(tipud, servad) =
	"beginfig(" ^ string_of_int(!nr) ^ ")\n" ^
		kastiTekstid() ^
		tippudeDefid(tipud) ^
		servadeTekst(servad) ^
		tippudeTekst(tipud) ^
		nimedeTekst(tipud) ^
		kaaludeTekst(servad) ^
		hindadeTekst(tipud) ^
		kirjeldusTekst() ^
		nimekirjadeTekst() ^
		(if !(AlgoBaas.algo) = FloydWarshall then tabeliTekst(tipud, servad) else "") ^
	"endfig;\n\n";;

let failiAlgus(tipud) =
	"input latexmp;\n" ^
	"setupLaTeXMP (packages=\"fontenc[T1], inputenc[ansinew,utf8], babel[estonian], times, huge\");\n" ^
	"u := 0.2mm;\n" ^
	"defaultscale := 1.0;\n" ^
	"defaultfont := \"ptmr8r\";\n" ^ (* ptmr8r? cmr10? *) 
	"prologues := 3;\n" ^
	"filenametemplate \"temp%c.mps\";\n" ^
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
	"interim ahlength := " ^ string_of_float(noolePikkus) ^ "u;\n" ^
	"interim ahangle := " ^ string_of_float(ymarda(2. *. radiaanKraadideks(atan (nooleLaius /. noolePikkus)))) ^ ";\n" ^
	"pair " ^ (String.concat ", " (List.map (fun t -> t.nimi) tipud)) ^ ";\n\n";;

let failiLopp() =
	"end;";;