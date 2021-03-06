(* moodul FloydWarshall teostab sammsammulist Floyd-Warshalli algoritmi l�bim�ngu *) 

open Struktuurid;;
open AlgoBaas;;

let tabel = ref([||]);;	(* graafi servade k�lgnevusmaatriks *)

let x = ref(-1);;				(* jooksev x *)
let y = ref(-1);;				(* jooksev y *)
let fiks = ref(-1);;		(* fikseeritud tulba ja veeru number *)

(* funktsioon servadevahelise kauguse s�nena kuvamiseks. Kui teed ei leidu, kuvatakse "inf", vastasel korral tee pikkus *)
let string_of_lahter(nr) =
	if nr = max_int then "inf" else string_of_int(nr);;

(* funktsioon, mis tagastab tippude t1 ja t2 vahelise serva kaalu. Kui tipud on samad, tagastatakse 0, kui nende vahel serva *)
(* pole, siis tagastatakse max_int *)
let leiaServaKaal(t1, t2, servad) =
	if t1 = t2 then 0 else
	try
		let serv = List.find (fun s -> !(s.tipp1) = t1 && !(s.tipp2) = t2) servad in
		match serv.kaal with
			| None -> failwith("Serval pole kaalu. Ei tohiks juhtuda.")
			| Some k -> k
	with
		| Not_found -> max_int;;

(* funktsioon, mis uuendab x ja y loendureid *)
let uuendaLoendureid() =
	if !y = (Array.length !tabel) - 1	(* kui y loendur j�udis rea l�ppu, paneme nulliks ja suurendame x loendurit *)
		then (
			y := 0;
			x := !x + 1
		)
	else y := !y + 1;;								(* vastasel korral lihtsalt suurendame y loendurit *)

(* algoritmi algus, mille k�igus luuakse servade k�lgnevusmaatriks *)
let algus(tipud, servad) =
	tekst := "Floyd-Warshalli algoritm alustab. Tekitame graafi servade k�lgnevusmaatriksi.";
	let tippudeArv = List.length tipud in
	let maatriks = Array.make tippudeArv [||] in
	(* loome maatriksi *)
	for i = 0 to (tippudeArv - 1)
	do
		let rida = Array.make tippudeArv max_int in	(* algv��rtuseks max_int *)
		maatriks.(i) <- rida
	done;
	(* t�idame tippudevaheliste kaarte kaaludega *)
	for i = 0 to (tippudeArv - 1)
	do
		for j = 0 to (tippudeArv - 1)
		do
			let kaal =  leiaServaKaal(List.nth tipud i, List.nth tipud j, servad) in
			maatriks.(i).(j) <- kaal
		done
	done;
	tabel := maatriks;
	i := Fikseerimine;;

(* veeru ja tulba fikseerimine *)
let fikseerimine(tipud) =
	x := 0;
	y := -1;
	fiks := !fiks + 1;
	(List.nth tipud !fiks).tv := Vaadeldav;
	tekst := "Fikseerime " ^ string_of_lahter(!fiks + 1) ^ ". veeru ja tulba ja hakkame �lej��nud lahtrites kaugusi uuendama.";
	tekst := !tekst ^ " " ^ "Kui lahtris olev v��rtus on suurem kui vastava valitud veeru ja tulba v��rtuste summa, siis uuendame seda.";
	i := LahtriVaatlus;;

(* lahtri vaatlemine ja otsustamine, kas tema v��rtust muuta v�i mitte *)
let lahtriVaatlus(tipud) =
	uuendaLoendureid();
	while !x = !fiks || !y = !fiks || !x = !y
	do
		uuendaLoendureid()
	done;
	if !x >= (Array.length !tabel)			(* kui tabel sai l�bi, l�hme kas uut rida&veergu fikseerima v�i l�pule *)
		then (
			tekst := "K�ik lahtrid said vaadatud.";
			List.iter (fun t -> if !(t.tv) = Vaadeldav then t.tv := Vaadeldud) tipud;
			if !fiks < Array.length !tabel - 1
				then (
					tekst := !tekst ^ " " ^ "L�hme j�rgmist rida ja veergu fikseerima.";
					i := Fikseerimine
				)
			else (
				i := Lopp
			)
	)
	else (
		let senine = !tabel.(!x).(!y) in
		let uusX = !tabel.(!fiks).(!y) in
		let uusY = !tabel.(!x).(!fiks) in
		if uusX = max_int || uusY = max_int || uusX + uusY >= senine
			then (
				tekst := "Kuna " ^ string_of_lahter(uusX) ^ " + " ^ string_of_lahter(uusY) ^ " >= " ^ string_of_lahter(senine) ^
					", siis me lahtris olevat v��rtust ei muuda.";
					i := LahtriVaatlus
			)
			else (
				tekst := "Kuna " ^ string_of_lahter(uusX) ^ " + " ^ string_of_lahter(uusY) ^ " < " ^ string_of_lahter(senine) ^
				", siis uuendame lahtris olevat v��rtust.";
				i := LahtriMuutmine
			)
	);;

(* lahtri v��rtuse muutmine *)
let lahtriMuutmine() =
	!tabel.(!x).(!y) <- !tabel.(!fiks).(!y) + !tabel.(!x).(!fiks);
	tekst := "Muudame lahtris olevat v��rtust.";
	i := LahtriVaatlus;;

(* algoritmi l�pp *)
let lopp() =
	tekst := "Floyd-Warshalli algoritm l�petab, olles leidnud k�ikide tippude v�himad kaugused �ksteisest.";
	fiks := !fiks + 1;	(* kuvamise jaoks *)
	AlgoBaas.lopp();;
	
(* algoritmi samm *)
let samm(tipud, servad) = 
	match !i with
		| Algus -> algus(tipud, servad)
		| Fikseerimine -> fikseerimine(tipud)
		| LahtriVaatlus -> lahtriVaatlus(tipud)
		| LahtriMuutmine -> lahtriMuutmine()
		| Lopp -> lopp()
		| _ -> ();;