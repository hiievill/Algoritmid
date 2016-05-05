open Struktuurid;;
open AlgoBaas;;

let tabel = ref([||]);;

let x = ref(-1);;		(* jooksev x *)
let y = ref(-1);;		(* jooksev y *)
let fiks = ref(-1);;	(* fikseeritud tulba ja veeru number *)

(* funktsioon servadevahelise kauguse sõnena kuvamiseks. Kui teed ei leidu, kuvatakse "inf", vastasel korral tee pikkus *)
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
	if !y = (Array.length !tabel) - 1	(* kui y loendur jõudis rea lõppu, paneme nulliks ja suurendame x loendurit *)
		then (
			y := 0;
			x := !x + 1
		)
	else y := !y + 1;;								(* vastasel korral lihtsalt suurendame y loendurit *)

let algus(tipud, servad) =
	tekst := "Floyd-Warshalli algoritm alustab. Tekitame graafi servade külgnevusmaatriksi.";
	let tippudeArv = List.length tipud in
	let maatriks = Array.make tippudeArv [||] in
	(* loome maatriksi *)
	for i = 0 to (tippudeArv - 1)
	do
		let rida = Array.make tippudeArv max_int in	(* algväärtuseks max_int *)
		maatriks.(i) <- rida
	done;
	(* täidame tippudevaheliste kaarte kaaludega *)
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

let fikseerimine(tipud) =
	x := 0;
	y := -1;
	fiks := !fiks + 1;
	(List.nth tipud !fiks).tv := Vaadeldav;
	tekst := "Fikseerime " ^ string_of_lahter(!fiks + 1) ^ ". veeru ja tulba ja hakkame ülejäänud lahtrites kaugusi uuendama.";
	tekst := !tekst ^ "\\n" ^ "Kui lahtris olev väärtus on suurem kui vastava valitud veeru ju tulba väärtuste summa, siis uuendame seda.";
	i := LahtriVaatlus;;

let lahtriVaatlus(tipud) =
	uuendaLoendureid();
	while !x = !fiks || !y = !fiks
	do
		uuendaLoendureid()
	done;
	if !x >= (Array.length !tabel)			(* kui tabel sai läbi, lähme kas uut rida&veergu fikseerima või lõpule *)
		then (
			tekst := "Kõik lahtrid said vaadatud.";
			List.iter (fun t -> if !(t.tv) = Vaadeldav then t.tv := Vaadeldud) tipud;
			if !fiks < Array.length !tabel - 1
				then (
					tekst := !tekst ^ "\\n" ^ "Lähme järgmist rida ja veergu fikseerima.";
					i := Fikseerimine
				)
			else (
				i := Lopp
			)
	)
	else (
		(*tekst := "Vaatleme lahtrit " ^ (List.nth tipud !x).nimi ^ (List.nth tipud !y).nimi;
		tekst := !tekst ^ "\n" ^ "Fikseeritud: " ^ string_of_int(!fiks);*)
  	if !x = !y
  		then (
  			tekst := "Tipu kaugust iseendast ei uuenda.";
				i := LahtriVaatlus
  		)
  	else (
  		let senine = !tabel.(!x).(!y) in
  		let uusX = !tabel.(!fiks).(!y) in
  		let uusY = !tabel.(!x).(!fiks) in
  		if uusX = max_int || uusY = max_int || uusX + uusY >= senine
  			then (
  				tekst := "Kuna " ^ string_of_lahter(uusX) ^ " + " ^ string_of_lahter(uusY) ^ " >= " ^ string_of_lahter(senine) ^
  					", siis me lahtris olevat väärtust ei muuda.";
						i := LahtriVaatlus
  			)
  			else (
  				tekst := "Kuna " ^ string_of_lahter(uusX) ^ " + " ^ string_of_lahter(uusY) ^ " < " ^ string_of_lahter(senine) ^
  				", siis uuendame lahtris olevat väärtust.";
  				i := LahtriMuutmine
  			)
  	)
	);;

let lahtriMuutmine() =
	!tabel.(!x).(!y) <- !tabel.(!fiks).(!y) + !tabel.(!x).(!fiks);
	tekst := "Muudame lahtris olevat väärtust.";
	i := LahtriVaatlus;;

let lopp() =
	tekst := "Floyd-Warshalli algoritm lõpetab, olles leidnud kõikide tippude vähimad kaugused teineteisest.";
	fiks := !fiks + 1;	(* kuvamise jaoks *)
	AlgoBaas.lopp();;
	

let floydWarshall(tipud, servad) = 
	match !i with
		| Algus -> algus(tipud, servad)
		| Fikseerimine -> fikseerimine(tipud)
		| LahtriVaatlus -> lahtriVaatlus(tipud)
		| LahtriMuutmine -> lahtriMuutmine()
		| Lopp -> lopp()
		| _ -> ();;