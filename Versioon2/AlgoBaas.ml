open Struktuurid;;

(* siia k�ik funktsioonid ja muutujad, mida mitu algoritmi kasutavad *)

let i = ref(Algus);; (* counter algoritmi sammude jaoks*)

let tekst = ref("");;	(* algoritmi sammudel kuvatav tekst *)

let algo = ref(Laiuti);;

let algoL2bi = ref(false);;

(* funktsioon, mis leiab kaalutud graafil mingist servade hulgast l�hima serva *)
let leiaLyhimServ(servad) =
	List.fold_left (fun s1 s2 -> if s1.kaal < s2.kaal then s1 else s2) (List.hd servad) (List.tl servad);;

(* funktsioon tippude j�rjendi s�nena esitamiseks *)
let string_of_tipud(tipud) =
	String.concat ", " (List.map (fun t -> t.nimi) tipud);;

(* funktsioon servade j�rjendi s�nena esitamiseks *)
let string_of_servad(servad) =
	String.concat ", " (List.map (fun s -> !(s.tipp1).nimi ^ !(s.tipp2).nimi) servad);;

(* funktsioon, mis kontrollib, et graaf sidus oleks. TODO. *)
let kontrolliSidusust() = ();;

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

let graafiKontroll(servad, sidusus, suunatus, kaaludega) =
	if sidusus then kontrolliSidusust();
	kontrolliSuunatust(servad, suunatus);
	kontrolliKaale(servad, kaaludega);;
	(* TODO: kaalude m�rk (enamikul >= 0) ja t�klilisuse kontroll ka (topodel ei v�i �ldse, FW-l ei v�i neg ts�kleid) *)
	
(*tagastab true, kui vastav tipp on vaadeldud*)
let tippVaadeldud(tipp) =
	!(tipp.tv) = Vaadeldud;;

(* alguses refi t�itmiseks *)
let tyhiTipp = {
	nimi = "-";
	x = ref(0);
	y = ref(0);
	tv = ref(Vaatlemata);
	hind = None
};;

let lopp() =
	algoL2bi := true;
	i := L2bi;;