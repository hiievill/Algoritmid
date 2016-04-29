open Struktuurid;;

(* siia k�ik funktsioonid ja muutujad, mida mitu algoritmi kasutavad *)

let i = ref(Algus);; (* counter algoritmi sammude jaoks*)

let tekst = ref("");;	(* algoritmi sammudel kuvatav tekst *)

let algo = ref(Laiuti);;

let algoL2bi = ref(false);;

let nk1 = ref("");;		(* graafi kohal kuvatavad nimekirjad *)
let nk2 = ref("");;
let nk3 = ref("");;

(* funktsioon, mis leiab kaalutud graafil mingist servade hulgast l�hima serva *)
let leiaLyhimServ(servad) =
	List.fold_left (fun s1 s2 -> if s1.kaal < s2.kaal then s1 else s2) (List.hd servad) (List.tl servad);;

(* funktsioon tippude j�rjendi s�nena esitamiseks *)
let string_of_tipud(tipud) =
	"[" ^ String.concat ", " (List.map (fun t -> t.nimi) tipud) ^ "]";;

(* funktsioon servade j�rjendi s�nena esitamiseks *)
let string_of_servad(servad) =
	"[" ^ String.concat ", " (List.map (fun s -> !(s.tipp1).nimi ^ !(s.tipp2).nimi ^ 
		" (" ^ match s.kaal with | None -> "-" | Some k -> string_of_int(k) ^ ")") servad) ^ "]";;

(* funktsioon k�lastatud tippude s�nena esitamiseks *)
let string_of_toodeldudTipud(tipud) =
	"T��deldud tipud: " ^ string_of_tipud(tipud);;

(* funktsioon servade eelistusj�rjekorra s�nena esitamiseks *)
let string_of_sej(servad) =
	"Servade eelistusj�rjekord: " ^ string_of_servad(servad);;

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

(* funktsioon, mis kontrollib, kas k�ik servad on suunaga v�i k�ik servad on suunata *)
let samadSuunad(servad) =
	List.for_all (fun s -> s.nool) servad || List.for_all (fun s -> s.nool = false) servad;;

(* funktsioon, mis kontrollib, et graaf sidus oleks. TODO: mujale t�sta v�i laiuti topelt teha *)
let kontrolliSidusust(algtipp, tipud, servad) =
	(*while !algoL2bi = false
		do
			Laiuti.laiuti(algtipp, tipud, servad)
		done;*)
	algoL2bi := false;
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud = false
		then failwith("Graaf peab sidus olema.")
	else (
		List.iter (fun t -> t.tv := Vaatlemata) tipud;
		List.iter (fun s -> s.sv := Vaatlemata) servad;
	);;

(* funktsioon, mis kontrollib graafi sobivust *)
let graafiKontroll(algtipp, tipud, servad, suunatus, kaaludega, hindadega, sidusus) =
	match suunatus with
		| None -> if samadSuunad(servad) = false then failwith("Graafis esineb nii suunatu kui mittesuunatud servi.")
		| Some b -> kontrolliSuunatust(servad, b);
	kontrolliKaale(servad, kaaludega);
	kontrolliHindu(tipud, hindadega);
	if sidusus then kontrolliSidusust(algtipp, tipud, servad);;
	(* TODO: kaalude m�rk (enamikul >= 0) ja t�klilisuse kontroll ka (topodel ei v�i �ldse, FW-l ei v�i neg ts�kleid) *)
	
(*tagastab true, kui vastav tipp on vaadeldud*)
let tippVaadeldud(tipp) =
	!(tipp.tv) = Vaadeldud;;

(* funktsioon, mis tagastab kahe serva puhul -1, kui 1. kaal on v�iksem, 1, kui 2. kaal on v�iksem, 0 muudel juhtudel *)
let v2iksemaKaaluga serv1 serv2 =
	match serv1.kaal with
		| None -> 0
		| Some k1 -> (
			match serv2.kaal with
				| None -> 0
				| Some k2 -> if k1 < k2 then -1 else if k1 > k2 then 1 else 0
		);; 

(* funktsioon, mis sorteerib servade listi kaalude p�hjal *)
let sordiJ2rjekord(servad) =
	List.sort v2iksemaKaaluga servad;;

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