open Struktuurid;;

(* siia kõik funktsioonid ja muutujad, mida mitu algoritmi kasutavad *)

let i = ref(Algus);; (* counter algoritmi sammude jaoks*)

let tekst = ref("");;	(* algoritmi sammudel kuvatav tekst *)

let algo = ref(Laiuti);;

let algoL2bi = ref(false);;

let nk1 = ref("");;		(* graafi kohal kuvatavad nimekirjad *)
let nk2 = ref("");;
let nk3 = ref("");;

(* funktsioon, mis leiab kaalutud graafil mingist servade hulgast lühima serva *)
let leiaLyhimServ(servad) =
	List.fold_left (fun s1 s2 -> if s1.kaal < s2.kaal then s1 else s2) (List.hd servad) (List.tl servad);;

(* funktsioon tippude järjendi sõnena esitamiseks *)
let string_of_tipud(tipud) =
	"[" ^ String.concat ", " (List.map (fun t -> t.nimi) tipud) ^ "]";;

(* funktsioon servade järjendi sõnena esitamiseks *)
let string_of_servad(servad) =
	"[" ^ String.concat ", " (List.map (fun s -> !(s.tipp1).nimi ^ !(s.tipp2).nimi ^ 
		" (" ^ match s.kaal with | None -> "-" | Some k -> string_of_int(k) ^ ")") servad) ^ "]";;

(* funktsioon külastatud tippude sõnena esitamiseks *)
let string_of_toodeldudTipud(tipud) =
	"Töödeldud tipud: " ^ string_of_tipud(tipud);;

(* funktsioon servade eelistusjärjekorra sõnena esitamiseks *)
let string_of_sej(servad) =
	"Servade eelistusjärjekord: " ^ string_of_servad(servad);;
	
(*tagastab true, kui vastav tipp on vaadeldud*)
let tippVaadeldud(tipp) =
	!(tipp.tv) = Vaadeldud;;

(* funktsioon, mis tagastab kahe serva puhul -1, kui 1. kaal on väiksem, 1, kui 2. kaal on väiksem, 0 muudel juhtudel *)
let v2iksemaKaaluga serv1 serv2 =
	match serv1.kaal with
		| None -> 0
		| Some k1 -> (
			match serv2.kaal with
				| None -> 0
				| Some k2 -> if k1 < k2 then -1 else if k1 > k2 then 1 else 0
		);; 

(* funktsioon, mis sorteerib servade listi kaalude põhjal *)
let sordiJ2rjekord(servad) =
	List.sort v2iksemaKaaluga servad;;

(* alguses refi täitmiseks *)
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