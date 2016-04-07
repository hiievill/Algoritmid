open Struktuurid;;

(* siia kõik funktsioonid ja muutujad, mis algoritmidel ühised on *)

let i = ref(Algus);; (* counter algoritmi sammude jaoks*)

let tekst = ref("");;	(* algoritmi sammudel kuvatav tekst *)

(* funktsioon, mis leiab kaalutud graafil minist servade hulgast lühima serva *)
let leiaLyhimServ(servad) =
	List.fold_left (fun s1 s2 -> if s1.kaal < s2.kaal then s1 else s2) (List.hd servad) (List.tl servad);;

(* funktsioon, mis kontrollib, et graaf sidus oleks. TODO. *)
let kontrolliSidusust() = ();;

(* funktsioon, mis kontrollib, et graafi servad oleks kas kõik suunatud või kõik ilma suunata. TODO. *)
let kontrolliSuunatust(servad, suunatus) = ();;

(* funktsioon, mis kontrollib, et graafi servad oleks kas kõik kaaludega või kõik ilma kaaludeta. TODO. *)
let kontrolliKaale(servad, kaaludega) = ();;

let graafiKontroll(servad, sidusus, suunatus, kaaludega) =
	if sidusus then kontrolliSidusust();
	kontrolliSuunatust(servad, suunatus);
	kontrolliKaale(servad, kaaludega);;
	(* TODO: kaalude märk (enamikul >= 0) ja tüklilisuse kontroll ka (topodel ei või üldse, FW-l ei või neg tsükleid) *)
	

let lopp(loppTekst) =
	tekst := loppTekst;
	algoL2bi := true;
	i := L2bi;;