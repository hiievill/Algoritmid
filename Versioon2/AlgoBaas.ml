open Struktuurid;;

(* siia k�ik funktsioonid ja muutujad, mis algoritmidel �hised on *)

let i = ref(Algus);; (* counter algoritmi sammude jaoks*)

let tekst = ref("");;	(* algoritmi sammudel kuvatav tekst *)

(* funktsioon, mis leiab kaalutud graafil minist servade hulgast l�hima serva *)
let leiaLyhimServ(servad) =
	List.fold_left (fun s1 s2 -> if s1.kaal < s2.kaal then s1 else s2) (List.hd servad) (List.tl servad);;

(* funktsioon, mis kontrollib, et graaf sidus oleks. TODO. *)
let kontrolliSidusust() = ();;

(* funktsioon, mis kontrollib, et graafi servad oleks kas k�ik suunatud v�i k�ik ilma suunata. TODO. *)
let kontrolliSuunatust(servad, suunatus) = ();;

(* funktsioon, mis kontrollib, et graafi servad oleks kas k�ik kaaludega v�i k�ik ilma kaaludeta. TODO. *)
let kontrolliKaale(servad, kaaludega) = ();;

let graafiKontroll(servad, sidusus, suunatus, kaaludega) =
	if sidusus then kontrolliSidusust();
	kontrolliSuunatust(servad, suunatus);
	kontrolliKaale(servad, kaaludega);;
	(* TODO: kaalude m�rk (enamikul >= 0) ja t�klilisuse kontroll ka (topodel ei v�i �ldse, FW-l ei v�i neg ts�kleid) *)
	

let lopp(loppTekst) =
	tekst := loppTekst;
	algoL2bi := true;
	i := L2bi;;