(* moodulis AlgoBaas on kõik funktsioonid ja muutujad, mida mitu algoritmi kasutavad, sh teksti- ja nimekirjaväljad, *)
(* kaarte andmed ja nimekirjade sõnena esitamise funktsioonid. *)

open Struktuurid;;

let algo = ref(Laiuti);;				(* läbimängitav algoritm *)
let algoL2bi = ref(false);;			(* muutuja, mis kannab infot, kas algoritmi läbimäng on pooleli või läbi *)
let i = ref(Algus);; 						(* loendur algoritmi sammude jaoks*)
let sammuNr = ref(0);;					(* algoritmi sammude numbriline loendur *)
let tekst = ref("");;						(* algoritmi sammudel kuvatav tekst *)
let programmK2ib = ref(true);;	(* muutuja, mis kannab infot, kas programm käib või mitte (aken tuleks sulgeda) *)

let nk1 = ref("");;							(* graafi kohal kuvatavad nimekirjad *)
let nk2 = ref("");;
let nk3 = ref("");;
let nk4 = ref("");;

let prindiLoppTekst = ref(true);;	(* kas klahvidega "n" ja "b" läbi mängides viimast teksi printida või mitte *)

(* järgmised on mapid serva tippude nimed : int, hoiustamaks ringjoone võrrandit (x+a)^2 + (y+b)^2 = r^2 *)
let kaareX : (string, int) Hashtbl.t = Hashtbl.create 10;; (* ringjoone keskpunkti x-koordinaat *)
let kaareY : (string, int) Hashtbl.t = Hashtbl.create 10;; (* ringjoone keskpunkti y-koordinaat *)
let kaareR : (string, int) Hashtbl.t = Hashtbl.create 10;; (* ringjoone raadius *)
let kaareK : (string, int) Hashtbl.t = Hashtbl.create 10;; (* tippudevahelise lõigu keskpunkti minimaalne kaugus kaarest *)

(* funktsioon, mis uuendab kaare kõiki andmeid *)
let uuendaKaareAndmeid(tipp1, tipp2, x, y, r, k) =
	let nimi = tipp1.nimi ^ ":" ^ tipp2.nimi in
	Hashtbl.replace kaareX nimi x;
	Hashtbl.replace kaareY nimi y;
	Hashtbl.replace kaareR nimi r;
	Hashtbl.replace kaareK nimi k;;

(* funktsioon, mis leiab kaalutud graafil mingist servade hulgast lühima serva *)
let leiaLyhimServ(servad) =
	List.fold_left (fun s1 s2 -> if s1.kaal < s2.kaal then s1 else s2) (List.hd servad) (List.tl servad);;

(* funktsioon tippude järjendi sõnena esitamiseks *)
let string_of_tipud(tipud) =
	"[" ^ String.concat ", " (List.map (fun t -> t.nimi) tipud) ^ "]";;

(* funktsioon servade järjendi sõnena esitamiseks *)
let string_of_servad(servad) =
	"[" ^ String.concat ", " (List.map (fun s -> !(s.tipp1).nimi ^ !(s.tipp2).nimi ^ 
		match s.kaal with | None -> "" | Some k -> " (" ^ string_of_int(k) ^ ")") servad) ^ "]";;

(* funktsioon külastatud tippude sõnena esitamiseks *)
let string_of_toodeldudTipud(tipud) =
	"Töödeldud tipud: " ^ string_of_tipud(tipud);;

(* funktsioon servade eelistusjärjekorra sõnena esitamiseks *)
let string_of_sej(servad) =
	"Servade eelistusjärjekord: " ^ string_of_servad(servad);;
	
(* funktsioon, mis tagastab, kas vastav tipp on vaadeldud *)
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

(* funktsioon, mis tagastab kahe tipu puhul -1, kui 1. nimi on eespool, 1, kui 2. nimi on eespool, 0 muudel juhtudel *)
let v2iksemaNimegaTipud tipp1 tipp2 =
	if tipp1.nimi < tipp2.nimi then -1 else if tipp1.nimi > tipp2.nimi then 1 else 0;;

(* funktsioon, mis sorteerib tippude listi tähestikuliselt *)
let sordiTipud(tipud) =
	List.sort v2iksemaNimegaTipud tipud;;

(* tühi tipp alguses refi täitmiseks *)
let tyhiTipp = {
	nimi = "-";
	x = ref(0);
	y = ref(0);
	tv = ref(Vaatlemata);
	hind = ref(None)
};;

(* kõikide algoritmide ühine lõpusamm *)
let lopp() =
	algoL2bi := true;
	prindiLoppTekst := false;
	i := L2bi;;