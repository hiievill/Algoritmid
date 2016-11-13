(* moodul Dijkstra teostab sammsammulist Dijkstra algoritmi läbimängu *) 

open Struktuurid;;
open AlgoBaas;;

let kaugused = Hashtbl.create 10;; 		(* sisuliselt map tipp.nimi : int *)

let valitudTipp = ref(tyhiTipp);;			(* valitud tipp *)

(* funktsioon, mis tagastab kahe tipu puhul -1, kui 1. kaugus on väiksem, 1, kui 2. kaugus on väiksem, 0 muudel juhtudel *)
let v2iksemaKaugusega tipp1 tipp2 =
	let k1 = Hashtbl.find kaugused tipp1.nimi in
	let k2 = Hashtbl.find kaugused tipp2.nimi in
	if k1 < k2 then -1 else if k1 > k2 then 1 else 0;; 

let v2iksemaKaugusega serv1 serv2 =
	let k1 = (match serv1.kaal with | Some k -> k | None -> 0) + Hashtbl.find kaugused !(serv1.tipp2).nimi in
	let k2 = (match serv2.kaal with | Some k -> k | None -> 0) + Hashtbl.find kaugused !(serv2.tipp2).nimi in
	if k1 < k2 then -1 else if k1 > k2 then 1 else 0;; 

(* funktsioon, mis sorteerib tippude listi kauguste põhjal algtipust *)
let sordiTipud(tipud) =
	List.sort v2iksemaKaugusega tipud;;

let sordiServad(servad) =
	List.sort v2iksemaKaugusega servad;;

(*funktsioon, mis tagastab sõnena kõikide mittevaadeldud tippude kaugused algtipust *)
let string_of_vaadeldavateKaugused(servad) =
	let vaadeldavadServad = sordiServad(List.filter (fun s -> !(s.sv) = Vaadeldav) servad) in
	let s = List.fold_left (fun a b -> a ^ ", " ^ !(b.tipp1).nimi ^ !(b.tipp2).nimi ^ ": " ^ string_of_int((match b.kaal with | Some k -> k | None -> 0) + Hashtbl.find kaugused !(b.tipp1).nimi)) "" vaadeldavadServad in
	"Servade eelistusjärjekord: [" ^ (if s = "" then "" else String.sub s 2 (String.length s - 2)) ^ "]";;

(*funktsioon, mis tagastab sõnena kõikide tippude kaugused algtipust *)
let string_of_kaugused() =
	let s = Hashtbl.fold (fun k v acc -> k ^ ": " ^ (if v = max_int then "inf" else string_of_int(v)) ^ ", " ^ acc) kaugused "" in
  "Tippude kaugused algtipust: " ^ String.sub s 0 (String.length(s) - 2);;

(* funktsioon, mis leiab kõik tipust tipp väljuvad servad *)
let leiaV2ljuvadServad(tipp, servad) =
	List.filter ((fun t s -> !(s.tipp1) = t) tipp) servad;;

(* funktsioon, mis määrab tipule kauguse *)
let lisaKaugus kaugus tipp =
	Hashtbl.replace kaugused tipp.nimi kaugus;;

(* funktsioon, mis muudab tipust tipp väljuva serva teises otsas oleva tipu kaugust (tipu tipp kaudu), *)
(* kui see on senisest lühem*)
let uuendaKaugust(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; kaal = Some k;} -> (
			let senineKaugus = Hashtbl.find kaugused !t2.nimi in
			let uusKaugus = (Hashtbl.find kaugused !t1.nimi) + k in
			if uusKaugus < senineKaugus 
				then (
					lisaKaugus uusKaugus !t2;
					(*!t2.hind := Some uusKaugus*)
				)
		)
		| _ -> failwith("Serval puudub kaal. Seda et tohiks juhtuda.");;

(* funktsioon, mis märgib serva ja tema tipp2 vaadeldavaks (v.a. kui viimane juba valitud/vaadeldud on) *)
let vaatleServa(serv) =
	serv.sv := Vaadeldav;
	if !(!(serv.tipp2).tv) = Vaatlemata then !(serv.tipp2).tv := Vaadeldav;;

(* funktsioon, mis märgib serva ja tema tipp2 valituks (v.a. kui viimane juba vaadeldud on) *)
let valiServ(serv) =
	serv.sv := Valitud;
	if !(!(serv.tipp2).tv) <> Vaadeldud then !(serv.tipp2).tv := Valitud;;

(* funktsioon, mis märgib serva ja tema tipp2 vaadelduks *)
let lisaServ(serv) =
	serv.sv := Vaadeldud;
	!(serv.tipp2).tv := Vaadeldud;;

(* funktsioon, mis tagastab, kas serv on valitav, st on vaadeldav ja tema tipp2 pole vaadeldud *)
let valitavServ(serv) =
	match serv with
		| {tipp2 = t2; sv = v} -> (
			!v = Vaadeldav && !(!t2.tv) <> Vaadeldud
		);;

(* funktsioon, mis tagastab tipu, mille kaugus algtipust on kõige väiksem *)
let leiaL2himTipp(tipud) =
	List.fold_left (fun t1 t2 -> if Hashtbl.find kaugused t1.nimi <=  Hashtbl.find kaugused t2.nimi then t1 else t2) (List.hd tipud) (List.tl tipud);;

(* funktsioon, mis tagastab, kas serv on tipule vastav, st mille teise tipu kaugus + serv.kaal on tipu kaugus *)
let vastavServ tipp serv =
	match serv.kaal with
		| None -> false
		| Some k -> !(serv.tipp2) = tipp && Hashtbl.find kaugused !(serv.tipp1).nimi + k = Hashtbl.find kaugused tipp.nimi;;

(* algoritmi algus, mille käigus määratakse kõikidele tippudele kaugused algtipust *)
let algus(algtipp, tipud, servad) =
	List.iter (lisaKaugus max_int) tipud; 			(* paneme kõikidele tippudele algseks kauguseks algtipust suurima võimaliku *)
	lisaKaugus 0 algtipp; 											(* ainult algtipule paneme kauguseks 0 *)
	tekst := "Dijkstra algoritm alustab valitud algtipust. Määrame kõikidele tippudele kaugused algtipust: algtipule 0, kõikidele teistele lõpmatuse.";
	(*List.iter (fun t -> t.hind := Some (Hashtbl.find kaugused t.nimi)) tipud;*)
	nk1 := string_of_vaadeldavateKaugused(servad);
	valitudTipp := algtipp;
	i := ServaLisamine;;

(* valitud tipu ja vastava serva vaadelduks märkimine *)
let servaLisamine(algtipp, tipud, servad) =
	if !valitudTipp = algtipp																								(* kui valitud tipp on algtipp *)
		then algtipp.tv := Vaadeldud																					(* märgime ta vaadelduks *)
	else List.iter (fun s -> if !(s.sv) = Valitud then lisaServ(s)) servad; (* muidu märgime serva koos tipuga vaadelduks*)
	!valitudTipp.hind := Some (Hashtbl.find kaugused !valitudTipp.nimi);
	tekst := "Märgime valitud " ^ (if !valitudTipp = algtipp then "tipu" else "serva ja selle sihttipu") ^ " vaadelduks.";
	nk1 := string_of_vaadeldavateKaugused(servad);
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud										(* kui kõik tipu on vaadeldud, lähme lõpule *)
  		then i := Lopp
	else i := ServaVaatlus;;																								(* vastasel juhul lähme tippude kaugusi uuendama *)

(* äsja vaadeldud tipust väljuvate servade leidmine, vaadeldavateks märkimine ja nende sihttippude kauguste uuendamine *)
let servaVaatlus(servad) =
  	let vaadeldavadServad = leiaV2ljuvadServad(!valitudTipp, servad) in		(* leiame äsja külastatud tipust väljuvad servad *)
  	List.iter vaatleServa vaadeldavadServad;															(* märgime need vaadeldavateks *)
  	List.iter uuendaKaugust vaadeldavadServad;														(* uuendame nende 2. tippude kaugusi algtipust *)
  	tekst := "Vaatleme äsja vaadeldud tipust väljuvaid servi ning uuendame servade sihttippude kaugusi, kui need on senisest väiksemad.";
		nk1 := string_of_vaadeldavateKaugused(servad);
  	i := ServaValik;;

(* algtipule lähima külastamata tipu ja vastava serva leidmine ja valituks märkimine *)
let servaValik(tipud, servad) =
	let t = leiaL2himTipp(List.filter (fun t -> !(t.tv) = Vaadeldav) tipud) in		(* leiame algtipule lähima külastamata tipu*)
	let s = List.find (vastavServ t) servad in																		(* ja talle vastava serva *)
	valiServ(s);																																	(* märgime serva ja tipu valituks *)
	valitudTipp := t;
	tekst := "Valime eelistusjärjekorrast serva, mille sihttipu kaugus algtipust on vähim.";
	nk1 := string_of_vaadeldavateKaugused(servad);
	i := ServaLisamine;;

(* algoritmi lõpp *)
let lopp(tipud) =
	tekst := "Eelistusjärjekord sai tühjaks. Algoritm lõpetab, olles leidnud kõikide tippude vähima kauguse algtipust. Pildil on tekkinud kauguste puu.";
	nk1 := string_of_kaugused();
	AlgoBaas.lopp();;

(* algoritmi samm *)
let samm(algtipp, tipud, servad) =
	match !i with
		| Algus -> algus(algtipp, tipud, servad)
		| ServaVaatlus -> servaVaatlus(servad)
		| ServaLisamine -> servaLisamine(algtipp, tipud, servad)
		| ServaValik -> servaValik(tipud, servad)
		| Lopp -> lopp(tipud)
		| _ -> ();;
