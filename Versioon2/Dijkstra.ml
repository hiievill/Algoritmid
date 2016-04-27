open Struktuurid;;
open AlgoBaas;;

let kaugused = Hashtbl.create 10;; (* sisuliselt map tipp.nimi : int *)

(*funktsioon, mis tagastab stringina k�ikide tippude kaugused algtipust *)
let string_of_kaugused() =
	let s = Hashtbl.fold (fun k v acc -> k ^ ": " ^ (if v = max_int then "inf" else string_of_int(v)) ^ ", " ^ acc) kaugused "" in
  "Tippude kaugused algtipust: " ^ String.sub s 0 (String.length(s) - 2);;

let valitudTipp = ref(tyhiTipp);;

(* funktsioon, mis leiab k�ik tipust tipp v�ljuvad servad *)
let leiaV2ljuvadServad(tipp, servad) =
	List.filter ((fun t s -> !(s.tipp1) = t) tipp) servad;;

(* funktsioon, mis m��rab tipule kauguse *)
let lisaKaugus kaugus tipp =
	Hashtbl.replace kaugused tipp.nimi kaugus;;

(* funktsioon, mis muudab tipust tipp v�ljuva serva teises otsas oleva tipu kaugust (tipu tipp kaudu), *)
(* kui see on senisest l�hem*)
let uuendaKaugust(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; kaal = Some k;} -> (
			let senineKaugus = Hashtbl.find kaugused !t2.nimi in
			let uusKaugus = (Hashtbl.find kaugused !t1.nimi) + k in
			if uusKaugus < senineKaugus then lisaKaugus uusKaugus !t2
		)
		| _ -> failwith("Serval puudub kaal. Seda et tohiks juhtuda.");;

(* funktsioon, mis m�rgib serva ja tema tipp2 vaadeldavaks (v.a. kui viimane juba valitud/vaadeldud on) *)
let vaatleServa(serv) =
	serv.sv := Vaadeldav;
	if !(!(serv.tipp2).tv) = Vaatlemata then !(serv.tipp2).tv := Vaadeldav;;

(* funktsioon, mis m�rgib serva ja tema tipp2 valituks (v.a. kui viimane juba vaadeldud on) *)
let valiServ(serv) =
	serv.sv := Valitud;
	if !(!(serv.tipp2).tv) <> Vaadeldud then !(serv.tipp2).tv := Valitud;;

(* funktsioon, mis m�rgib serva ja tema tipp2 vaadelduks *)
let lisaServ(serv) =
	serv.sv := Vaadeldud;
	!(serv.tipp2).tv := Vaadeldud;;

(* funktsioon, mis tagastab, kas serv on valitav, st on vaadeldav ja tema tipp2 pole vaadeldud *)
let valitavServ(serv) =
	match serv with
		| {tipp2 = t2; sv = v} -> (
			!v = Vaadeldav && !(!t2.tv) <> Vaadeldud
		);;

(* funktsioon, mis tagastab tipu, mille kaugus algtipust on k�ige v�iksem *)
let leiaL2himTipp(tipud) =
	List.fold_left (fun t1 t2 -> if Hashtbl.find kaugused t1.nimi <=  Hashtbl.find kaugused t2.nimi then t1 else t2) (List.hd tipud) (List.tl tipud);;

(* funktsioon, mis tagastab, kas serv on tipule vastav, st mille teise tipu kaugus + serv.kaal on tipu kaugus *)
let vastavServ tipp serv =
	match serv.kaal with
		| None -> false
		| Some k -> !(serv.tipp2) = tipp && Hashtbl.find kaugused !(serv.tipp1).nimi + k = Hashtbl.find kaugused tipp.nimi;;

let algus(algtipp, tipud, servad) =
	(*AlgoBaas.graafiKontroll(servad, false, true, true);*)
	List.iter (lisaKaugus max_int) tipud; 			(* paneme k�ikidele tippudele algseks kauguseks algtipust suurima v�imaliku *)
	lisaKaugus 0 algtipp; 											(* ainult algtipule paneme kauguseks 0 *)
	tekst := "Dijkstra algoritm alustab. M��rame k�ikidele tippudele kaugused algtipust: algtipule 0, k�ikidele teistele l�pmatuse.";
	tekst := !tekst ^ "\n" ^ string_of_kaugused();
	i := EsimeneTipp;;

let esimeneTipp(algtipp) =
	tekst := "Valime esimese tipu.";
	tekst := !tekst ^ "\n" ^ string_of_kaugused();
	algtipp.tv := Valitud;											(* m�rgime algtipu valituks *)
	valitudTipp := algtipp;
	i := ServaLisamine;;

let servaLisamine(algtipp, tipud, servad) =
	tekst := "M�rgime valitud tipu vaadelduks.";
	tekst := !tekst ^ "\n" ^ string_of_kaugused();
	if !valitudTipp = algtipp																								(* kui valitud tipp on algtipp *)
		then algtipp.tv := Vaadeldud																					(* m�rgime ta vaadelduks *)
	else List.iter (fun s -> if !(s.sv) = Valitud then lisaServ(s)) servad; (* muidu m�rgime serva koos tipuga vaadelduks*)
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud										(* kui k�ik tipu on vaadeldud, l�hme l�pule *)
  		then i := Lopp
	else i := ServaVaatlus;;																								(* vastasel juhul l�hme tippude kaugusi uuendama *)

let servaVaatlus(tipud, servad) =
  	let vaadeldavadServad = leiaV2ljuvadServad(!valitudTipp, servad) in		(* leiame �sja k�lastatud tipust v�ljuvad servad *)
  	List.iter vaatleServa vaadeldavadServad;															(* m�rgime need vaadelduks *)
  	List.iter uuendaKaugust vaadeldavadServad;														(* uuendame nende 2. tippude kaugusi algtipust *)
  	tekst := "Vaatleme tipust v�ljuvaid servi ning uuendame vastavate tippude kaugusi, kui need on senisest v�iksemad.";
		tekst := !tekst ^ "\n" ^ string_of_kaugused();
  	i := ServaValik;;

let servaValik(tipud, servad) =
	let t = leiaL2himTipp(List.filter (fun t -> !(t.tv) = Vaadeldav) tipud) in		(* leiame algtipule l�hima k�lastamata tipu*)
	let s = List.find (vastavServ t) servad in																		(* ja talle vastava serva *)
	valiServ(s);																																	(* m�rgime serva ja tipu valituks *)
	valitudTipp := t;
	tekst := "Valime algtipule l�hima k�lastamata tipu.";
	tekst := !tekst ^ "\n" ^ string_of_kaugused();
	i := ServaLisamine;;

let lopp() =
	tekst := "Algoritm l�petab, olles leidnud k�ikide tippude v�hima kauguse algtipust. \n" ^ string_of_kaugused();
	AlgoBaas.lopp();;

let dijkstra(algtipp, tipud, servad) =
	match !i with
		| Algus -> algus(algtipp, tipud, servad)
		| EsimeneTipp -> esimeneTipp(algtipp)		(* vaatleme tippu (m�rgime valituks) *)
		| ServaVaatlus -> servaVaatlus(tipud, servad)
				(* vaatleme k�iki sellest tipust v�ljuvaid servi (vaadeldavad) ja uuendame kaugusi*)
		| ServaLisamine -> servaLisamine(algtipp, tipud, servad)		(* m�rgime tipu (ja serva) vaadelduks *)
		| ServaValik -> servaValik(tipud, servad)			(* valime l�hima serva, mis �hendab Vaadeldud ja Vaadeldavat tippu *)
		| Lopp -> lopp()
		| _ -> ();;