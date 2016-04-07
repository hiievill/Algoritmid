open Struktuurid;;
open AlgoBaas;;

let kaugused = Hashtbl.create 10;; (* sisuliselt map tipp.nimi : int *)

(*funktsioon, mis tagastab stringina kõikide tippude kaugused algtipust *)
let string_of_kaugused() =
	let s = Hashtbl.fold (fun k v acc -> k ^ ": " ^ (if v = max_int then "inf" else string_of_int(v)) ^ ", " ^ acc) kaugused "" in
  "Tippude kaugused algtipust: " ^ String.sub s 0 (String.length(s) - 2);;

let valitudTipp = ref(None);;

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
			if uusKaugus < senineKaugus then lisaKaugus uusKaugus !t2
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

let algus(algtipp, tipud, servad) =
	AlgoBaas.graafiKontroll(servad, false, true, true);
	List.iter (lisaKaugus max_int) tipud; (* paneme kõikidele tippudele algseks kauguseks algtipust suurima võimaliku *)
	lisaKaugus 0 algtipp; (* ainult algtipule 0 *)
	tekst := "Dijkstra algoritm alustab.";
	i := EsimeneTipp;;

let esimeneTipp(algtipp) =
	tekst := "Valime esimese tipu.";
	algtipp.tv := Valitud;
	valitudTipp := Some algtipp;
	i := ServaVaatlus;;

let servaVaatlus(servad) =
	match !valitudTipp with
	| None -> failwith("Valitud tipp puudub. Ei tohiks juhtuda.")
	| Some vt -> (
  	let vaadeldavadServad = leiaV2ljuvadServad(vt, servad) in
  	List.iter vaatleServa vaadeldavadServad;
  	List.iter uuendaKaugust vaadeldavadServad;
  	tekst := "Vaatleme tipust väljuvaid servi ning uuendame vastavate tippude kaugusi, kui need on senisest väiksemad.";
		tekst := string_of_kaugused();
		i := ServaLisamine
	);;

let servaLisamine(algtipp, tipud, servad) =
	tekst := "Märgime valitud tipu vaadelduks.";
	if !valitudTipp = Some algtipp
		then algtipp.tv := Vaadeldud
	else (
		assert (List.length (List.filter (fun s -> !(s.sv) = Valitud) servad) = 1); (* kontrollime, et vaid 1 serv oleks valitud *)
		List.iter (fun s -> if !(s.sv) = Valitud then lisaServ(s)) servad; (* märgime vaadelduks*)
	);
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud
		then i := Lopp
	else i := ServaValik;;

let servaValik(servad) =
	let vs = List.filter valitavServ servad in (* leiame vaadeldavad servad *)
	let ls = AlgoBaas.leiaLyhimServ(vs) in
	valiServ(ls);
	valitudTipp := Some !(ls.tipp2);
	tekst := "Valime lühima serva, mis ühendab vaadeldud ja vaatlemata tippe.";
	i := ServaVaatlus;;

let lopp() =
	AlgoBaas.lopp("Algoritm lõpetab, olles leidnud kõikide tippude vähima kauguse algtipust. \n" ^ string_of_kaugused());;

let dijkstra(algtipp, tipud, servad) =
	match !i with
		| Algus -> algus(algtipp, tipud, servad)
		| EsimeneTipp -> esimeneTipp(algtipp)		(* vaatleme tippu (märgime valituks) *)
		| ServaVaatlus -> servaVaatlus(servad)
				(* vaatleme kõiki sellest tipust väljuvaid servi (vaadeldavad) ja uuendame kaugusi*)
		| ServaLisamine -> servaLisamine(algtipp, tipud, servad)		(* märgime tipu (ja serva) vaadelduks *)
		| ServaValik -> servaValik(servad)			(* valime lühima serva, mis ühendab Vaadeldud ja Vaadeldavat tippu *)
		| Lopp -> lopp()
		| _ -> ();;