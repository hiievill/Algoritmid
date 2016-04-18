open Struktuurid;;
open AlgoBaas;;

let sisendastmed = Hashtbl.create 10;; (*sisuliselt map tipp.nimi : int *)

let tekkinudJ2rjestus = ref([]);; (* siia tekkinud topoloogiline j�rjestus *)

let string_of_sisendastmed() =
	let s = Hashtbl.fold (fun k v acc -> k ^ ": " ^ string_of_int(v) ^ ", " ^ acc) sisendastmed "" in
  "Tippude sisendastmed: " ^ String.sub s 0 (String.length(s) - 2);;

let string_of_topo() =
	"Topoloogiline j�rjestus: " ^ String.concat ", " (List.map (fun t -> t.nimi) !tekkinudJ2rjestus);;

let uuendaSisendastet nr tipp = (*analoogiline fn-ga Dijkstra.lisaKaugus, kokku v�tta?*)
	Hashtbl.replace sisendastmed tipp.nimi nr;;

let leiaSisendaste(tipp, servad) =
	List.length (List.filter ((fun t s -> !(s.tipp2) = tipp) tipp) servad);;

(* v�hendame sisendtipu sisendastet (v.a. kui see juba 0 on) *)
let v2hendaSisendastet(serv) =
	let t = !(serv.tipp2) in
	let senineSisendaste = Hashtbl.find sisendastmed t.nimi in
	if senineSisendaste > 0
		then uuendaSisendastet (senineSisendaste - 1) t;;

(* valime suvaliselt tipu, millel on sisendaste 0 ning mis pole veel Vaadeldud.*)
let valiTipp(tipud) : Struktuurid.tipp =
	let nsat = List.filter (fun t -> Hashtbl.find sisendastmed t.nimi = 0 && !(t.tv) <> Vaadeldud) tipud in (*nullise sisendastmega tipud *)
	if List.length nsat = 0 
		then failwith("Pole �htegi k�lastamata tippu, mille sisendaste oleks 0. Ei tohiks juhtuda.");
	List.nth nsat (Random.int (List.length nsat));;

let vaatleServa(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2; sv = v} -> (
			if !(!t1.tv) = Valitud
				then (
					v := Vaadeldav;
					v2hendaSisendastet(serv);
					if !(!t2.tv) = Vaatlemata then !t2.tv := Vaadeldav 
				)
		);;
	
let lisaTipp(tipp) =
	tipp.tv := Vaadeldud;
	tekkinudJ2rjestus := !tekkinudJ2rjestus @ [tipp];;

let algus(tipud, servad) =
	List.iter (fun t -> uuendaSisendastet (leiaSisendaste(t, servad)) t) tipud;
	tekst := "Kahni algoritm topoloogilise j�rjestuse leidmiseks alustab.";
	tekst := "M��rame iga tipuga vastavusse temasse sisenevate servade arvu";
	tekst := string_of_sisendastmed();
	i := ServaValik;;

(* �igupoolest tipu valik *)
let servaValik(tipud) =
	tekst := "Valime suvaliselt �he tipu, mille sisendaste on 0.";
	let valitudTipp = valiTipp(tipud) in
	valitudTipp.tv := Valitud;
	(*List.iter (fun t -> print_endline(t.nimi ^ ": " ^ ( if !(t.tv) = Vaadeldud then "Vaadeldud" else if !(t.tv) = Vaatlemata then "Vaatlemata" else if !(t.tv) = Vaadeldav then "Vaadeldav" else "Valitud") ^ ", " ^ string_of_int(Hashtbl.find sisendastmed t.nimi))) tipud;*)
	i := ServaVaatlus;;

let servaVaatlus(servad) =
	List.iter vaatleServa servad;
	tekst := "Vaatleme k�iki servi, mis valitud tipust v�ljuvad, ja v�hendame sisendtippude sisendastet 1 v�rra.";
	tekst := string_of_sisendastmed();
	i := ServaLisamine;;

let servaLisamine(tipud, servad) =
	tekst := "Lisame valitud tipu topoloogilisse j�rjestusse.";
	List.iter (fun t -> if !(t.tv) = Valitud then lisaTipp t) tipud;
	List.iter (fun t -> if !(t.tv) = Vaadeldav then t.tv := Vaatlemata) tipud; 
					(* k�ik teised tipud uuesti Vaadeldav -> Vaatlemata *)
	List.iter (fun s -> if !(s.sv) = Vaadeldav then s.sv := Vaadeldud) servad;
					(* k�ik Vaadeldavad servad -> Vaadeldud *)
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud
		then i := Lopp
	else i := ServaValik;;

let lopp() =
	(*AlgoBaas.lopp("Algoritm l�petab, olles leidnud sunatud graafi topoloogilise j�rjestuse.");;*)
	AlgoBaas.lopp(string_of_topo());;

let topoKahn(tipud, servad) = 
	match !i with
		| Algus -> algus(tipud, servad)
		| ServaValik -> servaValik(tipud)
		| ServaVaatlus -> servaVaatlus(servad)
		| ServaLisamine -> servaLisamine(tipud, servad)
		| Lopp -> lopp()
		| L2bi -> ()
		| _ -> ();;