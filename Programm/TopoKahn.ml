(* moodul TopoKahn teostab sammsammulist l�bim�ngu topoloogilise j�rjestuse leidmiseks Kahni algoritmiga *) 

open Struktuurid;;
open AlgoBaas;;

let sisendastmed = Hashtbl.create 10;; 		(*sisuliselt map tipp.nimi : int *)

let tekkinudJ2rjestus = ref([]);; 				(* siia tekkinud topoloogiline j�rjestus *)

(* funktsioon tippude sisendastmete s�nena esitamiseks *)
let string_of_sisendastmed(tipud) =
	let j2rjestatudTipud = sordiTipud(tipud) in
	let s = List.fold_left (fun acc t -> acc ^ t.nimi ^ ": " ^ string_of_int(Hashtbl.find sisendastmed t.nimi) ^ ", ") "" j2rjestatudTipud in
	(*let s = Hashtbl.fold (fun k v acc -> k ^ ": " ^ string_of_int(v) ^ ", " ^ acc) sisendastmed "" in*)
  "Tippude sisendastmed: [" ^ String.sub s 0 (String.length(s) - 2) ^ "]";;

(* funktsioon tippude topoloogilise j�rjestuse s�nena esitamiseks *)
let string_of_topo() =
	"Topoloogiline j�rjestus: " ^ string_of_tipud(!tekkinudJ2rjestus);;

(* funktsioon sisendastme uuendamiseks Hashtblis *)
let uuendaSisendastet nr tipp =
	Hashtbl.replace sisendastmed tipp.nimi nr;;

(* funktsioon tipu sisendastme leidmiseks *)
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
	Random.self_init();
	let nsat = List.filter (fun t -> Hashtbl.find sisendastmed t.nimi = 0 && !(t.tv) <> Vaadeldud) tipud in (*nullise sisendastmega tipud *)
	if List.length nsat = 0
		then (
			(*failwith("Pole �htegi k�lastamata tippu, mille sisendaste oleks 0. Ei tohiks juhtuda.");*)
			let vaatlemataTipud = List.filter (fun t -> !(t.tv) <> Vaadeldud) tipud in
			List.nth vaatlemataTipud (Random.int (List.length vaatlemataTipud))
		)
	else List.nth nsat (Random.int (List.length nsat));;

(* funktsioon, mis juhul, kui serva l�htetipp on valitud, m�rgib serva vaadeldavaks, v�hendab tema sisendastet ja *)
(* m�rgib vaatlemata sihttipu vaadeldavaks*)
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
	
(* funktsioon, mis m�rgib tipu vaadelduks ja lisab ta tekkinud j�rjestuse l�ppu *)
let lisaTipp(tipp) =
	tipp.tv := Vaadeldud;
	tekkinudJ2rjestus := !tekkinudJ2rjestus @ [tipp];;

(* funktsioon, mis uuendab graafi kohal kuvatavaid nimekirju *)
let lisatekst(tipud) =
	nk1 := string_of_sisendastmed(tipud);
	nk2 := string_of_topo();;

(* algoritmi algus *)
let algus() =
	Random.self_init();
	tekst := "Kahni algoritm topoloogilise j�rjestuse leidmiseks alustab. ";
	i := Vahe1;;

(* paneme iga tipuga vastavusse tema sisendastme *)
let vahe1(tipud, servad) =
	List.iter (fun t -> uuendaSisendastet (leiaSisendaste(t, servad)) t) tipud;
	tekst := "M��rame iga tipuga vastavusse temasse sisenevate servade arvu.";
	nk1 := string_of_sisendastmed(tipud);
	i := TipuValik;;

(* suvalise sisendastemga 0 tipu valimine *)
let tipuValik(tipud) =
	tekst := "Valime suvaliselt �he tipu, mille sisendaste on 0.";
	lisatekst(tipud);
	let valitudTipp = valiTipp(tipud) in
	valitudTipp.tv := Valitud;
	i := ServaVaatlus;;

(* k�ikide valitud tipust v�ljuvate servade vaatlemine ja sihttippude sisendastmete v�hendamine *)
let servaVaatlus(tipud, servad) =
	List.iter vaatleServa servad;
	tekst := "Vaatleme k�iki servi, mis valitud tipust v�ljuvad, ja v�hendame sihttippude sisendastet 1 v�rra.";
	lisatekst(tipud);
	i := ServaLisamine;;

(* valitud tipu lisamine topoloogilisse j�rjestusse *)
let servaLisamine(tipud, servad) =
	List.iter (fun t -> if !(t.tv) = Valitud then lisaTipp t) tipud;
	List.iter (fun t -> if !(t.tv) = Vaadeldav then t.tv := Vaatlemata) tipud;
					(* k�ik teised tipud uuesti Vaadeldav -> Vaatlemata *)
	List.iter (fun s -> if !(s.sv) = Vaadeldav then s.sv := Vaadeldud) servad;
					(* k�ik Vaadeldavad servad -> Vaadeldud *)
	tekst := "Lisame valitud tipu topoloogilisse j�rjestusse.";
	lisatekst(tipud);
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud
		then i := Lopp
	else i := TipuValik;;

(* algoritmi l�pp *)
let lopp(tipud) =
	tekst := "K�ik tipud on vaadeldud. Algoritm l�petab, olles leidnud sunatud graafi topoloogilise j�rjestuse.";
	lisatekst(tipud);
	AlgoBaas.lopp();;

(* algoritmi samm *)
let samm(tipud, servad) = 
	match !i with
		| Algus -> algus()
		| Vahe1 -> vahe1(tipud, servad)
		| TipuValik -> tipuValik(tipud)
		| ServaVaatlus -> servaVaatlus(tipud, servad)
		| ServaLisamine -> servaLisamine(tipud, servad)
		| Lopp -> lopp(tipud)
		| L2bi -> ()
		| _ -> ();;