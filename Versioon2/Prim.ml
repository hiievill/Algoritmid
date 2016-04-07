open Struktuurid;;
open AlgoBaas;;

(* kui serva üks tipp on vaadeldud ja teine mitte, märgime serva ja vaatlemata tipu vaadeldavateks*)
let vaatle(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> 
			if !((!t1).tv) = Vaadeldud && !((!t2).tv) <> Vaadeldud
				then (
					(!t2).tv := Vaadeldav;
					serv.sv := Vaadeldav;
				)
			else if !((!t2).tv) = Vaadeldud && !((!t1).tv) <> Vaadeldud
				then (
					(!t1).tv := Vaadeldav;
					serv.sv := Vaadeldav
				);;

(* tagastab true, kui serv on vaadeldav *)
let servVaadeldav(serv) = !(serv.sv) = Vaadeldav;;
		
let valiServ(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> (
			if !((!t1).tv) = Vaadeldav then (!t1).tv := Valitud;
			if !((!t2).tv) = Vaadeldav then (!t2).tv := Valitud;
			serv.sv := Valitud;
		);;
		
let rec lisaServ(servad) =
	match servad with
		| x::xs -> (
			match x with
				| {tipp1 = t1; tipp2 = t2; sv = v} -> (
					if !v = Valitud 
						then (
							v := Vaadeldud;
							(!t1).tv := Vaadeldud;
							(!t2).tv := Vaadeldud
						)
					else lisaServ(xs)
				);
		)
		| [] -> print_endline("Ühtegi serva ei lisatud. Ei tohiks juhtuda.");;
	
	
let lopetaTipuVaatlus(tipp) =
	if !(tipp.tv) = Vaadeldav then tipp.tv := Vaatlemata;;
		
let lopetaServaVaatlus(serv) =
	if !(serv.sv) = Vaadeldav then serv.sv := Vaatlemata;;

let algus(servad) =
	AlgoBaas.graafiKontroll(servad, true, false, true);
	tekst := "Primi algoritm alustab.";
	i := EsimeneTipp;;
		
let esimeneTipp(algtipp, tipud) =
	tekst := "Märgime algtipu külastatuks. Sellest hakkame toesepuud ehitama.";
	algtipp.tv := Vaadeldud;
	if List.for_all tippVaadeldud tipud (*TODO: või ei vii enam ühtki serva siit välja*)
		then i := Lopp
	else i := ServaVaatlus;;

let servaVaatlus(servad) = 
	List.iter vaatle servad;
	tekst := "Vaatleme kõiki servi, mis ühendavad külastatud tippe külastamata tippudega."; (* TODO: konkreetsed servad? *)
	i := ServaValik;;
				
let servaValik(servad) = 
	let vaadeldavadServad = List.filter servVaadeldav servad in
	let lyhim = AlgoBaas.leiaLyhimServ(vaadeldavadServad) in
	valiServ(lyhim);
	tekst := "Valime lühima serva.";
	i := ServaLisamine;;

let servaLisamine(tipud, servad) =
	lisaServ(servad);
	List.iter lopetaTipuVaatlus tipud;
	List.iter lopetaServaVaatlus servad;
	tekst := "Loeme serva ja vastava tipu külastatuks ning ühendame tekkinud puuga.";
	if List.for_all tippVaadeldud tipud
		then i := Lopp
	else i := ServaVaatlus;;

let lopp() = 
	AlgoBaas.lopp("Algoritm lõpetab, olles leidnud minimaalse toesepuu.");;
	
(*tipp - tipp, millest läbimängu alustame; tipud - kõik graafis esinevad tipud; servad - kõik graafis esinevad servad *)
let prim(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus(servad)
		| EsimeneTipp -> esimeneTipp(algtipp, tipud)
		| ServaVaatlus -> servaVaatlus(servad)
		| ServaValik -> servaValik(servad)
		| ServaLisamine -> servaLisamine(tipud, servad)
		| Lopp -> lopp()
		| L2bi -> ();;
		