open Struktuurid;;

(*type primiSamm = Algus
	| EsimeneTipp
	| ServaVaatlus		(*vaatleme võimalikke servi, mida lisada*)
	| ServaValik		(*valime neist lühima *)
	| ServaLisamine		(*lisame selle ja talle vastava tipu puusse *)
	| Lopp				(*kuvame tekkinud minimaalse toesepuu*)
	| L2bi;;

let i = ref(Algus);; (* counter algoritmi sammude jaoks*)*)

let algus() =
	tekst := "Primi algoritm alustab.";
	i := EsimeneTipp;;

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
				

	
let rec leiaLyhimServ(servad) =
	match servad with
		| x::xs -> (
			let lyhim = leiaLyhimServ(xs) in
			match lyhim with
				| Some lyhimServ -> if x.kaal < lyhimServ.kaal then Some x else lyhim
				| None -> Some x
		)
		| [] -> None;;

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
	let lyhim = leiaLyhimServ(vaadeldavadServad) in
	match lyhim with
		| None -> print_endline("Midagi on viga.");
		| Some lyhimServ ->	(
			valiServ(lyhimServ);
			tekst := "Valime lühima serva.";
		);
	i := ServaLisamine;;

let servaLisamine(tipud, servad) =
	lisaServ(servad);
	List.iter lopetaTipuVaatlus tipud;
	List.iter lopetaServaVaatlus servad;
	tekst := "Loeme serva ja vastava tipu külastatuks ning ühendame tekkinud puuga.";
	if List.for_all tippVaadeldud tipud (*TODO: või pole kuhugi enam minna*)
		then i := Lopp
	else i := ServaVaatlus;;

let lopp() = 
	tekst := "Algoritm lõpetab, olles leidnud minimaalse toesepuu.";
	algoL2bi := true;
	i := L2bi;;
	
(*TODO: peaks kontrollima ka seda, et graaf ikka sidus oleks? Ja et kõikidel servadel (mitteneg) kaalud oleks. *)
(*tipp - tipp, millest läbimängu alustame; tipud - kõik graafis esinevad tipud; servad - kõik graafis esinevad servad *)
let prim(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus()
		| EsimeneTipp -> esimeneTipp(algtipp, tipud)
		| ServaVaatlus -> servaVaatlus(servad)
		| ServaValik -> servaValik(servad)
		| ServaLisamine -> servaLisamine(tipud, servad)
		| Lopp -> lopp()
		| L2bi -> ();;
		