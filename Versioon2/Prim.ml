open Struktuurid;;

(*type primiSamm = Algus
	| EsimeneTipp
	| ServaVaatlus		(*vaatleme v�imalikke servi, mida lisada*)
	| ServaValik		(*valime neist l�hima *)
	| ServaLisamine		(*lisame selle ja talle vastava tipu puusse *)
	| Lopp				(*kuvame tekkinud minimaalse toesepuu*)
	| L2bi;;

let i = ref(Algus);; (* counter algoritmi sammude jaoks*)*)

let algus() =
	tekst := "Primi algoritm alustab.";
	i := EsimeneTipp;;

(* kui serva �ks tipp on vaadeldud ja teine mitte, m�rgime serva ja vaatlemata tipu vaadeldavateks*)
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
		| [] -> print_endline("�htegi serva ei lisatud. Ei tohiks juhtuda.");;
	
	
let lopetaTipuVaatlus(tipp) =
	if !(tipp.tv) = Vaadeldav then tipp.tv := Vaatlemata;;
		
let lopetaServaVaatlus(serv) =
	if !(serv.sv) = Vaadeldav then serv.sv := Vaatlemata;;
		
let esimeneTipp(algtipp, tipud) =
	tekst := "M�rgime algtipu k�lastatuks. Sellest hakkame toesepuud ehitama.";
	algtipp.tv := Vaadeldud;
	if List.for_all tippVaadeldud tipud (*TODO: v�i ei vii enam �htki serva siit v�lja*)
		then i := Lopp
	else i := ServaVaatlus;;

let servaVaatlus(servad) = 
	List.iter vaatle servad;
	tekst := "Vaatleme k�iki servi, mis �hendavad k�lastatud tippe k�lastamata tippudega."; (* TODO: konkreetsed servad? *)
	i := ServaValik;;
				
let servaValik(servad) = 
	let vaadeldavadServad = List.filter servVaadeldav servad in
	let lyhim = leiaLyhimServ(vaadeldavadServad) in
	match lyhim with
		| None -> print_endline("Midagi on viga.");
		| Some lyhimServ ->	(
			valiServ(lyhimServ);
			tekst := "Valime l�hima serva.";
		);
	i := ServaLisamine;;

let servaLisamine(tipud, servad) =
	lisaServ(servad);
	List.iter lopetaTipuVaatlus tipud;
	List.iter lopetaServaVaatlus servad;
	tekst := "Loeme serva ja vastava tipu k�lastatuks ning �hendame tekkinud puuga.";
	if List.for_all tippVaadeldud tipud (*TODO: v�i pole kuhugi enam minna*)
		then i := Lopp
	else i := ServaVaatlus;;

let lopp() = 
	tekst := "Algoritm l�petab, olles leidnud minimaalse toesepuu.";
	algoL2bi := true;
	i := L2bi;;
	
(*TODO: peaks kontrollima ka seda, et graaf ikka sidus oleks? Ja et k�ikidel servadel (mitteneg) kaalud oleks. *)
(*tipp - tipp, millest l�bim�ngu alustame; tipud - k�ik graafis esinevad tipud; servad - k�ik graafis esinevad servad *)
let prim(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus()
		| EsimeneTipp -> esimeneTipp(algtipp, tipud)
		| ServaVaatlus -> servaVaatlus(servad)
		| ServaValik -> servaValik(servad)
		| ServaLisamine -> servaLisamine(tipud, servad)
		| Lopp -> lopp()
		| L2bi -> ();;
		