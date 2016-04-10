open Struktuurid;;
open AlgoBaas;;

let j2rgmisedServad = ref([]);;

let kylastatudTipud = ref([]);;

let rec leiaJ2rgServad(tipp, servad) = 
	match servad with
		| x::xs -> (
			match x with 
				| {tipp1 = t1; tipp2 = t2; nool = n} -> 
					if n = false (*mittesuunatud graafide puhul*)
						then (
							if !t1 = tipp && !((!t2).tv) = Vaatlemata || !t2 = tipp && !((!t1).tv) = Vaatlemata
								then x :: leiaJ2rgServad(tipp, xs)
							else leiaJ2rgServad(tipp, xs)
						)
					else if n = true && !t1 = tipp && !((!t2).tv) = Vaatlemata
						then  x :: leiaJ2rgServad(tipp, xs)
					else leiaJ2rgServad(tipp, xs)
			)
		| [] -> [];;

let leiaLisatavTipp(serv) =
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> if !((!t1).tv) = Valitud then !t1 else !t2;;

let rec eemalda(servad) =
	match servad with
		| x::xs -> (match x with
			| {tipp1 = t1; tipp2 = t2;} -> (if !((!t1).tv) = Vaatlemata || !((!t2).tv) = Vaatlemata
				then x :: eemalda(xs) else eemalda(xs)
			) 
		)
		| [] -> [];;

let lisaServ(lisatavServ) = 
	match lisatavServ with
		| {tipp1 = t1; tipp2 = t2; sv = v} -> (
			v := Vaadeldud;
			(!t1).tv := Vaadeldud;
			(!t2).tv := Vaadeldud;
		);;


let algus() =
	(*AlgoBaas.graafiKontroll(...);*)
	tekst := "Laiuti läbimise algoritm alustab";
	i := EsimeneTipp;;
	
let esimeneTipp(algtipp, servad) =
	algtipp.tv := Vaadeldud;
	kylastatudTipud := (!kylastatudTipud) @ [algtipp];
	let js = leiaJ2rgServad(algtipp, servad) in
	j2rgmisedServad := (!j2rgmisedServad) @ js;
	tekst := "Märgime esimese tipu külastatuks.";
	i := ServaValik;;

let servaValik(servad) =
	let s = List.hd !j2rgmisedServad in
	j2rgmisedServad := List.tl !j2rgmisedServad;
	match s with
		| {tipp1 = t1; tipp2 = t2; sv = v;} -> ( (*märgime järgmise serva ja tipu valituks*)
			v := Valitud;
			if !((!t1).tv) = Vaatlemata then (!t1).tv := Valitud;
			if !((!t2).tv) = Vaatlemata then (!t2).tv := Valitud; 
		);
	let lisatavServ = List.find (fun s -> !(s.sv) = Valitud) servad in
	let lisatavTipp = leiaLisatavTipp(lisatavServ) in
	kylastatudTipud := (!kylastatudTipud) @ [lisatavTipp];
	let js = leiaJ2rgServad(lisatavTipp, servad) in
	j2rgmisedServad := (!j2rgmisedServad) @ js;
	j2rgmisedServad := eemalda(!j2rgmisedServad);
	tekst := "Valime järgmise tipu.";
	i := ServaLisamine;;

let servaLisamine(tipud, servad) =
	let lisatavServ = List.find (fun s -> !(s.sv) = Valitud) servad in
	lisaServ(lisatavServ);
	tekst := "Märgime selle tipu külastatuks.";
	(*if List.for_all tippVaadeldud tipud*) (*TODO: vÃµi ei vii enam Ã¼htki serva siit vÃ¤lja*)
	if List.length !j2rgmisedServad = 0
		then i := Lopp
	else i := ServaValik;;

let lopp() =
	AlgoBaas.lopp("Algoritm lõpetab, olles leidnud laiuti otsingu otsingupuu. \n" ^ string_of_tippude_j2rjekord(!kylastatudTipud));;
		
let laiuti(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus();
		| EsimeneTipp -> esimeneTipp(algtipp, servad);
		| ServaValik -> servaValik(servad);
		| ServaLisamine -> servaLisamine(tipud, servad);
		| Lopp -> lopp();
		| _ -> ();;

