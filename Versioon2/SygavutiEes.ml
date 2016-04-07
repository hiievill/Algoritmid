open Struktuurid;;
open Laiuti;;
open AlgoBaas;;

let algus() =
	(*AlgoBaas.graafiKontroll(...);*)
	tekst := "Sügavuti eesjärjestuses läbimise algoritm alustab";
	i := EsimeneTipp;;

let servaValik(servad) = (* sama mis laiutil, ainult 1 rida erinev *)
	let s = List.hd !j2rgmisedServad in
	j2rgmisedServad := List.tl !j2rgmisedServad;
	match s with
		| {tipp1 = t1; tipp2 = t2; sv = v;} -> (
			v := Valitud;
			if !((!t1).tv) = Vaatlemata then (!t1).tv := Valitud;
			if !((!t2).tv) = Vaatlemata then (!t2).tv := Valitud; 
		);
	let lisatavServ = List.find (fun s -> !(s.sv) = Valitud) servad in
	let lisatavTipp = leiaLisatavTipp(lisatavServ) in
	kylastatudTipud := (!kylastatudTipud) @ [lisatavTipp];
	let js = leiaJ2rgServad(lisatavTipp, servad) in
	j2rgmisedServad := js @ (!j2rgmisedServad);
	j2rgmisedServad := eemalda(!j2rgmisedServad);
	tekst := "Valime järgmise tipu.";
	i := ServaLisamine;;

let sygavutiEes(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus();
		| EsimeneTipp -> Laiuti.esimeneTipp(algtipp, servad);
		| ServaValik -> servaValik(servad);
		| ServaLisamine -> Laiuti.servaLisamine(tipud, servad);
		| Lopp -> Laiuti.lopp();
		| _ -> ();;