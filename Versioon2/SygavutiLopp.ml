open Struktuurid;;
open Laiuti;;
open AlgoBaas;;

let j2rgmisedServad = ref([]);;
let lisatavadTipud = ref([]);;
let vaadeldavTipp = ref(None);;

let leiaVaadeldavTipp(serv) = (*analoogiline nagu leiaLisatavTipp*)
	match serv with
		| {tipp1 = t1; tipp2 = t2;} -> if !((!t1).tv) = Vaadeldav then !t1 else !t2;;

let algus() =
	(*AlgoBaas.graafiKontroll(...);*)
	tekst := "Sügavuti lõppjärjestuses läbimise algoritm alustab";
	i := EsimeneTipp;;

let esimeneTipp(algtipp, servad) = 
	algtipp.tv := Vaadeldav;
	vaadeldavTipp := Some algtipp;
	tekst := "Külastame esimest tippu.";
	let js = leiaJ2rgServad(algtipp, servad) in
	j2rgmisedServad := js @ (!j2rgmisedServad);
	lisatavadTipud := algtipp :: !lisatavadTipud;
	i := ServaVaatlus;;

let servaVaatlus(servad) = 
	tekst := "Külastame järgmist tippu.";
	let s = List.hd !j2rgmisedServad in
	j2rgmisedServad := List.tl !j2rgmisedServad;
	match s with
		| {tipp1 = t1; tipp2 = t2; sv = v;} -> ( (*märgime järgmise serva ja tipu vaadeldavaks*)
			v := Vaadeldav;
			if !((!t1).tv) = Vaatlemata then (
				(!t1).tv := Vaadeldav;
				vaadeldavTipp := Some !t1
			);
			if !((!t2).tv) = Vaatlemata then (
				(!t2).tv := Vaadeldav; 
				vaadeldavTipp := Some !t2
			);
		);
	match !vaadeldavTipp with
		| None -> print_endline("Mingi viga.")
		| Some vt -> (
				lisatavadTipud := vt :: !lisatavadTipud;
      	let js = leiaJ2rgServad(vt, servad) in
      	print_endline(vt.nimi ^ ", " ^ string_of_int(List.length js));
      	if List.length js = 0 (* sügavamale ei lähe, hakkame tippe/servi lisama *)
      		then (
      			i := ServaValik
      		)
      	else (
      		j2rgmisedServad := js @ (!j2rgmisedServad);
      		i := ServaVaatlus
  			)
  		);;

let servaValik(algtipp, servad) = 
	tekst := "Valime tipu.";
	let t = List.hd !lisatavadTipud in (* leiame tipu, mida lisama hakkame *)
	t.tv := Valitud;
	if t = algtipp 
		then ()
	else (
		let valitavServ = List.find ((fun t s -> !(s.sv) = Vaadeldav && (!(s.tipp1) = t || !(s.tipp2) = t)) t) servad in
		valitavServ.sv := Valitud
	);
	i := ServaLisamine;;

let servaLisamine(algtipp, servad) =
	tekst := "Loeme tipu külastatuks.";
	let t = List.hd !lisatavadTipud in (* leiame tipu, mida lisama hakkame *)
	print_endline(string_of_tipp(t));
	lisatavadTipud := List.tl !lisatavadTipud;
	t.tv := Vaadeldud;
	if t = algtipp
		then i := Lopp
	else (
		let valitavServ = List.find ((fun t s -> !(s.sv) = Valitud && (!(s.tipp1) = t || !(s.tipp2) = t)) t) servad in
		valitavServ.sv := Valitud;
		i := ServaVaatlus (*õige??*)
	);;

let lopp() = 
	AlgoBaas.lopp("Algoritm lõpetab, olles leidnud laiuti otsingu otsingupuu.");;

let sygavutiLopp(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus();
		| EsimeneTipp -> esimeneTipp(algtipp, servad);
		| ServaVaatlus -> servaVaatlus(servad);
		| ServaValik -> servaValik(algtipp, servad);
		| ServaLisamine -> servaLisamine(algtipp, servad);
		| Lopp -> lopp();
		| _ -> ();;