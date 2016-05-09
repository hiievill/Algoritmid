(* moodul Eeldusgraaf teostab sammsammulist eeldusgraafi analüüsi läbimängu *) 

open Struktuurid;;
open AlgoBaas;;

let vl = Hashtbl.create 10;; 							(* tippude varaseimad lõpuajad *)
let ha = Hashtbl.create 10;; 							(* tippude hiliseimad algusajad *)

let v2ljundastmed = Hashtbl.create 10;;		(* tippude väljundastmed *)

let kriitilineTee = ref([]);;							(* kriitiline tee *)

(* funktsioon, mis tagastab tipu vahetud eellased *)
let leiaTipuEellased(tipp, servad) =
	List.map (fun s -> !(s.tipp1)) (List.filter (fun s -> !(s.tipp2) = tipp) servad);;

(* funktsioon, mis tagastab tipu vahetud järglased *)
let leiaTipuJ2rglased(tipp, servad) =
	List.map (fun s -> !(s.tipp2)) (List.filter (fun s -> !(s.tipp1) = tipp) servad);;

(* funktsioon, mis leiab tippude varaseimate lõpuaegade hulgast suurima *)
let leiaSuurimVL(tipud) = 
	let lopuajad = List.map (fun t -> Hashtbl.find vl t.nimi) tipud in
	List.fold_left (fun a b -> if a < b then b else a) (List.hd lopuajad) (List.tl lopuajad);;

(* funktsioon, mis leiab tippude hiliseimate algusaegade hulgast vähima *)
let leiaV2himHA(tipud) =
	let algusajad = List.map (fun t -> Hashtbl.find ha t.nimi) tipud in
	List.fold_left (fun a b -> if a < b then a else b) (List.hd algusajad) (List.tl algusajad);;

(* kui tipu sisendaste oli 0, siis tema varaseimaks lõpusajaks tema hind, else max (temasse sisenevad) + hind. *)
let lisaVaraseimLopuaeg(tipp, servad) =
	match !(tipp.hind) with
		| None -> failwith("Tipul pole hinda.")
		| Some h -> (
    	if Hashtbl.find TopoKahn.sisendastmed tipp.nimi = 0
    		then (
					tekst := "Kuna tipul eellasi pole, määrame tema varaseimaks lõpuajaks tipu hinna.";
					Hashtbl.add vl tipp.nimi h
				)
    	else (
    		let eellased = leiaTipuEellased(tipp, servad) in
				tekst := "Määrame tipu varaseimaks lõpuajaks tema hinna ja tema vahetute eellaste suurima varaseima lõpuaja summa.";
    		Hashtbl.add vl tipp.nimi ((leiaSuurimVL(eellased)) + h)
    	)
	);;

(* kui tipu väljundaste on 0, siis projektiAeg - hind, muidu tema vahetute järglaste väikseim aeg - hind *)
let lisaHiliseimAlgusaeg(tipp, servad, projektiAeg) =
	match !(tipp.hind) with
		| None -> failwith("Tipul pole hinda.")
		| Some h -> (
    	if Hashtbl.find v2ljundastmed tipp.nimi = 0
				then (
					tekst := "Kuna tipul järglasi pole, määrame tema hiliseimaks algusaks projekti varaseima lõpuaja ja tipu hinna vahe.";
    			Hashtbl.add ha tipp.nimi (projektiAeg - h)
				)
    	else (
    		let j2rglased = leiaTipuJ2rglased(tipp, servad) in
				tekst := "Määrame tipu hiliseimaks algusajaks tipu vahetute järglaste vähima hiliseima algusaja ja tipu hinna vahe.";
    		Hashtbl.add ha tipp.nimi ((leiaV2himHA(j2rglased)) - h)
    	)
	);;

(* funktsioon tipu väljundastme leidmiseks *)
let leiaV2ljundaste(tipp, servad) =
	List.length (List.filter ((fun t s -> !(s.tipp1) = tipp) tipp) servad);;

(* funktsioon, mis tagastab, kas tipp on kriitiline (st kas tema hind + hiliseim algusaeg = varaseim lõpuaeg *)
let onKriitiline(tipp) =
	match !(tipp.hind) with
		| None ->  failwith("Tipul pole hinda.")
		| Some h -> Hashtbl.find vl tipp.nimi = Hashtbl.find ha tipp.nimi + h

(* funktsioon väljundastme uuendamiseks *)
let uuendaV2ljundastet nr tipp =
	Hashtbl.replace v2ljundastmed tipp.nimi nr;;

(* funktsioon tippude varaseimate lõpuaegade sõnena esitamiseks *)
let string_of_varaseimadLopuajad(tipud) =
	"Varaseimad lõpuajad: [" ^ String.concat ", " (List.map (fun t -> t.nimi ^ ": " ^ if Hashtbl.mem vl t.nimi then string_of_int(Hashtbl.find vl t.nimi) else "-") tipud) ^ "]";;
	
(* funktsioon tippude hiliseimate algusaegade sõnena esitamiseks *)
let string_of_hiliseimadAlgusajad(tipud) =
	"Hiliseimad algusajad: [" ^ String.concat ", " (List.map (fun t -> t.nimi ^ ": " ^ if Hashtbl.mem ha t.nimi then string_of_int(Hashtbl.find ha t.nimi) else "-") tipud) ^ "]";;

(* funktsioon tagurpidi topoloogilise järjestuse sõnena esitamiseks *)
let string_of_tagurpidiTopo() =
	"Tagurpidi topoloogiline järjestus: [" ^ String.concat ", " (List.map (fun t -> t.nimi) (List.rev !(TopoKahn.tekkinudJ2rjestus))) ^ "]";;

(* funktsioon kriitilise tee sõnena esitamiseks *)
let string_of_kriitilineTee()=
	"Kriitiline tee: " ^ string_of_tipud(!kriitilineTee);;

(* algoritmi algus *)
let algus() =
	tekst := "Teostame eeldusgraafi analüüsi: leiame tippude hiliseima algusaja, varaseima lõpuaja ja kriitilise tee.";
	i := Topo;;

(* graafi topoloogilise järjestuse leidmine *)
let topo(tipud, servad) =
	i := Algus;
	while !algoL2bi = false
		do
			TopoKahn.samm(tipud, servad)
		done;
	algoL2bi := false;
	tekst := "Leiame graafi topoloogilise järjestuse.";
	nk1 := TopoKahn.string_of_topo();
	nk2 := "";
	i := Vahe1;;

(* graafi ja sisendastmete algoleku taastamine *)
let vahe1(tipud, servad) =
	tekst := "Hakkame tippe topoloogilises järjestuses läbima ja neile varasemaid lõpuaegu määrama.";
	nk1 := TopoKahn.string_of_topo();
	nk2 := string_of_varaseimadLopuajad(tipud);
	List.iter (fun t -> t.tv := Vaatlemata) tipud;
	List.iter (fun s -> s.sv := Vaatlemata) servad;
	List.iter (fun t -> TopoKahn.uuendaSisendastet (TopoKahn.leiaSisendaste(t, servad)) t) tipud;
	(* reset'ime sisendastmed, sest TopoKahni käigus muutsime neid *)
	List.iter (fun t -> uuendaV2ljundastet (leiaV2ljundaste(t, servad)) t) tipud;
	i := TipuValikVL;;

(* topoloogilises järjestuses järgmise vaatlemata tipu valituks märkimine *)
let tipuValikVL(tipud, servad) =
	tekst := "Valime topoloogilises järjestuses järgmise vaatlemata tipu.";
	nk1 := TopoKahn.string_of_topo();
	nk2 := string_of_varaseimadLopuajad(tipud);
	let valitudTipp = List.find (fun t -> !(t.tv) = Vaatlemata) !(TopoKahn.tekkinudJ2rjestus) in
	valitudTipp.tv := Valitud;
	i := TipuLisamineVL;;

(* valitud tipu ja vastava serva vaadelduks märkimine ning tipule varaseima lõpuaja määramine *)
let tipuLisamineVL(tipud, servad) = 
	let valitudTipp = List.find (fun t -> !(t.tv) = Valitud) tipud in
	List.iter (fun s -> if !(s.tipp2) = valitudTipp then s.sv := Valitud) servad;
	lisaVaraseimLopuaeg(valitudTipp, servad);
	List.iter (fun t -> if !(t.tv) = Valitud then t.tv := Vaadeldud) tipud;
	List.iter (fun s -> if !(s.sv) = Valitud then s.sv := Vaadeldud) servad;
	nk1 := TopoKahn.string_of_topo();
	nk2 := string_of_varaseimadLopuajad(tipud);
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud
		then i := ProjektiAeg
	else i := TipuValikVL;;

(* kogu projekti varaseima lõpuaja arvutamine *)
let projektiAeg(tipud) =
	tekst := "Leiame kogu projekti varaseima lõpuaja, milleks on järglasteta tippude suurim varaseim lõpuaeg.";
	nk3 := "Kogu projekti varaseim lõpuaeg: " ^ string_of_int(leiaSuurimVL(tipud));
	i := Vahe2;;

(* vaheteksti kuvamine *)
let vahe2(tipud, servad) =
	tekst := "Hakkame tippe topoloogilises järjestuses tagurpidi läbima ja neile hilisemaid algusaegu määrama.";
	nk1 := string_of_varaseimadLopuajad(tipud);
	nk2 := "Kogu projekti varaseim lõpuaeg: " ^ string_of_int(leiaSuurimVL(tipud));
	nk3 := TopoKahn.string_of_topo();
	nk4 := string_of_tagurpidiTopo();
	List.iter (fun t -> t.tv := Vaatlemata) tipud;
	List.iter (fun s -> s.sv := Vaatlemata) servad;
	i := TipuValikHA;;

(* tagurpidi topoloogilises järjestuses järgmise vaatlemata tipu valituks märkimine *)
let tipuValikHA(tipud, servad) =
	let valitudTipp = List.find (fun t -> !(t.tv) = Vaatlemata) (List.rev !(TopoKahn.tekkinudJ2rjestus)) in
	valitudTipp.tv := Valitud;
	tekst := "Valime tagurpidi topoloogilises järjestuses järgmise vaatlemata tipu.";
	nk3 := string_of_tagurpidiTopo();
	nk4 := string_of_hiliseimadAlgusajad(tipud);
	i := TipuLisamineHA;;

(* valitud ja vastava serva vaadelduks märkimine ja tipule hiliseima algusaja määramine *)
let tipuLisamineHA(tipud, servad) =
	let projektiAeg = leiaSuurimVL(tipud) in
	let valitudTipp = List.find (fun t -> !(t.tv) = Valitud) tipud in
	List.iter (fun s -> if !(s.tipp1) = valitudTipp then s.sv := Valitud) servad;
	lisaHiliseimAlgusaeg(valitudTipp, servad, projektiAeg);
	List.iter (fun t -> if !(t.tv) = Valitud then t.tv := Vaadeldud) tipud;
	List.iter (fun s -> if !(s.sv) = Valitud then s.sv := Vaadeldud) servad;
	nk3 := string_of_tagurpidiTopo();
	nk4 := string_of_hiliseimadAlgusajad(tipud);
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud
		then i := Vahe3
	else i := TipuValikHA;;

(* vaheteksti kuvamine *)
let vahe3(tipud, servad) =
	tekst := "Leiame kriitilise tee.";
	nk3 := string_of_hiliseimadAlgusajad(tipud);
	nk4 := "";
	i := Kriitiline;;

(* esimese kriitilise tipu leidmine ja valituks märkimine *)
(*let esimeneKriitiline(tipud) =
	let kriitilineTipp = List.find (fun t -> Hashtbl.find ha t.nimi = 0) tipud in
	kriitilineTipp.tv := Valitud;
	kriitilineTee := [kriitilineTipp];
	tekst := "Leiame esimese kriitilise tipu - sellise tipu, mille hiliseim algusaeg on 0.";
	nk4 := string_of_kriitilineTee();
	if List.length tipud = 1
		then i := Lopp
	else i := Kriitiline;;*)

(* kriitilise serva ja vastava kriitilise tipu leidmine ja valituks märkimine *)
(*let kriitiline(tipud, servad) =
	let projektiAeg = leiaSuurimVL(tipud) in
	let kriitilineServ = List.find (fun s -> !(!(s.tipp1).tv) = Valitud && !(!(s.tipp2).tv) <> Valitud && onKriitiline(!(s.tipp2))) servad in
	let kriitilineTipp = !(kriitilineServ.tipp2) in
	kriitilineServ.sv := Valitud;
	kriitilineTipp.tv := Valitud;
	kriitilineTee := !kriitilineTee @ [kriitilineTipp];
	tekst := "Leiame järgmise kriitilise tipu - sellise tipu, mille hiliseima algusaja ja hinna summa on võrdne tema varaseima lõpetamisajaga.";
	nk4 := string_of_kriitilineTee();
	if Hashtbl.find vl kriitilineTipp.nimi = projektiAeg
		then i := Lopp
	else i := Kriitiline;;*)
	
let kriitiline(tipud, servad) =
	List.iter (fun t -> if onKriitiline(t) then t.tv := Valitud) tipud;
	List.iter (fun s -> if onKriitiline(!(s.tipp1)) && onKriitiline(!(s.tipp2)) then s.sv := Valitud) servad;
	tekst := "Leiame kõik kriitilised tipud ja nende kaudu moodustuva kriitilise tee.";
	kriitilineTee := List.filter (fun t -> !(t.tv) = Valitud) tipud;
	nk4 := string_of_kriitilineTee();
	i := Lopp;;

(* algoritmi lõpp *)
let lopp() =
	tekst := "Kriitiline tee on leitud ja sellega on eldusgraafi analüüs lõppenud.";
	nk4 := string_of_kriitilineTee();
	AlgoBaas.lopp();;

(* algoritmi samm *)
let samm(tipud, servad) = 
	match !i with
		| Algus -> algus()
		| Topo -> topo(tipud, servad)
		| Vahe1 -> vahe1(tipud, servad)
		| TipuValikVL -> tipuValikVL(tipud, servad)
		| TipuLisamineVL -> tipuLisamineVL(tipud, servad)
		| ProjektiAeg -> projektiAeg(tipud)
		| Vahe2 -> vahe2(tipud, servad)
		| TipuValikHA -> tipuValikHA(tipud, servad)
		| TipuLisamineHA -> tipuLisamineHA(tipud, servad)
		| Vahe3 -> vahe3(tipud, servad)
		(*| EsimeneKriitiline -> esimeneKriitiline(tipud)*)
		| Kriitiline -> kriitiline(tipud, servad)
		| Lopp -> lopp()
		| _ -> ();;