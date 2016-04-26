open Struktuurid;;
open AlgoBaas;;

let vl = Hashtbl.create 10;; (* varaseim l�puaeg *)
let ha = Hashtbl.create 10;; (* hiliseim algusaeg *)

let v2ljundastmed = Hashtbl.create 10;;

let kriitilineTee = ref([]);;

let leiaTipuEellased(tipp, servad) =
	List.map (fun s -> !(s.tipp1)) (List.filter (fun s -> !(s.tipp2) = tipp) servad);;

let leiaTipuJ2rglased(tipp, servad) =
	List.map (fun s -> !(s.tipp2)) (List.filter (fun s -> !(s.tipp1) = tipp) servad);;

(* funktsioon, mis leiab tippude varaseimate l�puaegade hulgast suurima *)
let leiaSuurimVL(tipud) = 
	let lopuajad = List.map (fun t -> Hashtbl.find vl t.nimi) tipud in
	List.fold_left (fun a b -> if a < b then b else a) (List.hd lopuajad) (List.tl lopuajad);;

(* funktsioon, mis leiab tippude hiliseimate algusaegade hulgast v�hima *)
let leiaV2himHA(tipud) =
	let algusajad = List.map (fun t -> Hashtbl.find ha t.nimi) tipud in
	List.fold_left (fun a b -> if a < b then a else b) (List.hd algusajad) (List.tl algusajad);;

(* kui tipu sisendaste oli 0, siis tema varaseimaks l�pusajaks tema hind, else max (temasse sisenevad) + hind. *)
let lisaVaraseimLopuaeg(tipp, servad) =
	match tipp.hind with
		| None -> failwith("Tipul pole hinda.")
		| Some h -> (
    	if Hashtbl.find TopoKahn.sisendastmed tipp.nimi = 0
    		then Hashtbl.add vl tipp.nimi h
    	else (
    		let eellased = leiaTipuEellased(tipp, servad) in
    		Hashtbl.add vl tipp.nimi ((leiaSuurimVL(eellased)) + h)
    	)
	);;

(* kui tipu v�ljundaste on 0, siis projektiAeg - hind, muidu tema vahetute j�rglaste v�ikseim aeg - hind *)
let lisaHiliseimAlgusaeg(tipp, servad, projektiAeg) =
	match tipp.hind with
		| None -> failwith("Tipul pole hinda.")
		| Some h -> (
    	if Hashtbl.find v2ljundastmed tipp.nimi = 0
    		then Hashtbl.add ha tipp.nimi (projektiAeg - h)
    	else (
    		let j2rglased = leiaTipuJ2rglased(tipp, servad) in
    		Hashtbl.add ha tipp.nimi ((leiaV2himHA(j2rglased)) - h)
    	)
	);;

let leiaV2ljundaste(tipp, servad) =
	List.length (List.filter ((fun t s -> !(s.tipp1) = tipp) tipp) servad);;

(* funktsioon, mis tagastab, kas tipp on kriitiline (st kas tema hind + hiliseim algusaeg = varaseim l�puaeg *)
let onKriitiline(tipp) =
	match tipp.hind with
		| None ->  failwith("Tipul pole hinda.")
		| Some h -> Hashtbl.find vl tipp.nimi = Hashtbl.find ha tipp.nimi + h

let uuendaV2ljundastet nr tipp = (*analoogiline fn-ga Dijkstra.lisaKaugus, kokku v�tta?*)
	Hashtbl.replace v2ljundastmed tipp.nimi nr;;

(* funktsioon tippude varaseimate l�puaegade s�nena esitamiseks *)
let string_of_varaseimadLopuajad(tipud) =
	"Varaseimad l�puajad: " ^ String.concat ", " (List.map (fun t -> t.nimi ^ ": " ^ if Hashtbl.mem vl t.nimi then string_of_int(Hashtbl.find vl t.nimi) else "-") tipud);;
	
(* funktsioon tippude hiliseimate algusaegade s�nena esitamiseks *)
let string_of_hiliseimadAlgusajad(tipud) =
	"Hiliseimad algusajad: " ^ String.concat ", " (List.map (fun t -> t.nimi ^ ": " ^ if Hashtbl.mem ha t.nimi then string_of_int(Hashtbl.find ha t.nimi) else "-") tipud);;

(* funktsioon tagurpidi topoloogilise j�rjestuse s�nena esitamiseks *)
let string_of_tagurpidiTopo() =
	"Tagurpidi topoloogiline j�rjestus: " ^ String.concat ", " (List.map (fun t -> t.nimi) (List.rev !(TopoKahn.tekkinudJ2rjestus)));;

(* funktsioon kriitilise tee s�nena esitamiseks *)
let string_of_kriitilineTee()=
	"Kriitiline tee: " ^ string_of_tipud(!kriitilineTee);;

let algus() =
	tekst := "Teostame eeldusgraafi anal��si: leiame tippude hiliseima algusaja, varaseima l�puaja ja kriitilise tee.";
	i := Topo;;

let topo(tipud, servad) =
	i := Algus;
	while !algoL2bi = false
		do
			TopoKahn.topoKahn(tipud, servad)
		done;
	algoL2bi := false;
	tekst := "Leiame graafi topoloogilise j�rjestuse.";
	tekst := !tekst ^ "\n" ^ TopoKahn.string_of_topo();
	i := Vahe1;;

let vahe1(tipud, servad) =
	tekst := "Hakkame tippe topoloogilises j�rjestuses l�bima ja neile varasemaid l�puaegu m��rama.";
	tekst := !tekst ^ "\n" ^ TopoKahn.string_of_topo();
	List.iter (fun t -> t.tv := Vaatlemata) tipud;
	List.iter (fun s -> s.sv := Vaatlemata) servad;
	List.iter (fun t -> TopoKahn.uuendaSisendastet (TopoKahn.leiaSisendaste(t, servad)) t) tipud;
	(* reset'ime sisendastmed, sest TopoKahni k�igus muutsime neid *)
	List.iter (fun t -> uuendaV2ljundastet (leiaV2ljundaste(t, servad)) t) tipud;
	(* TODO: siin TopoKahn.uuendaSisendastet. Tegeleme siiski v�ljundastmetega. Teha Hashtbl.replace jaoks eri fn *)
	i := TipuValikVL;;

let tipuValikVL(tipud, servad) =
	tekst := "Valime topoloogilises j�rjestuses j�rgmise vaatlemata tipu.";
	tekst := !tekst ^ "\n" ^ TopoKahn.string_of_topo();
	tekst := !tekst ^ "\n" ^ string_of_varaseimadLopuajad(tipud);
	let valitudTipp = List.find (fun t -> !(t.tv) = Vaatlemata) !(TopoKahn.tekkinudJ2rjestus) in
	valitudTipp.tv := Valitud;
	i := TipuLisamineVL;;

let tipuLisamineVL(tipud, servad) = 
	let valitudTipp = List.find (fun t -> !(t.tv) = Valitud) tipud in
	List.iter (fun s -> if !(s.tipp2) = valitudTipp then s.sv := Valitud) servad;
	lisaVaraseimLopuaeg(valitudTipp, servad);
	List.iter (fun t -> if !(t.tv) = Valitud then t.tv := Vaadeldud) tipud;
	List.iter (fun s -> if !(s.sv) = Valitud then s.sv := Vaadeldud) servad;
	tekst := "M��rame tipule varaseima l�petamisaja, milleks on tema hind, kui tema sisendaste on 0, " ^
		"vastasel juhul tipu hinna ja tipu eellaste suurima varaseima l�puaja summa.";
	tekst := !tekst ^ "\n" ^ TopoKahn.string_of_topo();
	tekst := !tekst ^ "\n" ^ string_of_varaseimadLopuajad(tipud);
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud
		then i := Vahe2
	else i := TipuValikVL;;

let vahe2(tipud, servad) =
	tekst := "Hakkame tippe topoloogilises j�rjestuses tagurpidi l�bima ja neile hilisemaid algusaegu m��rama.";
	tekst := !tekst ^ "\n" ^ string_of_tagurpidiTopo();
	List.iter (fun t -> t.tv := Vaatlemata) tipud;
	List.iter (fun s -> s.sv := Vaatlemata) servad;
	i := TipuValikHA;;

let tipuValikHA(tipud, servad) =
	let valitudTipp = List.find (fun t -> !(t.tv) = Vaatlemata) (List.rev !(TopoKahn.tekkinudJ2rjestus)) in
	valitudTipp.tv := Valitud;
	tekst := "Valime tagurpidi topoloogilises j�rjestuses j�rgmise vaatlemata tipu.";
	tekst := !tekst ^ "\n" ^ string_of_hiliseimadAlgusajad(tipud);
	tekst := !tekst ^ "\n" ^ string_of_tagurpidiTopo();
	i := TipuLisamineHA;;

let tipuLisamineHA(tipud, servad) =
	let projektiAeg = leiaSuurimVL(tipud) in (* projekti varaseim l�puaeg *) (* TODO: et ei peaks seda pidevalt arvutama *)
	let valitudTipp = List.find (fun t -> !(t.tv) = Valitud) tipud in
	List.iter (fun s -> if !(s.tipp1) = valitudTipp then s.sv := Valitud) servad;
	lisaHiliseimAlgusaeg(valitudTipp, servad, projektiAeg);
	List.iter (fun t -> if !(t.tv) = Valitud then t.tv := Vaadeldud) tipud;
	List.iter (fun s -> if !(s.sv) = Valitud then s.sv := Vaadeldud) servad;
	tekst := "M��rame tipule hiliseima algusaja, milleks on kogu projekti varaseim l�puaeg, kui tal j�rglasi pole, " ^
		"vastasel juhul tipu j�rglaste v�hima hiliseima algusaja ja tipu hinna vahe.";
	tekst := !tekst ^ "\n" ^ string_of_hiliseimadAlgusajad(tipud);
	tekst := !tekst ^ "\n" ^ string_of_tagurpidiTopo();
	(* TODO: eelnev VL-ga kokku v�tta *)
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud
		then i := Vahe3
	else i := TipuValikHA;;

let vahe3(tipud, servad) =
	tekst := "Leiame kriitilise tee.";
	i := EsimeneKriitiline;;

let esimeneKriitiline(tipud) =
	let kriitilineTipp = List.find (fun t -> Hashtbl.find ha t.nimi = 0) tipud in
	kriitilineTipp.tv := Valitud;
	kriitilineTee := [kriitilineTipp];
	tekst := "Leiame esimese kriitilise tipu - sellise tipu, mille hiliseim algusaeg on 0.";
	tekst := !tekst ^ "\n" ^ string_of_kriitilineTee();
	i := Kriitiline;;
	
let kriitiline(tipud, servad) =
	let projektiAeg = leiaSuurimVL(tipud) in
	let kriitilineServ = List.find (fun s -> !(!(s.tipp1).tv) = Valitud && !(!(s.tipp2).tv) <> Valitud && onKriitiline(!(s.tipp2))) servad in
	let kriitilineTipp = !(kriitilineServ.tipp2) in
	kriitilineServ.sv := Valitud;
	kriitilineTipp.tv := Valitud;
	kriitilineTee := !kriitilineTee @ [kriitilineTipp];
	tekst := "Leiame j�rgmise kriitilise tipu - sellise tipu, mille hiliseima algusaja ja hinna summa on v�rdne tema varaseima l�petamisajaga.";
	tekst := !tekst ^ "\n" ^ string_of_kriitilineTee();
	if Hashtbl.find vl kriitilineTipp.nimi = projektiAeg
		then i := Lopp
	else i := Kriitiline;;

let lopp() =
	tekst := "Kriitiline tee on leitud ja sellega on eldusgraafi anal��s l�ppenud.";
	AlgoBaas.lopp();;

let eeldusgraaf(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus()
		| Topo -> topo(tipud, servad)
		| Vahe1 -> vahe1(tipud, servad)
		| TipuValikVL -> tipuValikVL(tipud, servad)
		| TipuLisamineVL -> tipuLisamineVL(tipud, servad)
		| Vahe2 -> vahe2(tipud, servad)
		| TipuValikHA -> tipuValikHA(tipud, servad)
		| TipuLisamineHA -> tipuLisamineHA(tipud, servad)
		| Vahe3 -> vahe3(tipud, servad)
		| EsimeneKriitiline -> esimeneKriitiline(tipud)
		| Kriitiline -> kriitiline(tipud, servad)
		| Lopp -> lopp()
		| _ -> ();;