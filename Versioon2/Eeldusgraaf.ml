open Struktuurid;;
open AlgoBaas;;

let vl = Hashtbl.create 10;; (* varaseim l�puaeg *)
let ha = Hashtbl.create 10;; (* hiliseim algusaeg *)

let v2ljundastmed = Hashtbl.create 10;;

let leiaTipuEellased(tipp, servad) =
	List.map (fun s -> !(s.tipp1)) (List.filter (fun s -> !(s.tipp2) = tipp) servad);;

let leiaTipuJ2rglased(tipp, servad) =
	List.map (fun s -> !(s.tipp2)) (List.filter (fun s -> !(s.tipp1) = tipp) servad);;

(* funktsioon, mis leiab tippude hiliseimate algusaegade hulgast suurima *)
let leiaSuurimVL(tipud) = 
	let lopuajad = List.map (fun t -> Hashtbl.find vl t.nimi) tipud in
	List.fold_left (fun a b -> if a < b then b else a) (List.hd lopuajad) (List.tl lopuajad);;

(* funktsioon, mis leiab tippude varaseimate l�puaegade hulgast v�hima *)
let leiaV2himHA(tipud) =
	let algusajad = List.map (fun t -> Hashtbl.find ha t.nimi) tipud in
	List.fold_left (fun a b -> if a < b then a else b) (List.hd algusajad) (List.tl algusajad);;

(* kui tipu sisendaste oli 0, siis tema varaseimaks algusajaks 0, else max (temasse sisenevad) + hind. *)
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

let kriitilineTipp(tipp) =
	match tipp.hind with
		| None -> failwith("Tipul pole hinda.")
		| Some h -> if Hashtbl.find vl tipp.nimi = Hashtbl.find ha tipp.nimi + h then tipp.tv := Valitud;;

let uuendaV2ljundastet nr tipp = (*analoogiline fn-ga Dijkstra.lisaKaugus, kokku v�tta?*)
	Hashtbl.replace v2ljundastmed tipp.nimi nr;;

let algus() =
	tekst := "Teostame eeldusgraafi anal��si: leiame tippude hiliseima algusaja, varaseima l�puaja ja kriitilise tee.";
	i := Topo;;

let topo(tipud, servad) =
	tekst := "Leiame graafi topoloogilise j�rjestuse.";
	i := Algus;
	while !algoL2bi = false
		do
			TopoKahn.topoKahn(tipud, servad)
		done;
	algoL2bi := false;
	tekst := TopoKahn.string_of_topo();
	i := Vahe1;;

let vahe1(tipud, servad) =
	tekst := "Hakkame tippe topoloogilises j�rjestuses l�bima.";
	List.iter (fun t -> t.tv := Vaatlemata) tipud;
	List.iter (fun s -> s.sv := Vaatlemata) servad;
	List.iter (fun t -> TopoKahn.uuendaSisendastet (TopoKahn.leiaSisendaste(t, servad)) t) tipud;
	(* reset'ime sisendastmed, sest TopoKahni k�igus muutsime neid *)
	List.iter (fun t -> uuendaV2ljundastet (leiaV2ljundaste(t, servad)) t) tipud;
	(* TODO: siin TopoKahn.uuendaSisendastet. Tegeleme siiski v�ljundastmetega. Teha Hashtbl.replace jaoks eri fn *)
	i := TipuValikVL;;

let tipuValikVL(tipud, servad) =
	tekst := "Valime topoloogilises j�rjestuses j�rgmise vaatlemata tipu.";
	let valitudTipp = List.find (fun t -> !(t.tv) = Vaatlemata) !(TopoKahn.tekkinudJ2rjestus) in
	valitudTipp.tv := Valitud;
	List.iter (fun s -> if !(s.tipp2) = valitudTipp then s.sv := Valitud) servad;
	lisaVaraseimLopuaeg(valitudTipp, servad);
	i := TipuLisamineVL;;

let tipuLisamineVL(tipud, servad) = 
	tekst := "M��rame talle varaseima l�petamisaja.";
	List.iter (fun t -> if !(t.tv) = Valitud then t.tv := Vaadeldud) tipud;
	List.iter (fun s -> if !(s.sv) = Valitud then s.sv := Vaadeldud) servad;
	tekst := "VL: " ^ String.concat ", " (List.map (fun t -> t.nimi ^ ": " ^ if Hashtbl.mem vl t.nimi then string_of_int(Hashtbl.find vl t.nimi) else "-") tipud);
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud
		then i := Vahe2
	else i := TipuValikVL;;

let vahe2(tipud, servad) =
	tekst := "Hakkame tippe topoloogilises j�rjestuses tagurpidi l�bima.";
	List.iter (fun t -> t.tv := Vaatlemata) tipud;
	List.iter (fun s -> s.sv := Vaatlemata) servad;
	i := TipuValikHA;;

let tipuValikHA(tipud, servad) =
	tekst := "Valime tagurpidi topoloogilises j�rjestuses j�rgmise vaatlemata tipu.";
	let projektiAeg = leiaSuurimVL(tipud) in (* projekti varaseim l�puaeg *) (* TODO: et ei peaks seda pidevalt arvutama *)
	let valitudTipp = List.find (fun t -> !(t.tv) = Vaatlemata) (List.rev !(TopoKahn.tekkinudJ2rjestus)) in
	valitudTipp.tv := Valitud;
	List.iter (fun s -> if !(s.tipp1) = valitudTipp then s.sv := Valitud) servad;
	lisaHiliseimAlgusaeg(valitudTipp, servad, projektiAeg);
	i := TipuLisamineHA;;

let tipuLisamineHA(tipud, servad) =
	tekst := "M��rame talle hiliseima algusaja.";
	List.iter (fun t -> if !(t.tv) = Valitud then t.tv := Vaadeldud) tipud;
	List.iter (fun s -> if !(s.sv) = Valitud then s.sv := Vaadeldud) servad;
	tekst := "HA: " ^ String.concat ", " (List.map (fun t -> t.nimi ^ ": " ^ if Hashtbl.mem ha t.nimi then string_of_int(Hashtbl.find ha t.nimi) else "-") tipud);
	(* TODO: eelnev VL-ga kokku v�tta *)
	if List.for_all (fun t -> !(t.tv) = Vaadeldud) tipud
		then i := Vahe3
	else i := TipuValikHA;;

let vahe3(tipud, servad) =
	tekst := "Leiame kriitilise tee.";
	List.iter kriitilineTipp tipud;
	List.iter (fun s -> if !(!(s.tipp1).tv) = Valitud && !(!(s.tipp2).tv) = Valitud then s.sv := Valitud) servad;
	i := Lopp;;

let lopp() =
	AlgoBaas.lopp("Eeldusgraafi anal��s on l�ppenud.");
	algoL2bi := false;;

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
		| Lopp -> lopp()
		| _ -> ();;