open Struktuurid;;
open AlgoBaas;;

(* TODO: TopoKahn.string_of_topo �ra kasutada *)
(* TODO: string_of_... funktsioonid *)

let algus() =
	tekst := "Algoritm opoloogilise j�rjestuse leidmiseks l�ppj�rjestuse kaudu alustab.";
	tekst := !tekst ^ "\n" ^ "L�bime graafi s�gavuti l�ppj�rjestusega.";
	i := ServaVaatlus;;

(* TODO: rename. Algosamme oleks juurde vaja. *)
(* TODO: v�i peaks k�ik s�gavuti l�bimise sammud ka kujutama? Kas v�i ainult tippude lisamist? *)
let servaVaatlus(algtipp, tipud, servad) =
	i := Algus; (* s�gavuti jaoks *)
	while !algoL2bi = false
		do
			SygavutiLopp.sygavutiLopp(algtipp, tipud, servad)
		done;
	List.iter (fun s -> s.sv := Vaadeldud) servad; (* k�ik vaadelduks, et graafil kajastuks *)
	nk1 := "L�ppj�rjestus: " ^ string_of_tipud(!(SygavutiLopp.toodeldudTipud));
	nk2 := "";
	algoL2bi := false;
	i := Tagurpidi;;

let tagurpidi() =
	tekst := "Topoloogilise j�rjestuse leidmiseks keerame tekkinud j�rjestuse tagurpidi.";
	nk1 := "L�pp�rjestus: " ^ string_of_tipud(!(SygavutiLopp.toodeldudTipud));
	nk2 := "Tagurpidi l�ppj�rjestus: " ^ string_of_tipud(List.rev !(SygavutiLopp.toodeldudTipud));
	i := Lopp;;

let lopp() =
	tekst := "Algoritm l�petab, olles leidnud topoloogilise j�rjestuse.";
	nk1 := "Topoloogiline j�rjestus: " ^ string_of_tipud(List.rev !(SygavutiLopp.toodeldudTipud));
	nk2 := "";
	AlgoBaas.lopp();;

let topoLopp(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus();
		| ServaVaatlus -> servaVaatlus(algtipp, tipud, servad);
		| Tagurpidi -> tagurpidi();
		| Lopp -> lopp();
		| _ -> ();;

