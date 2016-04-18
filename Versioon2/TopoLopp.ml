open Struktuurid;;
open AlgoBaas;;

(* TODO: TopoKahn.string_of_topo �ra kasutada *)

let algus() =
	tekst := "Algoritm opoloogilise j�rjestuse leidmiseks l�ppj�rjestuse kaudu alustab.";
	tekst := "L�bime graafi s�gavuti l�ppj�rjestusega.";
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
	tekst := "Tipud l�biti j�rgnevas j�rjekorras: " ^ string_of_tipud(!(SygavutiLopp.kylastatudTipud));
	i := ServaValik;;

(* TODO: rename *)
let servaValik() =
	tekst := "Topoloogilise j�rjestuse leidmiseks keerame tekkinud j�rjestuse tagurpidi.";
	i := Lopp;;

let lopp() =
	(*AlgoBaas.lopp("Algoritm l�petab, olles leidnud topoloogilise j�rjestuse.");;*)
	AlgoBaas.lopp(string_of_tipud(List.rev !(SygavutiLopp.kylastatudTipud)));;

let topoLopp(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus();
		| ServaVaatlus -> servaVaatlus(algtipp, tipud, servad);
		| ServaValik -> servaValik();
		| Lopp -> lopp();
		| _ -> ();;

