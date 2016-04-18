open Struktuurid;;
open AlgoBaas;;

(* TODO: TopoKahn.string_of_topo ära kasutada *)

let algus() =
	tekst := "Algoritm opoloogilise järjestuse leidmiseks lõppjärjestuse kaudu alustab.";
	tekst := "Läbime graafi sügavuti lõppjärjestusega.";
	i := ServaVaatlus;;

(* TODO: rename. Algosamme oleks juurde vaja. *)
(* TODO: või peaks kõik sügavuti läbimise sammud ka kujutama? Kas või ainult tippude lisamist? *)
let servaVaatlus(algtipp, tipud, servad) =
	i := Algus; (* sügavuti jaoks *)
	while !algoL2bi = false
		do
			SygavutiLopp.sygavutiLopp(algtipp, tipud, servad)
		done;
	List.iter (fun s -> s.sv := Vaadeldud) servad; (* kõik vaadelduks, et graafil kajastuks *)
	tekst := "Tipud läbiti järgnevas järjekorras: " ^ string_of_tipud(!(SygavutiLopp.kylastatudTipud));
	i := ServaValik;;

(* TODO: rename *)
let servaValik() =
	tekst := "Topoloogilise järjestuse leidmiseks keerame tekkinud järjestuse tagurpidi.";
	i := Lopp;;

let lopp() =
	(*AlgoBaas.lopp("Algoritm lõpetab, olles leidnud topoloogilise järjestuse.");;*)
	AlgoBaas.lopp(string_of_tipud(List.rev !(SygavutiLopp.kylastatudTipud)));;

let topoLopp(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus();
		| ServaVaatlus -> servaVaatlus(algtipp, tipud, servad);
		| ServaValik -> servaValik();
		| Lopp -> lopp();
		| _ -> ();;

