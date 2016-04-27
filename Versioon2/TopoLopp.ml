open Struktuurid;;
open AlgoBaas;;

(* TODO: TopoKahn.string_of_topo ära kasutada *)
(* TODO: string_of_... funktsioonid *)

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
	tekst := "Tipud läbiti järgnevas järjekorras: [" ^ string_of_tipud(!(SygavutiLopp.toodeldudTipud)) ^ "]";
	algoL2bi := false;
	i := Tagurpidi;;

let tagurpidi() =
	tekst := "Topoloogilise järjestuse leidmiseks keerame tekkinud järjestuse tagurpidi.";
	tekst := !tekst ^ "\n" ^ "Järjestus: [" ^ string_of_tipud(!(SygavutiLopp.toodeldudTipud)) ^ "]";
	tekst := !tekst ^ "\n" ^ "Tagurpidi järjestus: [" ^ string_of_tipud(List.rev !(SygavutiLopp.toodeldudTipud)) ^ "]";
	i := Lopp;;

let lopp() =
	tekst := "Algoritm lõpetab, olles leidnud topoloogilise järjestuse.";
	tekst := !tekst ^ "\n[" ^ string_of_tipud(List.rev !(SygavutiLopp.toodeldudTipud)) ^ "]";
	AlgoBaas.lopp();;

let topoLopp(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus();
		| ServaVaatlus -> servaVaatlus(algtipp, tipud, servad);
		| Tagurpidi -> tagurpidi();
		| Lopp -> lopp();
		| _ -> ();;

