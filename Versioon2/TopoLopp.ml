open Struktuurid;;
open AlgoBaas;;

(* TODO: TopoKahn.string_of_topo ära kasutada *)
(* TODO: string_of_... funktsioonid *)

let algus() =
	tekst := "Algoritm opoloogilise järjestuse leidmiseks lõppjärjestuse kaudu alustab.";
	tekst := !tekst ^ "\n" ^ "Läbime graafi sügavuti lõppjärjestusega.";
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
	nk1 := "Lõppjärjestus: " ^ string_of_tipud(!(SygavutiLopp.toodeldudTipud));
	nk2 := "";
	algoL2bi := false;
	i := Tagurpidi;;

let tagurpidi() =
	tekst := "Topoloogilise järjestuse leidmiseks keerame tekkinud järjestuse tagurpidi.";
	nk1 := "Lõppärjestus: " ^ string_of_tipud(!(SygavutiLopp.toodeldudTipud));
	nk2 := "Tagurpidi lõppjärjestus: " ^ string_of_tipud(List.rev !(SygavutiLopp.toodeldudTipud));
	i := Lopp;;

let lopp() =
	tekst := "Algoritm lõpetab, olles leidnud topoloogilise järjestuse.";
	nk1 := "Topoloogiline järjestus: " ^ string_of_tipud(List.rev !(SygavutiLopp.toodeldudTipud));
	nk2 := "";
	AlgoBaas.lopp();;

let topoLopp(algtipp, tipud, servad) = 
	match !i with
		| Algus -> algus();
		| ServaVaatlus -> servaVaatlus(algtipp, tipud, servad);
		| Tagurpidi -> tagurpidi();
		| Lopp -> lopp();
		| _ -> ();;

