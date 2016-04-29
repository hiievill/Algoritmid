open Struktuurid;;
open Graafika;;


let kaksTippuSuunatudKaaludegatipud = looTipud([
		("A", 100, 150, None);
		("B", 350, 250, None);
	]);;
let kaksTippuSuunatudKaaludegaservad = looServad([
		("A", "B", Some(8), true);
	], kaksTippuSuunatudKaaludegatipud);;

let mitteSidusKaaludetaSuunadetatipud = looTipud([
		("A", 100, 100, None);
		("B", 200, 200, None);
		("C", 300, 200, None);
		("D", 400, 300, None);
	]);;
let mitteSidusKaaludetaSuunadetaservad = looServad([
		("A", "B", None, true);
		("C", "D", None, true);
	], mitteSidusKaaludetaSuunadetatipud);;

let mitteTugevaltSidustipud = looTipud([
		("A", 100, 100, None);
		("B", 200, 200, None);
		("C", 300, 200, None);
		("D", 400, 300, None);
	]);;
let mitteTugevaltSidusservad = looServad([
		("A", "B", None, true);
		("C", "D", None, true);
		("C", "B", None, true);
	], mitteTugevaltSidustipud);;

let sidusKaaludetaSuunadetatipud = looTipud([
  	("A", 300, 400, None);
  	("B", 200, 300, None);
  	("C", 200, 200, None);
  	("D", 400, 300, None);
  	("E", 350, 200, None);
		("F", 450, 200, None);
		("G", 150, 100, None);
		("H", 200, 100, None);
		("I", 250, 100, None);
  ]);;
let sidusKaaludetaSuunadetaservad = looServad([
  	("A", "B", None, false);
  	("B", "C", None, false);
  	("A", "D", None, false);
  	("D", "E", None, false);
  	("D", "F", None, false);
		("C", "G", None, false);
		("C", "H", None, false);
		("C", "I", None, false);
  ], sidusKaaludetaSuunadetatipud);;

let sidusKaaludegaSuunadetatipud2 = looTipud([
  	("A", 300, 400, None);
  	("B", 200, 300, None);
  	("C", 200, 200, None);
  	("D", 400, 300, None);
  	("E", 350, 200, None);
		("F", 450, 200, None);
		("G", 150, 100, None);
		("H", 200, 100, None);
		("I", 250, 100, None);
  ]);;
let sidusKaaludegaSuunadetaservad2 = looServad([
  	("A", "B", Some(15), false);
  	("B", "C", Some(4), false);
  	("A", "D", Some(8), false);
  	("D", "E", Some(2), false);
  	("D", "F", Some(6), false);
		("C", "G", Some(9), false);
		("C", "H", Some(3), false);
		("C", "I", Some(9), false);
  ], sidusKaaludegaSuunadetatipud2);;

let sidusKaaludetaSuunadeta2tipud = looTipud([
  	("A", 100, 300, None);
  	("B", 300, 300, None);
  	("C", 100, 100, None);
  	("D", 300, 100, None);
  	("E", 500, 200, None);
  ]);;
let sidusKaaludetaSuunadeta2servad = looServad([
  	("A", "B", None, false);
  	("B", "D", None, false);
  	("B", "E", None, false);
  	("C", "D", None, false);
  	("A", "C", None, false);
  	("D", "E", None, false);
  ], sidusKaaludetaSuunadeta2tipud);;

let sidusKaaludetaSuunadegatipud = looTipud([
  	("A", 100, 300, None);
  	("B", 300, 300, None);
  	("C", 100, 100, None);
  	("D", 300, 100, None);
  	("E", 500, 200, None);
  ]);;
let sidusKaaludetaSuunadegaservad = looServad([
  	("A", "B", None, true);
  	("B", "D", None, true);
  	("B", "E", None, true);
  	("D", "C", None, true);
  ], sidusKaaludetaSuunadegatipud);;

let sidusKaaludegaSuunadetatipud = looTipud([
  	("A", 100, 300, None);
  	("B", 300, 300, None);
  	("C", 100, 100, None);
  	("D", 300, 100, None);
  	("E", 500, 200, None);
  ]);;
let sidusKaaludegaSuunadetaservad = looServad([
  	("A", "B", Some(1), false);
  	("B", "D", Some(3), false);
  	("B", "E", Some(5), false);
  	("C", "D", Some(2), false);
  	("A", "C", Some(4), false);
  	("D", "E", Some(6), false);
  ], sidusKaaludegaSuunadetatipud);;

let sidusKaaludetaSuunadega2tipud = looTipud([
  	("A", 100, 300, None);
  	("B", 300, 300, None);
  	("C", 100, 100, None);
  	("D", 300, 100, None);
  	("E", 500, 200, None);
  ]);;
let sidusKaaludetaSuunadega2servad = looServad([
  	("A", "B", None, true);
  	("B", "D", None, true);
  	("B", "E", None, true);
  	("C", "D", None, true);
  	("A", "C", None, true);
  	("D", "E", None, true);
  ], sidusKaaludetaSuunadega2tipud);;

let sidusKaaludegaSuunadegatipud = looTipud([
		("A", 200, 200, None);
		("B", 250, 40, None);
		("C", 30, 300, None);
		("D", 540, 410, None);
		("E", 10, 400, None);
		("F", 400, 70, None);
	]);;
let sidusKaaludegaSuunadegaservad = looServad([
		("A", "B", Some(6), true);
		("B", "C", Some(5), true);
		("C", "D", Some(8), true);
		("A", "C", Some(3), true);
		("A", "F", Some(8), true);
		("D", "F", Some(1), true);
		("D", "E", Some(9), true);
	], sidusKaaludegaSuunadegatipud);;

let sidusKaaludegaSuunadega2tipud = looTipud([
		("A", 100, 300, None);
  	("B", 300, 300, None);
  	("C", 100, 100, None);
  	("D", 300, 100, None);
  	("E", 500, 200, None);
	]);;
let sidusKaaludegaSuunadega2servad = looServad([
		("A", "B", Some(1), true);
  	("B", "D", Some(2), true);
  	("B", "E", Some(3), true);
  	("C", "D", Some(4), true);
  	("A", "C", Some(5), true);
  	("D", "E", Some(6), true);
	], sidusKaaludegaSuunadega2tipud);;

let sidusNegKaaludegaSuunadegatipud = looTipud([
		("A", 100, 300, None);
		("B", 300, 300, None);
		("C", 100, 100, None);
		("D", 300, 100, None);
	]);;
let sidusNegKaaludegaSuunadegaservad = looServad([
		("A", "C", Some(3), true);
  	("B", "D", Some(1), true);
  	("D", "B", Some(4), true);
  	("C", "D", Some(5), true);
  	("B", "A", Some(-2), true);
  	("A", "D", Some(0), true);
	], sidusNegKaaludegaSuunadegatipud);;

let sidusNegKaaludegaSuunadega2tipud = looTipud([
		("A", 100, 300, None);
		("B", 300, 300, None);
		("C", 100, 100, None);
		("D", 300, 100, None);
		("E", 200, 200, None);
	]);;
let sidusNegKaaludegaSuunadega2servad = looServad([
		("A", "B", Some(1), true);
  	("B", "D", Some(2), true);
  	("D", "C", Some(-2), true);
  	("D", "E", Some(5), true);
  	("C", "E", Some(3), true);
  	("C", "A", Some(4), true);
		("A", "E", Some(9), true);
	], sidusNegKaaludegaSuunadega2tipud);;

let sidusKaaludetaSuunadegaHindadegatipud = looTipud([
		("A", 100, 400, Some(8));
		("B", 200, 400, Some(2));
		("C", 300, 400, Some(10));
		("D", 400, 400, Some(5));
		("E", 100, 200, Some(7));
		("F", 200, 200, Some(17));
		("G", 300, 200, Some(1));
	]);;
let sidusKaaludetaSuunadegaHindadegaservad = looServad([
		("A", "B", None, true);
		("B", "C", None, true);
		("C", "D", None, true);
		("C", "G", None, true);
		("B", "F", None, true);
		("F", "G", None, true);
		("F", "E", None, true);
	], sidusKaaludetaSuunadegaHindadegatipud);;

let sidusKaaludetaSuunadega3tipud = looTipud([
		("A", 100, 400, None);
		("B", 200, 400, None);
		("C", 300, 400, None);
		("D", 400, 400, None);
		("E", 100, 200, None);
		("F", 200, 200, None);
		("G", 300, 200, None);
		("H", 400, 200, None);
	]);;

let sidusKaaludetaSuunadega3servad = looServad([
		("A", "B", None, true);
		("B", "C", None, true);
		("C", "D", None, true);
		("C", "G", None, true);
		("G", "C", None, true);
		("B", "F", None, true);
		("F", "G", None, true);
		("F", "E", None, true);
		("E", "A", None, true);
		("H", "G", None, true);
	], sidusKaaludetaSuunadega3tipud);;


let sidusKaaludetaSuunadeta = {								(* ts�kliteta *)
	tipud = sidusKaaludetaSuunadetatipud;
	servad = sidusKaaludetaSuunadetaservad;
};;

let sidusKaaludetaSuunadeta2 = {								(* ts�klitega *)
	tipud = sidusKaaludetaSuunadeta2tipud;
	servad = sidusKaaludetaSuunadeta2servad;
};;

let sidusKaaludetaSuunadega = {								(* ts�kliteta *)
	tipud = sidusKaaludetaSuunadegatipud;
	servad = sidusKaaludetaSuunadegaservad;
};;

let sidusKaaludetaSuunadega2 = {								(* ts�klitega *)
	tipud = sidusKaaludetaSuunadega2tipud;
	servad = sidusKaaludetaSuunadega2servad;
};;

let sidusKaaludetaSuunadega3 = {								(* ts�klitega *)		(* mitte tugevalt sidus - Kosaraju jaoks *)
	tipud = sidusKaaludetaSuunadega3tipud;
	servad = sidusKaaludetaSuunadega3servad;
};;

let sidusKaaludegaSuunadega = {								(* ts�klitega *)
	tipud = sidusKaaludegaSuunadegatipud;
	servad = sidusKaaludegaSuunadegaservad;
};;

let sidusNegKaaludegaSuunadega = {
	tipud = sidusNegKaaludegaSuunadegatipud;
	servad = sidusNegKaaludegaSuunadegaservad;
};;

let sidusNegKaaludegaSuunadega2 = {
	tipud = sidusNegKaaludegaSuunadega2tipud;
	servad = sidusNegKaaludegaSuunadega2servad;
};;

let sidusKaaludegaSuunadega2 = {								(* ts�klitega *)
	tipud = sidusKaaludegaSuunadega2tipud;
	servad = sidusKaaludegaSuunadega2servad;
};;

let sidusKaaludetaSuunadegaHindadega = {
	tipud = sidusKaaludetaSuunadegaHindadegatipud;
	servad = sidusKaaludetaSuunadegaHindadegaservad;
};;

let sidusKaaludetaSuunadega3 = {
	tipud = sidusKaaludetaSuunadega3tipud;
	servad = sidusKaaludetaSuunadega3servad;
};;

(* �hest tipust koosnev graaf *)
let yksTipp = {
	tipud = looTipud([("A", 300, 300, None)]);
	servad = [];
};;

(* �hest tipust koosnev graaf, mille tipul on hind *)
let yksTippHinnaga = {
	tipud = looTipud([("A", 300, 300, Some(10))]);
	servad = [];
};;

(* kahest tipust koosnev suunadeta ja kaaludeta graaf *)

(* kahest tipust koosnev suunadeta kaaludega graaf *)

(* kahest tipust koosnev suunadega kaaludeta graaf *)

(* kahest tipust koosnev suunatud ja kaaluga graaf *)
let kaksTippuSuunatudKaaluga = {
	tipud = kaksTippuSuunatudKaaludegatipud;
	servad = kaksTippuSuunatudKaaludegaservad;
};;

(* mittesidus kaaludeta ja suunadeta graaf *)
let mitteSidusKaaludetaSuunadeta = {
	tipud = mitteSidusKaaludetaSuunadetatipud;
	servad = mitteSidusKaaludetaSuunadetaservad;
};;

(*sidus, aga mitte tugevalt sidus kaaludeta ja suunadeta graaf *)
let mitteTugevaltSidus = {
	tipud = mitteTugevaltSidustipud;
	servad = mitteTugevaltSidusservad;
};;

(* sidus kaaludeta suunatud graaf *)

(* mittesidus kaaludeta suunatud graaf *)

(* sidus kaaludeta suunadeta graaf *)

(* mittesidus kaaludeta suunadeta graaf *)



let sidusKaaludegaSuunadeta = {
	tipud = sidusKaaludegaSuunadetatipud;
	servad = sidusKaaludegaSuunadetaservad;
};;

let sidusKaaludegaSuunadeta2 = {
	tipud = sidusKaaludegaSuunadetatipud2;
	servad = sidusKaaludegaSuunadetaservad2;
};;


let ntLaiuti1 = sidusKaaludetaSuunadeta;;			(* ts�kliteta *)
let ntLaiuti2 = sidusKaaludetaSuunadeta2;;		(* ts�klitega *)
let ntLaiuti3 = sidusKaaludetaSuunadega;;			(* ts�kliteta *)
let ntLaiuti4 = sidusKaaludetaSuunadega2;;		(* ts�klitega *)

let ntSygavutiEes1 = sidusKaaludetaSuunadeta;;			(* ts�kliteta *)
let ntSygavutiEes2 = sidusKaaludetaSuunadeta2;;			(* ts�klitega *)
let ntSygavutiEes3 = sidusKaaludetaSuunadega;;			(* ts�kliteta *)
let ntSygavutiEes4 = sidusKaaludetaSuunadega2;;			(* ts�klitega *)

let ntSygavutiLopp1 = sidusKaaludetaSuunadeta;;			(* ts�kliteta *)
let ntSygavutiLopp2 = sidusKaaludetaSuunadeta2;;		(* ts�klitega *)
let ntSygavutiLopp3 = sidusKaaludetaSuunadega;;			(* ts�kliteta *)
let ntSygavutiLopp4 = sidusKaaludetaSuunadega2;;		(* ts�klitega *)

let ntPrim1 = sidusKaaludegaSuunadeta;;				(* ts�klitega *)
let ntPrim2 = sidusKaaludegaSuunadeta2;;			(*ts�kliteta *)

let ntKruskal1 = sidusKaaludegaSuunadeta;;		(* ts�klitega *)
let ntKruskal2 = sidusKaaludegaSuunadeta2;;			(*ts�kliteta *)

let ntDijkstra1 = sidusKaaludegaSuunadega;; 	(* ts�klitega *)
let ntDijkstra2 = sidusKaaludegaSuunadega2;; 	(* ts�klitega *)
(* TODO: ts�kliteta *)

let ntFloydWarshall1 = sidusNegKaaludegaSuunadega2;;(* ts�klitega, negatiivsete kaaludega *)
let ntFloydWarshall2 = sidusNegKaaludegaSuunadega;; (* ts�klitega, negatiivsete kaaludega *)
let ntFloydWarshall3 = sidusKaaludegaSuunadega;; (* ts�klitega *)

let ntTopoLopp1 = sidusKaaludetaSuunadega;;		(* ts�kliteta *)
let ntTopoLopp2 = sidusKaaludetaSuunadega2;;		(* ts�klitega *)

let ntTopoKahn1 = sidusKaaludetaSuunadega;;		(* ts�kliteta *)
let ntTopoKahn2 = sidusKaaludetaSuunadega2;;		(* ts�klitega *)
	
let ntEeldusgraaf1 = sidusKaaludetaSuunadegaHindadega;;	(* ts�kliteta *)

let ntKosaraju1 = sidusKaaludetaSuunadega3;;		(* ts�klitega *)		(* mitte tugevalt sidus *)

