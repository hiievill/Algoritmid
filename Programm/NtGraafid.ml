(* moodul NtGraafid sisaldab iga algoritmi jaoks nelja näitegraafi loomist *)

open Struktuurid;;
open Graafika;;

let kaksTippuSuunatudKaaludegatipud = looTipud([
		("A", 100, 150, None);
		("B", 350, 250, None);
	]);;
let kaksTippuSuunatudKaaludegaservad = looServad([
		("A", "B", Some(8), true);
	], kaksTippuSuunatudKaaludegatipud);;

let mitteSidusKaaludegaSuundadegatipud = looTipud([
		("A", 100, 100, None);
		("B", 200, 200, None);
		("C", 300, 200, None);
		("D", 400, 300, None);
		("E", 200, 300, None);
	]);;
let mitteSidusKaaludegaSuundadegaservad = looServad([
		("A", "B", Some(7), true);
		("C", "D", Some(5), true);
		("D", "E", Some(8), true);
	], mitteSidusKaaludegaSuundadegatipud);;

let mitteSidusKaaludetaSuundadegatipud = looTipud([
		("A", 100, 100, None);
		("B", 200, 200, None);
		("C", 300, 200, None);
		("D", 400, 300, None);
		("E", 200, 300, None);
	]);;
let mitteSidusKaaludetaSuundadegaservad = looServad([
		("A", "B", None, true);
		("C", "D", None, true);
		("D", "E", None, true);
	], mitteSidusKaaludetaSuundadegatipud);;

let mitteSidusKaaludetaSuundadega3tipud = looTipud([
		("A", 100, 100, None);
		("B", 200, 400, None);
		("C", 300, 200, None);
		("D", 400, 300, None);
		("E", 200, 300, None);
		("F", 400, 100, None);
		("G", 400, 400, None);
	]);;
let mitteSidusKaaludetaSuundadega3servad = looServad([
		("A", "B", None, true);
		("B", "A", None, true);
		("A", "F", None, true);
		("C", "D", None, true);
		("D", "E", None, true);
		("E", "C", None, true);
		("G", "E", None, true);
	], mitteSidusKaaludetaSuundadega3tipud);;

let mitteSidusKaaludetaSuundadega2tipud = looTipud([
		("A", 270, 100, None);
		("B", 190, 250, None);
		("C", 15, 350, None);
		("D", 400, 400, None);
		("E", 380, 150, None);
	]);;
let mitteSidusKaaludetaSuundadega2servad = looServad([
		("A", "B", None, true);
		("B", "C", None, true);
		("C", "D", None, true);
		("B", "D", None, true);
	], mitteSidusKaaludetaSuundadega2tipud);;

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

let sidusKaaludetaSuundadetatipud = looTipud([
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
let sidusKaaludetaSuundadetaservad = looServad([
  	("A", "B", None, false);
  	("B", "C", None, false);
  	("A", "D", None, false);
  	("D", "E", None, false);
  	("D", "F", None, false);
		("C", "G", None, false);
		("C", "H", None, false);
		("C", "I", None, false);
  ], sidusKaaludetaSuundadetatipud);;

let sidusKaaludegaSuundadetatipud2 = looTipud([
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
let sidusKaaludegaSuundadetaservad2 = looServad([
  	("A", "B", Some(15), false);
  	("B", "C", Some(4), false);
  	("A", "D", Some(8), false);
  	("D", "E", Some(2), false);
  	("D", "F", Some(6), false);
		("C", "G", Some(9), false);
		("C", "H", Some(3), false);
		("C", "I", Some(9), false);
  ], sidusKaaludegaSuundadetatipud2);;

let sidusKaaludegaSuundadetatipud3 = looTipud([
  	("A", 100, 400, None);
  	("B", 300, 200, None);
  	("C", 300, 400, None);
  	("D", 100, 150, None);
  	("E", 400, 150, None);
		("F", 200, 400, None);
		("G", 400, 400, None);
		("H", 300, 300, None);
  ]);;
let sidusKaaludegaSuundadetaservad3 = looServad([
  	("A", "F", Some(2), false);
		("F", "C", Some(14), false);
		("C", "G", Some(5), false);
		("F", "H", Some(17), false);
		("A", "H", Some(33), false);
		("H", "B", Some(56), false);
		("B", "E", Some(20), false);
		("A", "D", Some(4), false);
		("D", "E", Some(6), false);
  ], sidusKaaludegaSuundadetatipud3);;

let sidusKaaludegaSuundadetatipud4 = looTipud([
  	("A", 100, 350, None);
  	("B", 250, 430, None);
  	("C", 400, 350, None);
  	("D", 400, 150, None);
  	("E", 250, 70, None);
		("F", 100, 150, None);
		("O", 250, 250, None);
  ]);;
let sidusKaaludegaSuundadetaservad4 = looServad([
  	("A", "B", Some(6), false);
  	("B", "C", Some(2), false);
  	("C", "D", Some(11), false);
  	("D", "E", Some(9), false);
  	("E", "F", Some(6), false);
		("F", "A", Some(9), false);
		("A", "O", Some(3), false);
		("B", "O", Some(25), false);
		("C", "O", Some(4), false);
		("D", "O", Some(2), false);
		("E", "O", Some(13), false);
		("F", "O", Some(19), false);
  ], sidusKaaludegaSuundadetatipud4);;

let sidusKaaludetaSuundadeta2tipud = looTipud([
  	("A", 100, 300, None);
  	("B", 300, 300, None);
  	("C", 100, 100, None);
  	("D", 300, 100, None);
  	("E", 500, 200, None);
  ]);;
let sidusKaaludetaSuundadeta2servad = looServad([
  	("A", "B", None, false);
  	("B", "D", None, false);
  	("B", "E", None, false);
  	("C", "D", None, false);
  	("A", "C", None, false);
  	("D", "E", None, false);
  ], sidusKaaludetaSuundadeta2tipud);;

let sidusKaaludetaSuundadegatipud = looTipud([
  	("A", 100, 300, None);
  	("B", 300, 300, None);
  	("C", 100, 100, None);
  	("D", 300, 100, None);
  	("E", 500, 200, None);
  ]);;
let sidusKaaludetaSuundadegaservad = looServad([
  	("A", "B", None, true);
  	("B", "D", None, true);
  	("B", "E", None, true);
  	("D", "C", None, true);
  ], sidusKaaludetaSuundadegatipud);;

let sidusKaaludegaSuundadetatipud = looTipud([
  	("A", 100, 300, None);
  	("B", 300, 300, None);
  	("C", 100, 100, None);
  	("D", 300, 100, None);
  	("E", 500, 200, None);
  ]);;
let sidusKaaludegaSuundadetaservad = looServad([
  	("A", "B", Some(1), false);
  	("B", "D", Some(3), false);
  	("B", "E", Some(5), false);
  	("C", "D", Some(2), false);
  	("A", "C", Some(4), false);
  	("D", "E", Some(6), false);
  ], sidusKaaludegaSuundadetatipud);;

let mittesidusKaaludegaSuundadetatipud = looTipud([
  	("A", 100, 100, None);
		("B", 200, 200, None);
		("C", 300, 200, None);
		("D", 400, 300, None);
		("E", 200, 400, None);
  ]);;
let mittesidusKaaludegaSuundadetaservad = looServad([
  	("A", "B", Some(7), false);
		("C", "D", Some(5), false);
		("D", "E", Some(8), false);
  ], mittesidusKaaludegaSuundadetatipud);;

let sidusKaaludetaSuundadega2tipud = looTipud([
  	("A", 100, 300, None);
  	("B", 300, 300, None);
  	("C", 100, 100, None);
  	("D", 300, 100, None);
  	("E", 500, 200, None);
  ]);;
let sidusKaaludetaSuundadega2servad = looServad([
  	("A", "B", None, true);
  	("B", "D", None, true);
  	("B", "E", None, true);
  	("C", "D", None, true);
  	("A", "C", None, true);
  	("D", "E", None, true);
  ], sidusKaaludetaSuundadega2tipud);;

let sidusKaaludegaSuundadegatipud = looTipud([
		("A", 200, 200, None);
		("B", 250, 40, None);
		("C", 30, 300, None);
		("D", 540, 410, None);
		("E", 10, 400, None);
		("F", 400, 70, None);
	]);;
let sidusKaaludegaSuundadegaservad = looServad([
		("A", "B", Some(6), true);
		("B", "C", Some(5), true);
		("C", "D", Some(8), true);
		("A", "C", Some(3), true);
		("A", "F", Some(8), true);
		("D", "F", Some(1), true);
		("D", "E", Some(9), true);
		("E", "D", Some(3), true);
	], sidusKaaludegaSuundadegatipud);;

let sidusKaaludegaSuundadegatipud3 = looTipud([
		("A", 200, 200, None);
		("B", 7, 40, None);
		("C", 30, 300, None);
		("D", 100, 340, None);
		("E", 10, 400, None);
		("F", 210, 108, None);
		("G", 370, 50, None);
		("H", 400, 70, None);
		("I", 200, 110, None);
		("J", 150, 250, None);
	]);;
let sidusKaaludegaSuundadegaservad3 = looServad([
		("A", "B", Some(1), true);
		("B", "C", Some(5), true);
		("C", "D", Some(13), true);
		("D", "E", Some(2), true);
		("E", "F", Some(8), true);
		("F", "G", Some(44), true);
		("G", "H", Some(17), true);
		("H", "I", Some(3), true);
		("I", "J", Some(8), true);
		("J", "A", Some(3), true);
	], sidusKaaludegaSuundadegatipud3);;

let sidusKaaludegaSuundadega2tipud = looTipud([
		("A", 100, 300, None);
  	("B", 300, 300, None);
  	("C", 100, 100, None);
  	("D", 300, 100, None);
  	("E", 500, 200, None);
	]);;
let sidusKaaludegaSuundadega2servad = looServad([
		("A", "B", Some(1), true);
  	("B", "D", Some(2), true);
  	("B", "E", Some(3), true);
  	("C", "D", Some(4), true);
  	("A", "C", Some(5), true);
  	("D", "E", Some(6), true);
	], sidusKaaludegaSuundadega2tipud);;

let sidusKaaludegaSuundadega4tipud = looTipud([
		("A", 100, 350, None);
  	("B", 250, 430, None);
  	("C", 400, 350, None);
  	("D", 400, 150, None);
  	("E", 250, 70, None);
		("F", 100, 150, None);
		("O", 250, 250, None);
	]);;
let sidusKaaludegaSuundadega4servad = looServad([
		("A", "B", Some(6), true);
  	("B", "C", Some(2), true);
  	("C", "D", Some(11), true);
  	("D", "E", Some(9), true);
  	("E", "F", Some(6), true);
		("F", "A", Some(9), true);
		("A", "O", Some(3), true);
		("B", "O", Some(25), true);
		("C", "O", Some(4), true);
		("O", "D", Some(2), true);
		("O", "E", Some(13), true);
		("O", "F", Some(19), true);
	], sidusKaaludegaSuundadega4tipud);;

let sidusKaaludegaSuundadega5tipud = looTipud([
		("A", 100, 350, None);
  	("B", 250, 430, None);
  	("C", 400, 350, None);
  	("D", 400, 150, None);
  	("E", 250, 70, None);
		("F", 100, 150, None);
		("O", 250, 250, None);
	]);;
let sidusKaaludegaSuundadega5servad = looServad([
		("A", "B", Some(11), true);
  	("B", "C", Some(4), true);
  	("A", "O", Some(6), true);
  	("O", "E", Some(3), true);
		("E", "F", Some(8), true);
		("E", "D", Some(5), true);
	], sidusKaaludegaSuundadega5tipud);;

let sidusNegKaaludegaSuundadega2tipud = looTipud([
		("A", 100, 300, None);
		("B", 300, 300, None);
		("C", 100, 100, None);
		("D", 300, 100, None);
		("E", 200, 200, None);
	]);;
let sidusNegKaaludegaSuundadega2servad = looServad([
		("A", "B", Some(1), true);
  	("B", "D", Some(2), true);
  	("D", "C", Some(-2), true);
  	("D", "E", Some(5), true);
  	("C", "E", Some(3), true);
  	("C", "A", Some(4), true);
		("A", "E", Some(9), true);
	], sidusNegKaaludegaSuundadega2tipud);;

let sidusKaaludetaSuundadegaHindadegatipud = looTipud([
		("A", 100, 400, Some(8));
		("B", 200, 400, Some(2));
		("C", 300, 400, Some(10));
		("D", 400, 400, Some(5));
		("E", 100, 200, Some(7));
		("F", 200, 200, Some(17));
		("G", 300, 200, Some(1));
	]);;
let sidusKaaludetaSuundadegaHindadegaservad = looServad([
		("A", "B", None, true);
		("B", "C", None, true);
		("C", "D", None, true);
		("C", "G", None, true);
		("B", "F", None, true);
		("F", "G", None, true);
		("F", "E", None, true);
	], sidusKaaludetaSuundadegaHindadegatipud);;

let sidusKaaludetaSuundadegaHindadega2tipud = looTipud([
		("A", 100, 400, Some(8));
		("B", 200, 400, Some(2));
		("C", 400, 400, Some(10));
		("D", 300, 250, Some(5));
		("E", 100, 200, Some(7));
		("F", 100, 100, Some(3));
		("G", 400, 100, Some(9));
	]);;
let sidusKaaludetaSuundadegaHindadega2servad = looServad([
		("A", "B", None, true);
		("B", "D", None, true);
		("E", "F", None, true);
		("F", "D", None, true);
		("D", "G", None, true);
		("D", "C", None, true);
	], sidusKaaludetaSuundadegaHindadega2tipud);;

let sidusKaaludetaSuundadegaHindadega3tipud = looTipud([
		("A", 100, 200, Some(5));
		("B", 200, 300, Some(7));
		("C", 200, 200, Some(2));
		("D", 200, 100, Some(3));
		("E", 300, 100, Some(4));
		("F", 400, 200, Some(9));
	]);;
let sidusKaaludetaSuundadegaHindadega3servad = looServad([
		("A", "B", None, true);
		("A", "C", None, true);
		("A", "D", None, true);
		("D", "E", None, true);
		("B", "F", None, true);
		("C", "F", None, true);
		("E", "F", None, true);
	], sidusKaaludetaSuundadegaHindadega3tipud);;

let sidusKaaludetaSuundadegaHindadega4tipud = looTipud([
		("A", 100, 450, Some(5));
		("B", 200, 300, Some(7));
		("C", 200, 200, Some(2));
		("D", 200, 100, Some(3));
		("E", 300, 100, Some(4));
		("F", 400, 200, Some(9));
		("G", 100, 100, Some(7));
		("H", 400, 300, Some(6));
	]);;
let sidusKaaludetaSuundadegaHindadega4servad = looServad([
		("A", "F", None, true);
		("F", "C", None, true);
		("C", "G", None, true);
		("F", "H", None, true);
		("A", "H", None, true);
		("H", "B", None, true);
		("B", "E", None, true);
		("A", "D", None, true);
		("D", "E", None, true);
	], sidusKaaludetaSuundadegaHindadega4tipud);;

let sidusKaaludetaSuundadega3tipud = looTipud([
		("A", 100, 400, None);
		("B", 200, 400, None);
		("C", 300, 400, None);
		("D", 400, 400, None);
		("E", 100, 200, None);
		("F", 200, 200, None);
		("G", 300, 200, None);
		("H", 400, 200, None);
	]);;
let sidusKaaludetaSuundadega3servad = looServad([
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
	], sidusKaaludetaSuundadega3tipud);;

let sidusKaaludetaSuundadega4tipud = looTipud([
		("A", 100, 350, None);
  	("B", 250, 430, None);
  	("C", 400, 350, None);
  	("D", 400, 150, None);
  	("E", 250, 70, None);
		("F", 100, 150, None);
		("O", 250, 250, None);
	]);;
let sidusKaaludetaSuundadega4servad = looServad([
		("A", "B", None, true);
  	("B", "C", None, true);
  	("C", "D", None, true);
  	("D", "E", None, true);
  	("E", "F", None, true);
		("A", "F", None, true);
		("A", "O", None, true);
		("O", "B", None, true);
		("C", "O", None, true);
		("D", "O", None, true);
		("O", "E", None, true);
		("O", "F", None, true);
	], sidusKaaludetaSuundadega4tipud);;

let sidusKaaludetaSuundadeta = looGraaf(sidusKaaludetaSuundadetatipud, sidusKaaludetaSuundadetaservad);;

let sidusKaaludetaSuundadeta2 = looGraaf(sidusKaaludetaSuundadeta2tipud, sidusKaaludetaSuundadeta2servad);;

let sidusKaaludetaSuundadega = looGraaf(sidusKaaludetaSuundadegatipud, sidusKaaludetaSuundadegaservad);;

let sidusKaaludetaSuundadega2 = looGraaf(sidusKaaludetaSuundadega2tipud, sidusKaaludetaSuundadega2servad);;

(* tsüklitega *)		(* mitte tugevalt sidus - Kosaraju jaoks *)
let sidusKaaludetaSuundadega3 = looGraaf(sidusKaaludetaSuundadega3tipud, sidusKaaludetaSuundadega3servad);;

let sidusKaaludegaSuundadega = looGraaf(sidusKaaludegaSuundadegatipud, sidusKaaludegaSuundadegaservad);;

let sidusKaaludegaSuundadega3 = looGraaf(sidusKaaludegaSuundadegatipud3, sidusKaaludegaSuundadegaservad3);;

let sidusNegKaaludegaSuundadega2 = looGraaf(sidusNegKaaludegaSuundadega2tipud, sidusNegKaaludegaSuundadega2servad);;

let sidusKaaludegaSuundadega2 = looGraaf(sidusKaaludegaSuundadega2tipud, sidusKaaludegaSuundadega2servad);;

let sidusKaaludegaSuundadega4 = looGraaf(sidusKaaludegaSuundadega4tipud, sidusKaaludegaSuundadega4servad);;

let sidusKaaludegaSuundadega5 = looGraaf(sidusKaaludegaSuundadega5tipud, sidusKaaludegaSuundadega5servad);;

let sidusKaaludetaSuundadegaHindadega = looGraaf(sidusKaaludetaSuundadegaHindadegatipud, sidusKaaludetaSuundadegaHindadegaservad);;

let sidusKaaludetaSuundadegaHindadega2 = looGraaf(sidusKaaludetaSuundadegaHindadega2tipud, sidusKaaludetaSuundadegaHindadega2servad);;

let sidusKaaludetaSuundadegaHindadega3 = looGraaf(sidusKaaludetaSuundadegaHindadega3tipud, sidusKaaludetaSuundadegaHindadega3servad);;

let sidusKaaludetaSuundadegaHindadega4 = looGraaf(sidusKaaludetaSuundadegaHindadega4tipud, sidusKaaludetaSuundadegaHindadega4servad);;

let sidusKaaludetaSuundadega3 = looGraaf(sidusKaaludetaSuundadega3tipud, sidusKaaludetaSuundadega3servad);;

let sidusKaaludetaSuundadega4 = looGraaf(sidusKaaludetaSuundadega4tipud, sidusKaaludetaSuundadega4servad);;

(* kahest tipust koosnev suunatud ja kaaluga graaf *)
let kaksTippuSuunatudKaaluga = looGraaf(kaksTippuSuunatudKaaludegatipud, kaksTippuSuunatudKaaludegaservad);;

(* mittesidus kaaludeta ja Suundadeta graaf *)
let mitteSidusKaaludegaSuundadega = looGraaf(mitteSidusKaaludegaSuundadegatipud, mitteSidusKaaludegaSuundadegaservad);;

(* mittesidus kaaludeta ja Suundadega graaf *)
let mitteSidusKaaludetaSuundadega = looGraaf(mitteSidusKaaludetaSuundadegatipud, mitteSidusKaaludetaSuundadegaservad);;

let mitteSidusKaaludetaSuundadega2 = looGraaf(mitteSidusKaaludetaSuundadega2tipud, mitteSidusKaaludetaSuundadega2servad);;

let mitteSidusKaaludetaSuundadega3 = looGraaf(mitteSidusKaaludetaSuundadega3tipud, mitteSidusKaaludetaSuundadega3servad);;

(*sidus, aga mitte tugevalt sidus kaaludeta ja Suundadeta graaf *)
let mitteTugevaltSidus = looGraaf(mitteTugevaltSidustipud, mitteTugevaltSidusservad);;

let sidusKaaludegaSuundadeta = looGraaf(sidusKaaludegaSuundadetatipud, sidusKaaludegaSuundadetaservad);;

let mittesidusKaaludegaSuundadeta = looGraaf(mittesidusKaaludegaSuundadetatipud, mittesidusKaaludegaSuundadetaservad);;

let sidusKaaludegaSuundadeta2 = looGraaf(sidusKaaludegaSuundadetatipud2, sidusKaaludegaSuundadetaservad2);;

let sidusKaaludegaSuundadeta3 = looGraaf(sidusKaaludegaSuundadetatipud3, sidusKaaludegaSuundadetaservad3);;

let sidusKaaludegaSuundadeta4 = looGraaf(sidusKaaludegaSuundadetatipud4, sidusKaaludegaSuundadetaservad4);;

(* ühest tipust koosnev graaf *)
let yksTipp = looGraaf(looTipud([("A", 300, 300, None)]), []);;

(* ühest tipust koosnev graaf, mille tipul on hind *)
let yksTippHinnaga = looGraaf(looTipud([("A", 300, 300, Some(10))]), []);;

let ntLaiuti1 = sidusKaaludetaSuundadeta;;
let ntLaiuti2 = sidusKaaludetaSuundadeta2;;
let ntLaiuti3 = sidusKaaludetaSuundadega;;
let ntLaiuti4 = sidusKaaludetaSuundadega2;;

let ntSygavutiEes1 = sidusKaaludetaSuundadeta;;
let ntSygavutiEes2 = sidusKaaludetaSuundadeta2;;
let ntSygavutiEes3 = sidusKaaludetaSuundadega;;
let ntSygavutiEes4 = sidusKaaludetaSuundadega2;;

let ntSygavutiLopp1 = sidusKaaludetaSuundadeta;;
let ntSygavutiLopp2 = sidusKaaludetaSuundadeta2;;
let ntSygavutiLopp3 = sidusKaaludetaSuundadega;;
let ntSygavutiLopp4 = sidusKaaludetaSuundadega2;;

let ntPrim1 = sidusKaaludegaSuundadeta;;
let ntPrim2 = sidusKaaludegaSuundadeta2;;
let ntPrim3 = sidusKaaludegaSuundadeta3;;
let ntPrim4 = sidusKaaludegaSuundadeta4;;

let ntKruskal1 = sidusKaaludegaSuundadeta;;
let ntKruskal2 = sidusKaaludegaSuundadeta2;;
let ntKruskal3 = mittesidusKaaludegaSuundadeta;;
let ntKruskal4 = sidusKaaludegaSuundadeta4;;

let ntDijkstra1 = sidusKaaludegaSuundadega;;
let ntDijkstra2 = sidusKaaludegaSuundadega2;;
let ntDijkstra3 = sidusKaaludegaSuundadega4;;
let ntDijkstra4 = sidusKaaludegaSuundadega5;;

let ntFloydWarshall1 = sidusNegKaaludegaSuundadega2;;(* tsüklitega, negatiivsete kaaludega *)
let ntFloydWarshall2 = sidusKaaludegaSuundadega;;
let ntFloydWarshall3 = mitteSidusKaaludegaSuundadega;;
let ntFloydWarshall4 = sidusKaaludegaSuundadega3;;

let ntTopoLopp1 = sidusKaaludetaSuundadega;;
let ntTopoLopp2 = sidusKaaludetaSuundadega2;;
let ntTopoLopp3 = mitteSidusKaaludetaSuundadega;;
let ntTopoLopp4 = mitteSidusKaaludetaSuundadega2;;

let ntTopoKahn1 = sidusKaaludetaSuundadega;;
let ntTopoKahn2 = sidusKaaludetaSuundadega2;;
let ntTopoKahn3 = mitteSidusKaaludetaSuundadega;;
let ntTopoKahn4 = mitteSidusKaaludetaSuundadega2;;
	
let ntEeldusgraaf1 = sidusKaaludetaSuundadegaHindadega;;
let ntEeldusgraaf2 = sidusKaaludetaSuundadegaHindadega2;;
let ntEeldusgraaf3 = sidusKaaludetaSuundadegaHindadega3;;
let ntEeldusgraaf4 = sidusKaaludetaSuundadegaHindadega4;;

let ntKosaraju1 = mitteSidusKaaludetaSuundadega3;;
let ntKosaraju2 = mitteSidusKaaludetaSuundadega;;	(* iga tipp eraldi komponent *)
let ntKosaraju3 = sidusKaaludetaSuundadega4;;
let ntKosaraju4 = sidusKaaludetaSuundadega3;; (* tsüklitega *)		(* mitte tugevalt sidus *)	(* TODO: kustutada *)
