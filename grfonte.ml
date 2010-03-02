(* This file is derived from grfonte.h in the Kicad project, 
	and follows the same copyright.  *)

(*  grfonte.h : codage de la fonte de caracteres utilisee en trace de
		textes graphiques  *)
(* 
formes des caracteres : definies sur matrice 10 x 13 sour forme de vecteurs
d'origine le debut du caractere (poSH_CODE matrice (0,0)) sous forme d'une sequence
ascii .les lignes au dessous de (0,0) sont notees a;b;c.
conventions :
	coord 0...9 : ascii '0'...'9' (2 octets successifs = 1 coord)
	'U'		montee de plume
	'D'		descente de plume
	'X'		fin de trace

la matrice totale du caractere est ici 12x9. et l'espacement est de 13 points
	-2..0 = jambage au dessous de la ligne
	0..9 = cadre matrice
Version actuelle:
	en X : dim 0..6 a ramener en 0..9 ( espacement = 13 )
Matrice:

	9  ----------
	8  ----------
	7  ----------
	6  ----------
	5  ----------
	4  ----------
	3  ----------
	2  ----------
	1  ----------
___ 0  ----------
	-1 ----------
	-2 ----------
	-3 ----------
	   0123456789---

	dans les descr ci dessous:
		X = fin de descr
		U = plume up
		D = plume Down
		n; n = 2 nombres = coord X et Y ( ici = -3 ... +9 )
 *)

let up = (42,42)

let noshape = [||] 		(* code non inscriptible *)
let char_shape_space = [||] 			(*  space  *)
let char_shape_ipoint = [|(0,4);(1,4);up;(4,4);(9,4);up|] 			(* ! *)
let char_shape_openacc = [|(8,3);(9,4);up;(8,5);(9,6);up|] 			(* [| *)
let char_shape_dieze = [|(3,1);(3,7);up;(5,2);(5,8);up;(1,3);(7,4);up;(1,5);(7,6);up|] 		(* # *)
let char_shape_dollar = [|(0,4);(9,4);up;(0,5);(9,5);up;(0,2);(0,7);(1,8);(3,8);(4,7);(4,2);(5,1);(7,1);(8,2);(8,7);up|] 	(* $ *)
let char_shape_percent = [|(0,1);(9,8);up;(9,1);(7,1);(7,3);(9,3);(9,1);up;(0,6);(0,8);(2,8);(2,6);(0,6);up|] 	(* % *)
let char_shape_and = [|(0,7);(7,2);(8,2);(9,4);(8,6);(7,6);(2,1);(1,1);(0,3);(0,4);(1,6);(2,7);up|] 			(* & *)
let char_shape_el = [|(9,4);(7,3);up|] 				(* ' *)
let char_shape_openpar = [|(0,4);(2,3);(7,3);(9,4);up|] 				(* ( *)
let char_shape_closepar = [|(0,4);(2,5);(7,5);(9,4);up|] 				(* ) *)
let char_shape_star = [|(1,4);(7,4);up;(4,1);(4,7);up;(2,2);(6,6);up;(6,2);(2,6);up|] 		(* * *)
let char_shape_plus = [|(4,1);(4,7);up;(1,4);(7,4);up|] 					(* + *)
let char_shape_comma = [|(1,4);(0,4);(-3,3);up|] 							(* ; *)
let char_shape_minus = [|(4,2);(4,7);up|] 							(* - *)
let char_shape_point = [|(0,4);(1,4);up|] 							(* . *)
let char_shape_slash = [|(0,2);(9,6);up|] 							(*  /  *)

let char_shape_0 = [|(0,1);(9,8);up;(1,1);(8,1);(9,2);(9,7);(8,8);(1,8);(0,7);(0,2);(1,1);up|] 		(* 0 *)
let char_shape_1 = [|(0,3);(0,7);up;(0,5);(9,5);up;(8,5);(6,3);up|] 				(* 1 *)
let char_shape_2 = [|(0,8);(0,1);(3,1);(4,2);(4,7);(5,8);(8,8);(9,7);(9,2);(8,1);up|] 			(* 2 *)
let char_shape_3 = [|(9,1);(9,8);(8,8);(5,4);(5,7);(4,8);(1,8);(0,7);(0,2);(1,1);up|] 			(* 3 *)
let char_shape_4 = [|(0,6);(9,6);(2,1);(2,8);up|] 						(* 4 *)
let char_shape_5 = [|(2,1);(1,1);(0,2);(0,7);(1,8);(4,8);(6,6);(6,1);(9,1);(9,8);up|] 			(* 5 *)
let char_shape_6 = [|(8,8);(9,7);(9,2);(8,1);(1,1);(0,2);(0,7);(1,8);(4,8);(5,7);(5,2);(4,1);up|] 		(* 6 *)
let char_shape_7 = [|(9,1);(9,8);(0,1);up|] 						(* 7 *)
let char_shape_8 = [|(0,2);(1,1);(4,1);(5,2);(5,7);(6,8);(8,8);(9,7);(9,2);(8,1);(6,1);(5,2);up;(5,7);(4,8);(1,8);(0,7);(0,2);up|]  (* 8 *)
let char_shape_9 = [|(0,2);(0,7);(1,8);(8,8);(9,7);(9,2);(8,1);(5,1);(4,2);(4,7);(5,8);up|] 		(* 9 *)

let char_shape_dbpoint = [|(6,4);(5,4);up;(3,4);(2,4);up|] 			(* : *)
let char_shape_vpoint = [|(5,4);(4,4);up;(2,4);(1,4);(-2,3);up|] 			(* ; *)
let char_shape_less = [|(0,7);(4,2);(9,7);up|] 				(* < *)
let char_shape_egal = [|(2,2);(2,7);up;(5,2);(5,7);up|] 			(* = *)
let char_shape_more = [|(0,2);(4,7);(9,2);up|] 				(* > *)
let char_shape_intpoint = [|(0,4);(1,4);up;(3,4);(4,4);(5,6);(6,7);(8,7);(9,6);(9,2);(8,1);up|] 		(* ? *)
let char_shape_arond = [|(0,8);(0,3);(2,1);(6,1);(8,3);(8,6);(6,8);(3,8);(3,5);(4,4);(5,5);(5,8);up|] 		(* @ *)

let char_shape_A = [|(0,1);(7,1);(9,3);(9,6);(7,8);(0,8);up;(5,1);(5,8);up|] 		(* A *)
let char_shape_B = [|(0,1);(9,1);(9,7);(8,8);(6,8);(5,7);(5,1);up;(5,7);(3,8);(1,8);(0,7);(0,1);up|] 	(* B *)
let char_shape_C = [|(1,8);(0,7);(0,2);(1,1);(8,1);(9,2);(9,7);(8,8);up|] 			(* C *)
let char_shape_D = [|(0,2);(9,2);up;(9,1);(9,7);(8,8);(1,8);(0,7);(0,1);up|] 		(* D *)
let char_shape_E = [|(0,8);(0,1);(9,1);(9,8);up;(5,1);(5,5);up|] 			(* E *)
let char_shape_F = [|(0,1);(9,1);(9,8);up;(5,1);(5,5);up|] 				(* F *)
let char_shape_G = [|(8,7);(9,7);(9,2);(8,1);(1,1);(0,2);(0,7);(1,8);(4,8);(4,5);up|] 		(* G *)
let char_shape_H = [|(0,1);(9,1);up;(0,8);(9,8);up;(4,1);(4,8);up|] 			(* H *)
let char_shape_I = [|(0,2);(0,6);up;(0,4);(9,4);up;(9,2);(9,6);up|] 			(* I *)
let char_shape_J = [|(2,1);(0,2);(0,4);(1,5);(9,5);up;(9,3);(9,7);up|] 			(* J *)
let char_shape_K = [|(0,1);(9,1);up;(9,7);(5,1);(0,8);up|] 			(* K *)
let char_shape_L = [|(9,1);(0,1);(0,9);up|] 				(* L *)
let char_shape_M = [|(0,1);(9,1);(5,5);(9,9);(0,9);up|] 			(* M *)
let char_shape_N = [|(0,1);(9,1);(0,8);(9,8);up|] 				(* N *)
let char_shape_O = [|(0,2);(1,1);(8,1);(9,2);(9,7);(8,8);(1,8);(0,7);(0,2);up|] 		(* O *)
let char_shape_P = [|(0,1);(9,1);(9,7);(8,8);(5,8);(4,7);(4,1);up|] 			(* P *)
let char_shape_Q = [|(1,1);(8,1);(9,2);(9,7);(8,8);(1,8);(0,7);(0,2);(1,1);up;(4,6);(0,8);up|] 		(* Q *)
let char_shape_R = [|(0,1);(9,1);(9,7);(8,8);(5,8);(4,7);(4,1);up;(4,4);(0,8);up|] 		(* R *)
let char_shape_S = [|(1,1);(0,2);(0,7);(1,8);(4,8);(5,7);(5,2);(6,1);(8,1);(9,2);(9,7);(8,8);up|] 		(* S *)
let char_shape_T = [|(9,1);(9,9);up;(9,5);(0,5);up|] 			(* T *)
let char_shape_U = [|(9,1);(1,1);(0,2);(0,7);(1,8);(9,8);up|] 			(* up; *)
let char_shape_V = [|(9,1);(0,5);(9,9);up|] 				(* V *)
let char_shape_W = [|(9,1);(0,3);(3,5);(0,7);(9,9);up|] 			(* W *)
let char_shape_X = [|(0,1);(9,8);up;(9,1);(0,8);up|] 			(* X *)
let char_shape_Y = [|(9,1);(4,5);(9,9);up;(4,5);(0,5);up|] 			(* Y *)
let char_shape_Z = [|(9,1);(9,8);(0,1);(0,8);up|] 				(* Z *)

let char_shape_opencr = [|(0,5);(0,3);(9,3);(9,5);up|] 				(* [ *)
let char_shape_backslash = [|(9,1);(0,7);up|] 				(* \ *)
let char_shape_closecr = [|(0,3);(0,5);(9,5);(9,3);up|] 				(* ] *)
let char_shape_xor = [|(7,1);(9,5);(7,8);up|] 				(* ^ *)
let char_shape_underscore = [|(0,1);(0,8);up|] 				(* _ *)
let char_shape_altel = [|(9,4);(7,5);up|] 				(* ` *)

let char_shape_a = [|(6,1);(6,7);(5,8);(0,8);(0,2);(1,1);(2,1);(3,2);(3,8);up|] 		(* a *)
let char_shape_b = [|(9,1);(0,1);(0,8);(4,8);(5,7);(5,1);up|] 		(* b *)
let char_shape_c = [|(1,8);(0,7);(0,2);(1,1);(5,1);(6,2);(6,7);(5,8);up|] 			(* c *)
let char_shape_d = [|(9,8);(0,8);(0,2);(1,1);(5,1);(6,2);(6,8);up|] 		(* d *)
let char_shape_e = [|(3,1);(3,8);(5,8);(6,7);(6,2);(5,1);(1,1);(0,2);(0,7);(1,8);up|] 		(* e *)
let char_shape_f = [|(0,2);(8,2);(9,3);(9,7);(8,8);up;(5,1);(5,4);up|] 			(* f *)
let char_shape_g = [|(-2,1);(-3,2);(-3,7);(-2,8);(5,8);(6,7);(6,2);(5,1);(1,1);(0,2);(0,7);(1,8);up|] 		(* g *)
let char_shape_h = [|(0,1);(9,1);up;(5,1);(6,2);(6,7);(5,8);(0,8);up|] 			(* h *)
let char_shape_i = [|(9,4);(8,4);up;(5,4);(1,4);(0,5);(0,6);up|] 			(* i *)
let char_shape_j = [|(9,4);(8,4);up;(5,4);(-2,4);(-3,3);(-2,2);up|] 			(* j *)
let char_shape_k = [|(0,2);(9,2);up;(3,2);(3,3);(6,8);up;(3,3);(0,8);up|] 		(* k *)
let char_shape_l = [|(9,3);(9,4);(0,4);up;(0,3);(0,5);up|] 			(* l *)
let char_shape_m = [|(0,1);(6,1);up;(5,1);(6,2);(6,4);(5,5);(0,5);up;(5,5);(6,6);(6,8);(5,9);(0,9);up|] 	(* m *)
let char_shape_n = [|(0,1);(6,1);up;(5,1);(6,2);(6,6);(5,7);(0,7);up|] 			(* n *)
let char_shape_o = [|(0,2);(1,1);(5,1);(6,2);(6,7);(5,8);(1,8);(0,7);(0,2);up|] 		(* o *)
let char_shape_p = [|(-3,1);(6,1);up;(5,1);(6,2);(6,7);(5,8);(1,8);(0,7);(0,2);(1,1);up|] 		(* p *)
let char_shape_q = [|(-3,8);(6,8);up;(5,8);(6,7);(6,2);(5,1);(1,1);(0,2);(0,7);(1,8);up|] 		(* q *)
let char_shape_r = [|(0,1);(6,1);up;(4,1);(6,3);(6,6);(4,8);up|] 			(* r *)
let char_shape_s = [|(1,1);(0,2);(0,7);(1,8);(2,8);(3,7);(3,2);(4,1);(5,1);(6,2);(6,7);(5,8);up|] 		(* s *)
let char_shape_t = [|(1,7);(0,6);(0,3);(1,2);(9,2);up;(6,2);(6,5);up|] 			(* t *)
let char_shape_u = [|(6,1);(2,1);(0,3);(0,6);(2,8);(6,8);up|] 			(* u *)
let char_shape_v = [|(6,1);(3,1);(0,4);(3,7);(6,7);up|] 			(* v *)
let char_shape_w = [|(6,1);(2,1);(0,3);(2,5);(0,7);(2,9);(6,9);up|] 			(* w *)
let char_shape_x = [|(0,1);(6,8);up;(6,1);(0,8);up|] 			(* x *)
let char_shape_y = [|(6,1);(2,1);(0,3);(0,6);(2,8);up;(6,8);(-2,8);(-3,7);(-3,2);(-2,1);up|]  (* y *)
let char_shape_z = [|(6,1);(6,8);(0,1);(0,8);up|]  (* z *)

let char_shape_opbrack = [|(9,5);(8,4);(6,4);(5,3);(4,3);(3,4);(1,4);(0,5);up|]  (* { *)
let char_shape_or = [|(9,4);(0,4);up|] 				(* | *)
let char_shape_closebr = [|(9,3);(8,4);(6,4);(5,5);(4,5);(3,4);(1,4);(0,3);up|]  (* } *)
let char_shape_tilda = [|(8,1);(9,2);(9,3);(7,5);(7,6);(8,7);up|]  (* ~ *)
let char_shape_del = [||]  (* <del> *)

(*  codes utiles >= 128  *)
let char_shape_C_Cedille = [|(1,8);(0,7);(0,2);(1,1);(8,1);(9,2);(9,7);(8,8);up|]  (*  C cedille *)
let char_shape_c_cedille = [|(1,8);(0,7);(0,2);(1,1);(5,1);(6,2);(6,7);(5,8);up;(0,4);(-3,2);up|]  (*  ç  *)
let char_shape_a_grave = [|(6,1);(6,7);(5,8);(0,8);(0,2);(1,1);(2,1);(3,2);(3,8);up;(7,4);(9,2);up|] 
let char_shape_a_aigu = [|(6,1);(6,7);(5,8);(0,8);(0,2);(1,1);(2,1);(3,2);(3,8);up;(7,2);(9,4);up|] 
let char_shape_a_circ = [|(6,1);(6,7);(5,8);(0,8);(0,2);(1,1);(2,1);(3,2);(3,8);up;(7,1);(9,4);(7,7);up|] 
let char_shape_a_trema = [|(6,1);(6,7);(5,8);(0,8);(0,2);(1,1);(2,1);(3,2);(3,8);up;(9,2);(9,3);up;(9,5);(9,6);up|] 
let char_shape_e_grave = [|(3,1);(3,8);(5,8);(6,7);(6,2);(5,1);(1,1);(0,2);(0,7);(1,8);up;(7,4);(9,2);up|] 
let char_shape_e_aigu = [|(3,1);(3,8);(5,8);(6,7);(6,2);(5,1);(1,1);(0,2);(0,7);(1,8);up;(7,3);(9,5);up|] 
let char_shape_e_circ = [|(3,1);(3,8);(5,8);(6,7);(6,2);(5,1);(1,1);(0,2);(0,7);(1,8);up;(7,1);(9,4);(7,7);up|] 
let char_shape_e_trema = [|(3,1);(3,8);(5,8);(6,7);(6,2);(5,1);(1,1);(0,2);(0,7);(1,8);up;(9,2);(9,3);up;(9,5);(9,6);up|] 
let char_shape_i_trema = [|(5,4);(1,4);(0,5);(0,6);up;(9,2);(9,3);up;(9,5);(9,6);up|] 
let char_shape_i_circ = [|(5,4);(1,4);(0,5);(0,6);up;(7,1);(9,4);(7,7);up|] 
let char_shape_u_grave = [|(6,1);(2,1);(0,3);(0,6);(2,8);(6,8);up;(7,4);(9,2);up|] 
let char_shape_o_trema = [|(0,2);(1,1);(5,1);(6,2);(6,7);(5,8);(1,8);(0,7);(0,2);up;(9,2);(9,3);up;(9,5);(9,6);up|] 
let char_shape_o_circ = [|(0,2);(1,1);(5,1);(6,2);(6,7);(5,8);(1,8);(0,7);(0,2);up;(7,2);(9,4);(7,7);up|] 
let char_shape_u_circ = [|(6,1);(2,1);(0,3);(0,6);(2,8);(6,8);up;(7,1);(9,4);(7,7);up|] 
let char_shape_u_trema = [|(6,1);(2,1);(0,3);(0,6);(2,8);(6,8);up;(9,2);(9,3);up;(9,5);(9,6);up|] 

let shapes =
	[|
	(*  codes 0..31: *)
	noshape; noshape; noshape; noshape; noshape; noshape; noshape; noshape;
	noshape; noshape; noshape; noshape; noshape; noshape; noshape; noshape;
	noshape; noshape; noshape; noshape; noshape; noshape; noshape; noshape;
	noshape; noshape; noshape; noshape; noshape; noshape; noshape; noshape;
	(*  codes 32 .. 127: *)
	char_shape_space;					(*  space  *)
	char_shape_ipoint;				(* ! *)
	char_shape_openacc;
	char_shape_dieze;
	char_shape_dollar;
	char_shape_percent;
	char_shape_and;
	char_shape_el;
	char_shape_openpar;
	char_shape_closepar;
	char_shape_star;
	char_shape_plus;
	char_shape_comma;
	char_shape_minus;
	char_shape_point;
	char_shape_slash;
	char_shape_0; char_shape_1; char_shape_2; char_shape_3; char_shape_4; char_shape_5; char_shape_6; char_shape_7; char_shape_8; char_shape_9;
	char_shape_dbpoint;
	char_shape_vpoint;
	char_shape_less;
	char_shape_egal;
	char_shape_more;
	char_shape_intpoint;
	char_shape_arond;

	char_shape_A; char_shape_B; char_shape_C; char_shape_D; char_shape_E; char_shape_F; char_shape_G; char_shape_H; char_shape_I;
	char_shape_J; char_shape_K; char_shape_L; char_shape_M; char_shape_N; char_shape_O; char_shape_P; char_shape_Q; char_shape_R;
	char_shape_S; char_shape_T; char_shape_U; char_shape_V; char_shape_W; char_shape_X; char_shape_Y; char_shape_Z;

	char_shape_opencr;
	char_shape_backslash;
	char_shape_closecr;
	char_shape_xor;
	char_shape_underscore;
	char_shape_altel;

	char_shape_a; char_shape_b; char_shape_c; char_shape_d; char_shape_e; char_shape_f; char_shape_g; char_shape_h; char_shape_i;
	char_shape_j; char_shape_k; char_shape_l; char_shape_m; char_shape_n; char_shape_o; char_shape_p; char_shape_q; char_shape_r;
	char_shape_s; char_shape_t; char_shape_u; char_shape_v; char_shape_w; char_shape_x; char_shape_y; char_shape_z;

	char_shape_opbrack;
	char_shape_or;
	char_shape_closebr;
	char_shape_tilda;
	char_shape_del;
	(*  codes >= 128: *)
	noshape; noshape; noshape; noshape;		(*  128..131 *)
	noshape; noshape; noshape; noshape;	(*  132..135 *)
	noshape; noshape; noshape; noshape;	(* 136..139 *)
	noshape; noshape; noshape; noshape;	(* 140..143 *)
	noshape; noshape; noshape; noshape;	(* 144..147 *)
	noshape; noshape; noshape; noshape;	(* 148..151 *)
	noshape; noshape; noshape; noshape;	(* 152..155 *)
	noshape; noshape; noshape; noshape;	(* 156..159 *)

	noshape; noshape; noshape; noshape;	(* 160..163 *)
	noshape; noshape; noshape; noshape;	(* 164..167 *)
	noshape; noshape; noshape; noshape;	(*  168..171 *)
	noshape; noshape; noshape; noshape;	(* 172..175 *)
	noshape; noshape; noshape; noshape;	(* 176..179 *)
	noshape; noshape; noshape; noshape;	(* 180..183 *)
	noshape; noshape; noshape; noshape;	(* 184..187 *)
	noshape; noshape; noshape; noshape;	(* 188..191 *)

	noshape; noshape; noshape; noshape;	(* 192..195 *)
	noshape; noshape; noshape; char_shape_C_Cedille;	(* 196..199 *)
	noshape; noshape; noshape; noshape;	(*  200..203 *)
	noshape; noshape; noshape; noshape;	(* 204..207 *)
	noshape; noshape; noshape; noshape;	(* 208..211 *)
	noshape; noshape; noshape; noshape;	(* 212..215 *)
	noshape; noshape; noshape; noshape;	(* 216..219 *)
	noshape; noshape; noshape; noshape;	(* 220..223 *)

	char_shape_a_grave; char_shape_a_aigu; char_shape_a_circ; noshape;	(* 224..227 *)
	char_shape_a_trema; noshape; noshape; char_shape_c_cedille;	(* 228..231 *)
	char_shape_e_grave; char_shape_e_aigu; char_shape_e_circ; char_shape_e_trema;	(* 232..235 *)
	noshape; noshape; char_shape_i_circ; char_shape_i_trema;	(* 236..239 *)
	noshape; noshape; noshape; noshape;	(* 240..243 *)
	char_shape_o_circ; noshape; char_shape_o_trema; noshape;	(* 244..247 *)
	noshape; char_shape_u_grave; noshape; char_shape_u_circ;	(* 248..251 *)
	char_shape_u_trema; noshape; noshape; noshape;	(* 252..255 *)

	|]



