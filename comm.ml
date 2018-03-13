(* Copyright 2008-2010, Timothy L Hanson *)
(* This file is part of Kicadocaml.

    Kicadocaml is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Kicadocaml is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Kicadocaml.  If not, see <http://www.gnu.org/licenses/>.
*)
let gscl = ref 10000.0 (* global file unit scaling (divide by this) *)
let gunit = ref "in"
let gfver = ref 1 (* file version *)
let pi = acos(-1.0)
let round x = int_of_float (floor (x +. 0.5))  (* cortesy of http://www.podval.org/~sds/ocaml-sucks.html *)
let fos = float_of_string
let ios = int_of_string
let foi = float_of_int
let iof = int_of_float
let foss x = (float_of_string x) /. !gscl
let sofs x = (if !gfver = 1 then 
		(string_of_int (round( x *. !gscl))) 
		else 
		(string_of_float (x *. !gscl)))
let sof = string_of_float
let soi = string_of_int
let fabs = abs_float
let fsign f = if f > 0.0 then 1.0 else if f < 0.0 then -1.0 else 0.0 
let mod2 a b =  (*this modulus maps to the positive numbers, unlike regular mod, which preserves sign *)
	if a >= 0 then (a mod b) (* it implements, in effect, a continuous sawtooth function *)
	else (
		let c = a + (-1*a/b+1)*b in
		c mod b
	) ;;
let fsto3 (g,_,_) = g ;;
let sndo3 (_,g,_) = g;;
let trdo3 (_,_,g) = g ;; 

let xor a b = (a && (not b)) || ((not a) && b) 

(* this used for debugging *)
let linenum = ref 0
let gfilereadname = ref ""
let input_line2 ic = 
	let line = input_line ic in
	linenum := !linenum + 1 ; 
 	print_endline ( (string_of_int !linenum) ^ " : " ^ line) ; 
	line ;;
let nothingxy (_:float) (_:float) = () ;;
let nothingsxy (_:string) (_:float) (_:float) = () ; 

type pcb_mode_type = Mode_MoveModule | Mode_MoveCell | Mode_MoveText |
		Mode_MoveTrack | Mode_AddTrack

let gwindowsize = ref (0, 0)
let gcurspos = ref (0. , 0. ) 
let goldcurspos = ref (0. , 0. ) 
let gpan = ref (0.0, 0.0)
let ginfodisp = ref print_endline 
let ginfodispappend = ref print_endline 
let gcursordisp = ref nothingsxy 
let print_nothing (_:string) = () 
let gcrossprobe = ref print_nothing 
let gdragShowAllNets = ref false 
let gdragShow5Smallest = ref false 
let gLimitRatnest200 = ref true
let grefresh = ref false 
let glayer = ref 0 
let glayerEn = Array.make 32 true 
let glayerPresent = ref [0]  (* pertains to copper layers -- setup while reading the file *)
let glayerZ = Array.make 32 0. 
let glayerZlist = ref []  (* list of integers, the layer numbers *)
let gmode = ref Mode_MoveModule 
let gsnapped = ref (0. , 0.)
let gclearance = ref 0.0098  (* 0.125mm *)
let gcurnet = ref 0 
let glookupnet = ref (fun (_:int) -> "unknown")
let gInvLookupNet = ref (fun (_:string) -> 0)
let gviaColor = ref (1., 1., 1.)
let gshowPadNumbers = ref true
let gshowHiddenText = ref true
let gselectRect = ref (1e99, 1e99, 1e99, 1e99)
let ggrid = [| 0.01; 0.05; 0.1; 0.5; 1.0 |]
let ggridDraw = ref false 
let ggridSnap = ref true 
let gsnapTracksToGrid = ref false
let gdrawText= ref true 
let gdosnap = ref true
let gTrackEndpointOnly = ref false
let gTrackOnlyOne = ref false
let gTrackDragConnected = ref false

let random_timestamp () = Printf.sprintf "%x" (Int32.to_int(Random.int32 (Int32.max_int))) ;;

let convert_units x y w h = 
	let fx = foss x in
	let fy = foss y in
	let fw = (foss w) /. 2. in
	let fh = (foss h) /. 2. in
	(fx, fy, fw, fh)
	
let format_dim x = 
	let y = x /. 4.000 in
	let z,unit = if y > 0.5 then (
		(y, "mm")
	) else (
		if y >= 0.001 then 
		(y *. 1000.0, "um")
		else (y *. 1000000.0, "nm")
	) in
	Printf.sprintf "%2.4f ( %2.3f %s )" x z unit
	;;
	
(* Z sorting: positive is on top. *)
let layer_to_z layer = 
	let count = ref 0 in
	ignore (List.find (fun l -> 
		incr count ; 
		l = layer ) !glayerZlist ); 
	1. -. ((foi !count) /. 40.)
	;;
let update_layer_z () = 
	for i = 0 to 31 do
		glayerZ.(i) <- layer_to_z i ; 
		(* print_endline ("layer " ^(soi i)^ " depth " ^(sof glayerZ.(i))) *)
	done 
	;;
let layer_to_string layer = 
	match layer with 
		| 0 -> "PPX"
		| 1 -> "L1"
		| 2 -> "L2"
		| 3 -> "L3"
		| 4 -> "L4"
		| 5 -> "L5"
		| 6 -> "L6"
		| 7 -> "L7"
		| 8 -> "L8"
		| 9 -> "L9"
		| 10 -> "L10"
		| 11 -> "L11"
		| 12 -> "L12"
		| 13 -> "L13"
		| 14 -> "L14"
		| 15 -> "M1"
		| 20 -> "M2"
		| 21 -> "M3"
		| 22 -> "M4"
		| 23 -> "M5"
		| 24 -> "Outline"
		| _ -> ""

let string_to_layer s = 
	match s with 
    | "PPX" -> 0
		| "L1" -> 1
		| "L2" -> 2
		| "L3" -> 3
		| "L4" -> 4
		| "L5" -> 5
		| "L6" -> 6
		| "L7" -> 7
		| "L8" -> 8
		| "L9" -> 9
		| "L10" -> 10
		| "L11" -> 11
		| "L12" -> 12
		| "L13" -> 13
		| "L14" -> 14
		| "M1" -> 15
		| "M2" -> 20
		| "M3" -> 21
		| "M4" -> 22
		| "M5" -> 23
		| "Outline" -> 24
		| _ -> 31

let layer_to_color layer = 
	match layer with 
(*		| 0 -> (0.0 , 1.0 , 0.0 ) ; (*green *)
		| 1 -> (0.0 , 0.0 , 1.0 ) ; (*blue, inner L1 *)
		| 2 -> (0.0 , 0.5 , 1.0 ) ; (*gray, inner L2 *)
		| 3 -> (0.0 , 1.0 , 0.5 ) ; (*magenta, inner L3 *)
		| 4 -> (0.5 , 1.0 , 0.0 ) ; (*cyan, inner L4 *)
		| 5 -> (0.5 , 0.0 , 1.0 ) ; (*red, inner L5 *)
    | 6 -> (1.0 , 0.0 , 0.5 ) ; (*yellow-green*)
		| 7 -> (1.0 , 0.5 , 0.0 ) ; (*gray, inner L2 *)
		| 8 -> (0.0 , 1.0 , 1.0 ) ; (*magenta, inner L3 *)
		| 9 -> (1.0 , 1.0 , 0.0 ) ; (*cyan, inner L4 *)
		| 10 -> (1.0 , 0.0 , 1.0 ) ; (*red, inner L5 *)
    | 11 -> (0.50 , 0.75 , 0.25 ) ; (*blue, inner L1 *)
		| 12 -> (0.75 , 0.25 , 0.50 ) ; (*gray, inner L2 *)
		| 13 -> (0.25 , 0.75 , 0.50 ) ; (*magenta, inner L3 *)
		| 14 -> (0.50 , 0.6 , 0.6 ) ; (*cyan, inner L4 *)
		| 15 -> (1. , 0. , 0. ) ; (*red, component , top*)
		| 20 -> (1.0, 0.9, 0.65 ) ; (* magentaish, silkscreen copper *)
		| 21 -> (0.65, 1.0, 0.9 ) ; (* cyanish, silkscreen component *)
		| 22 -> (1.0, 0.6, 0.0 ) ; (* orange, bottom solder mask *)
		| 23 -> (0.6, 0.0, 1.0 ) ; (* purple, top solder mask *)
		| 24 -> (0.65, 0.65, 0.65 ) ; (* drawings *)*)
(* -1625845152 maximizing min (dist - 0.3*cos(theta)) -0.008402 *)
    | 15 -> (1.00, 0.00, 0.00);
    | 0 -> (0.00, 1.00, 0.00);
    | 24 -> (0.50, 0.50, 0.50);
    | 1 -> (0.00, 0.65, 0.47);
    | 2 -> (0.87, 0.38, 0.86);
    | 3 -> (0.87, 0.65, 0.03);
    | 4 -> (0.52, 0.01, 1.00);
    | 5 -> (0.50, 0.50, 0.00);
    | 6 -> (0.38, 0.03, 0.00);
    | 7 -> (0.38, 0.80, 0.39);
    | 8 -> (0.41, 0.84, 0.90);
    | 9 -> (1.00, 0.51, 0.00);
    | 10 -> (0.01, 0.50, 0.97);
    | 11 -> (0.43, 0.00, 0.46);
    | 12 -> (0.74, 1.00, 0.00);
    | 20 -> (0.00, 0.19, 0.45);
    | 21 -> (0.00, 0.43, 0.01);
    | 22 -> (0.00, 0.00, 0.87);
    | 23 -> (0.98, 0.00, 0.51);
		| _ -> (0. , 0. , 0. )  (*black *)
		(* other relevant layers -
		16 -> Adhes_cop, bottom		0x00010000
		17 -> Adhes_cmp, top		0x00020000
		18 -> Soldp_cop, bot		0x00040000
		19 -> Soldp_cmp, top		0x00080000
		20 -> SilkS_cop, bot		0x00100000
		21 -> SilkS_cmp, top		0x00200000
		22 -> Mask_cop, bot			0x00400000
		23 -> Mask_cmp, top			0x00800000
		24 -> Drawings				0x01000000
		25 -> Comments				0x02000000
		*)
		;;
		
let flip_layer layer = 
	match layer with
		| 0 -> 15 (* bottom to top *)
		| 15 -> 0 (* top to bottom *)
		| 18 -> 19 (* bottom solder paste to top *)
		| 19 -> 18 (* vice versa *)
		| 20 -> 21 (* bottom silkscreen to top *)
		| 21 -> 20
		| 22 -> 23 (* bottom solder mask to top *)
		| 23 -> 22 
		| a -> a (* do not flip the internal layers *)
		;;
		
let clamp a b c = 
	if a < b then b 
	else (
		if a > c then c
		else a
	)
	;;
let layers_to_color layers enabled = 
	let saturate (r,g,b) = 
		((clamp r 0. 1.),(clamp g 0. 1.),(clamp b 0. 1.))
	in
	saturate (
		List.fold_left (fun (ar,ag,ab) b -> 
			let (br,bg,bb) = if enabled.(b) then
				layer_to_color b
				else (0. , 0., 0.)
			in
			(ar +. br, ag +. bg, ab +. bb)) (0., 0., 0.) layers
	)
	;;
let sort_int_list lst = 
	List.sort ( fun a b -> 
				if a > b then 1 else if b > a then -1 else 0 
				) lst 	
	;;
let rotateZ ~angle (x, y, z) = 
	(*rotate CW about the Z axis *)
	(cos(angle) *. x +. sin(angle) *. y, 
	 -1. *. sin(angle) *. x +. cos(angle) *. y, z)
	 ;;
	 
let rotate2 ~angle (x, y) = 
	(*rotate CW about the Z axis, 2d output *)
	(cos(angle) *. x +. sin(angle) *. y, 
	 -1. *. sin(angle) *. x +. cos(angle) *. y)
	 ;;
	 
let reverse_array arr = 
	let l = Array.length arr in
	Array.init l (fun i -> arr.(l-1-i)) 

let string_to_layers s = 
	(*converts a 8.8lX formatted string 
	into a list of layers *)
	let r = Int32.of_string( "0x" ^ s) in
	let i = ref (Int32.one) in
	let st = ref [] in
	for k = 0 to 31 do (
		if Int32.to_int(Int32.logand !i r) > 0 then (
			st := (k :: !st) ;
		); 
		i := Int32.shift_left !i 1 ; 
	) done; 
	!st 
	;;
	
let layers_to_int32 layers = 
	(*converts a list of layers to a int32 *)
	let r = ref ( Int32.of_int 0 ) in 
	List.iter (fun p ->
		r := Int32.add !r (Int32.shift_left Int32.one p)
	) layers ; 
	!r 
;;

(* should re-write the bbxIntersect to be as fast as possible, since we use it for online
DRC (no 'let' statements)*)

let bbxIntersect (xl1,yl1,xh1,yh1) (xl2,yl2,xh2,yh2) = 
	((xl1 <= xl2 && xl2 <= xh1) || (xl1 <= xh2 && xh2 <= xh1) ||
	 (xl2 <= xl1 && xl1 <= xh2) || (xl2 <= xh1 && xh1 <= xh2)) &&
	((yl1 <= yl2 && yl2 <= yh1) || (yl1 <= yh2 && yh2 <= yh1) ||
	 (yl2 <= yl1 && yl1 <= yh2) || (yl2 <= yh1 && yh1 <= yh2))
	 ;;
	
let bbxCenter (xl,yl,xh,yh) = ( ((xl +. xh) /. 2.), ((yl +. yh) /. 2.) )
	;;
let bbxWH (xl,yl,xh,yh) = (xh -. xl) , (yh -. yl) 
	;;
let bbxSize (minx, miny, maxx, maxy) = (maxx -. minx) *. (maxy -. miny )
	;;
let bbxTranslate (xl1,yl1,xh1,yh1) (mx,my) = 
	(xl1 +. mx, yl1 +. my, xh1 +. mx, yh1 +. my)
	;;
let bbxMerge  (xl1,yl1,xh1,yh1) (xl2,yl2,xh2,yh2) = 
	let min a b = if a<b then a else b in
	let max a b = if a>b then a else b in
	(min xl1 xl2),(min yl1 yl2),(max xh1 xh2),(max yh1 yh2)
	;;
let bbxInside (xl,yl,xh,yh) (px, py) = 
	(* is a point within a bounding box *)
	xl <= px && px <= xh && yl <= py && py <= yh
	;;
let bbxRotate (xl,yl,xh,yh) r = 
	let xln,yln = Pts2.rotate (xl,yl) r in
	let xhn,yhn = Pts2.rotate (xh,yh) r in
	((min xln xhn),
	(min yln yhn),
	(max xln xhn),
	(max yln yhn))
	;;
let bbxAdd bbx (px,py) = 
	bbxMerge bbx (px,py,px,py)
	;;
let fmin (a:float) (b:float) = if a < b then a else b ;;
let fmax (a:float) (b:float) = if a > b then a else b ;;

module Integer =
struct
	type t = int
	let compare a b = (
		if a < b then -1 
		else if a > b then 1 
		else 0
	)
end

let compare_I2 a b = 
	if (fst a) < (fst b) then 
		-1
	else if (fst a) > (fst b) then 
		1 
	else (
		if (snd a) < (snd b) then 
			-1
		else if (snd a) > (snd b) then
			1
		else 0
	)
	;;

let compare_float (a:float) (b:float) = 
	if a < b then -1
	else if a > b then 1
	else 0
	;;
	
let compare2 a b = 
	let r = compare (fst a) (fst b) in
	if r = 0 then compare (snd a) (fst b) else r
	;;
(*	
let rec range_float a b c = (* not tail-recursive *)
	if a > c then []
	else a :: range_float (a +. b) b c
	;;
*)
	
let range_float a b c = (* start step end; tail-recursive*)
	let rec rf_aux acc d = 
		if d > c then List.rev acc
		else rf_aux (d :: acc) (d +. b) 
	in
	rf_aux [] a
	;;
	
let snap x grid origin = (floor (((x -. origin) /. grid) +. 0.5)) *. grid +. origin
	(* snap to a grid point. *)
	;;

module SI = Set.Make(Integer) 
module SI2 = Set.Make(struct 
		type t = int*int
		let compare = compare_I2
	end) 
	
(* http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora130.html *)
(* need a stack with a 'head' function  - which does *not* pop. *)
(* or even better a function which pops then returns the *next* head *)
module Stack2 =
struct
	type 'a t = { mutable c : 'a list }
	exception Empty
	let create () = { c = [] }
	let clear s = s.c <- []
	let push x s = s.c <- x :: s.c
	let pop s = match s.c with hd::tl -> s.c <- tl; hd | [] -> raise Empty
	let pop_nxt s = 
		match List.length s.c with
			| 0 -> raise Empty
			| 1 -> List.hd s.c
			| _ -> s.c <- List.tl s.c ; List.hd s.c
	let length s = List.length s.c
	let iter f s = List.iter f s.c
	let hd s = match s.c with 
		| hd::_ -> hd 
		| [] -> raise Empty
end 
