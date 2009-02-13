let pi = acos(-1.0)
let round x = int_of_float (floor (x +. 0.5))  (* cortesy of http://www.podval.org/~sds/ocaml-sucks.html *)
let fos = float_of_string
let ios = int_of_string
let foi = float_of_int
let iof = int_of_float
let fois x = (float_of_int x) /. 10000.
let iofs x = (round( x *. 10000.))
let sof = string_of_float
let soi = string_of_int
let fabs = abs_float
let mod2 a b = 
	if a >= 0 then (a mod b) 
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
	(* print_endline ( (string_of_int !linenum) ^ " : " ^ line) ; *)
	line ;;
let nothingxy (x:float) (y:float) = () ;;
let nothingsxy (s:string) (x:float) (y:float) = () ; 

type pcb_mode_type = Mode_MoveModule | Mode_MoveText |
		Mode_MoveTrack | Mode_AddTrack

let gwindowsize = ref (0, 0)
let gcurspos = ref (0. , 0. ) 
let goldcurspos = ref (0. , 0. ) 
let gpan = ref (0.0, 0.0)
let ginfodisp = ref print_endline 
let ginfodispappend = ref print_endline 
let gcursordisp = ref nothingsxy 
let print_nothing (s:string) = () 
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
let glookupnet = ref (fun (n:int) -> "unkown")
let gviaColor = ref (1., 1., 1.)
let gshowPadNumbers = ref true
let gselectRect = ref (1e99, 1e99, 1e99, 1e99)
let ggrid = [| 0.01; 0.05; 0.1; 0.5; 1.0 |]
let ggridDraw = ref true 
let ggridSnap = ref true 
let gdrawText= ref true 

let convert_units x y w h = 
	let fx = fois x in
	let fy = fois y in
	let fw = (fois w) /. 2. in
	let fh = (fois h) /. 2. in
	(fx, fy, fw, fh)
	
let layer_to_z layer = 
	let count = ref 0 in
	ignore (List.find (fun l -> 
		incr count ; 
		l = layer ) !glayerZlist ); 
	1. -. ((foi !count) /. 32.)
	;;
let update_layer_z () = 
	for i = 0 to 31 do
		glayerZ.(i) <- layer_to_z i ; 
		(* print_endline ("layer " ^(soi i)^ " depth " ^(sof glayerZ.(i))) *)
	done 
	;;
let layer_to_string layer = 
	match layer with 
		| 0 -> "Cop"
		| 1 -> "L1"
		| 2 -> "L2"
		| 3 -> "L3"
		| 4 -> "L4"
		| 5 -> "L5"
		| 15 -> "Cmp"
		| 20 -> "SS_Cop"
		| 21 -> "SS_Cmp"
		| 24 -> "Drawings"
		| _ -> ""

let string_to_layer s = 
	match s with 
		| "Cop" -> 0
		| "L1" -> 1
		| "L2" -> 2
		| "L3" -> 3
		| "L4" -> 4
		| "L5" -> 5
		| "Cmp" -> 15
		| "SS_Cop" -> 20
		| "SS_Cmp" -> 21
		| "Drawings" -> 24
		| _ -> 31

let layer_to_color layer = 
	match layer with 
		| 0 -> (0. , 1. , 0. ) ; (*green, layer 'copper' *)
		| 1 -> (0. , 0. , 1. ) ; (*blue, inner L1 *)
		| 2 -> (0.6 , 0.75 , 0.6 ) ; (*gray, inner L2 *)
		| 3 -> (0.6 , 0.6 , 0.0 ) ; (*magenta, inner L3 *)
		| 4 -> (0.0 , 0.6 , 0.6 ) ; (*cyan, inner L4 *)
		| 5 -> (0.8 , 0.2 , 0. ) ; (*red, inner L5 *)
		| 15 -> (1. , 0. , 0. ) ; (*red, component *)
		| 20 -> (1.0, 0.9, 0.65 ) ; (* magentaish, silkscreen copper *)
		| 21 -> (0.65, 1.0, 0.9 ) ; (* cyanish, silkscreen component *)
		| 24 -> (0.65, 0.65, 0.65 ) ; (* drawings *)
		| _ -> (0. , 0. , 0. )  (*black *)
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
let bbxWH (xl,yl,xh,yh) = (xh -. xl) , (yh -. yl) ;;
	
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
	
let rec range_float a b c = 
	if a > c then []
	else a :: range_float (a +. b) b c
	;;
	
let snap x grid = (floor ((x /. grid) +. 0.5)) *. grid 
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
	let hd s = match s.c with hd::tl -> hd | [] -> raise Empty
end 