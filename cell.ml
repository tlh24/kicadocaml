(* Copyright 2008-2017, Timothy L Hanson *)
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
open Printf
open Comm
open Grfx
open Track

(* ocaml indexing: a.(row).(col) -- C-style. *) 
let matrix_multiply x y =
  let x0 = Array.length x
  and y0 = Array.length y in
  let y1 = if y0 = 0 then 0 else Array.length y.(0) in
  let z = Array.make_matrix x0 y1 0.0 in
  for i = 0 to x0-1 do
    for j = 0 to y1-1 do
      for k = 0 to y0-1 do
        z.(i).(j) <- z.(i).(j) +. x.(i).(k) *. y.(k).(j)
      done
    done
  done;
  z ;;
  
let matrix_vector_mult m a = 
	let x0 = Array.length m (* rows *)
	and y0 = Array.length a in
	let z = Array.make y0 0.0 in
	for i = 0 to x0-1 do
		for j = 0 to y0-1 do
			z.(i) <- z.(i) +. m.(i).(j) *. a.(j); 
		done;
	done;
	z ;; 	
  
let matrix_identity n = 
	let z = Array.make_matrix n n 0.0 in
	for i = 0 to n-1 do
		z.(i).(i) <- 1.0; 
	done; 
	z ;; 
	
let read_matrix ic = 
	let line2 = ref "" in
	line2 := input_line2 ic ; 
	let mat = matrix_identity 3 in
	for j = 0 to 2 do 
		let sp = Pcre.extract ~pat:"Tm ([\.\d-]+) ([\.\d-]+) ([\.\d-]+)" !line2 in
		for i = 0 to 2 do
			mat.(j).(i) <- fos sp.(1+i) ; 
		done ;
		line2 := input_line2 ic ;
	done;
	mat, !line2 ;;
	
type cell_instance = {
	mutable name: string; 
	mutable offset : float array array; 
	mutable n : int; 
	mutable iter : float array array; 
	mutable bbx : (float * float * float * float);
	mutable hit : bool;
	mutable gr : grfx;
	mutable moving : bool;
	mutable move : (float * float);
}
type cell = {
	mutable name : string ;
	mutable visible : bool ;
	mutable tracks : pcb_track list ;
	mutable cells : cell_instance list ; 
	mutable bbx : (float * float * float * float);
}

let ci_null () = 
	let ci = {
		name = ""; 
		offset = matrix_identity 3; 
		n = 1; 
		iter = matrix_identity 3; 
		bbx = (0.0, 0.0, 0.0, 0.0); 
		hit = false; 
		gr = new grfx;
		moving = false; 
		move = (0.0, 0.0); 
	} in 
	ci
	;;
let ce_null () = 
	let ce = {
		name = ""; 
		visible = true; 
		tracks = []; 
		cells = [];
		bbx = (0.0, 0.0, 0.0, 0.0); 
	} in
	ce
	;;

let ci_read ic line = 
	let ci = ci_null () in
	let sp = Pcre.extract ~pat:"Ci (\w+)" line in
	ci.name <- sp.(1); 
	let m,l = read_matrix ic in
	ci.offset <- m; 
	let sp = Pcre.extract ~pat:"Ni ([\.\d-]+)" l in
	ci.n <- (ios sp.(1)); 
	let m2,l2 = read_matrix ic in
	ci.iter <- m2; 
	ci,l2 
	;;
	
let ci_save (ci:cell_instance) oc = 
	fprintf oc "Ci %s\n" ci.name; 
	let print_matrix m = 
		for j = 0 to 2 do
			fprintf oc "Tm %f %f %f\n" m.(j).(0) m.(j).(1) m.(j).(2); 
		done;
	in
	print_matrix ci.offset; 
	fprintf oc "Ni %d\n" ci.n; 
	print_matrix ci.iter 
	;;

let ci_draw ci bbx = 
	let z = if ci.hit then 0.01 else 0.0 in
	if ci.moving then (
		GlMat.push () ; 
		GlMat.translate ~x:(fst ci.move) ~y:(snd ci.move) ~z (); 
	); 
	if ci.hit then ci.gr#setColor (0.0, 0.3, 1.0)
	else ci.gr#setColor (1.0, 0.3, 0.0);
	ci.gr#setAlpha 0.1; 
	ignore(ci.gr#draw bbx); 
	if ci.moving then (
		GlMat.pop () ; 
	); 
	;;
let ci_applyMove ci = 
	if ci.moving then (
		ci.offset.(0).(2) <- ci.offset.(0).(2) +. fst ci.move;
		ci.offset.(1).(2) <- ci.offset.(1).(2) +. snd ci.move; 
		ci.move <- (0.0, 0.0); 
		ci.moving <- false;
	)
	;;
let ci_hit (ci:cell_instance) (p, nn, hitsize_, hitz_, hitclear_) = 
	if bbxInside ci.bbx p then (
		if (bbxSize ci.bbx) < hitsize_ then (
			List.iter (fun f -> f ()) hitclear_; 
			ci.hit <- true; 
			!ginfodisp ( "cell instance " ^ ci.name);
			nn, (bbxSize ci.bbx), hitz_, [(fun () -> ci.hit <- false; )]
		) else (
			ci.hit <- false; 
			(nn, hitsize_, hitz_, hitclear_)
		)
	) else (
		ci.hit <- false; 
		(nn, hitsize_, hitz_, hitclear_)
	)
	;;

let ce_read ic line = 
	let line2 = ref line in 
	let ce = ce_null () in
	let sp = Pcre.extract ~pat:"Cn (\w+)" !line2 in
	ce.name <- sp.(1); 
	(* read in the tracks *)
	line2 := input_line2 ic ; (* eat $TRACKS *)
	while not (Pcre.pmatch ~pat:"EndTRACKS" !line2) do 
	(
		try( 
			let t = new pcb_track in
			t#read ic !line2 ; 
			ce.tracks <- (t :: ce.tracks) ; 
			line2 := input_line2 ic ; 
		) with _ -> line2 := input_line2 ic ; 
	)done ;
	line2 := input_line2 ic;  (* eat $CELLINSTANCE *)
	while not (Pcre.pmatch ~pat:"EndCELLINSTANCE" !line2) do 
	(
		try( 
			let ces, l2 = ci_read ic !line2 in
			line2 := l2; 
			ce.cells <- (ces :: ce.cells); 
		) with _ -> line2 := input_line2 ic ; 
	) done ;
	ce
	;;
let ce_save ce oc = 
	fprintf oc "Cn %s\n" ce.name; 
	fprintf oc "$TRACKS\n";
	List.iter (fun t -> t#save oc) ce.tracks; 
	fprintf oc "$EndTRACKS\n"; 
	fprintf oc "$CELLINSTANCE\n"; 
	List.iter (fun ci -> ci_save ci oc) (List.rev ce.cells); 
	fprintf oc "$EndCELLINSTANCE\n"; 
	;;
let ce_draw ce bbx lay firstLayer = 
	if ce.visible then (
		List.iter (fun t -> 
			if(t#getLayer() == lay) then
				t#draw bbx
		) ce.tracks; 
	);
	if lay = firstLayer then 
		List.iter (fun ci -> ci_draw ci bbx) ce.cells ; 
	;;
let ce_update ce = 
	List.iter (fun t -> 
		t#update ()
	) ce.tracks
	;;
let addTrack ce track = 
	ce.tracks <- (track :: ce.tracks)
	;;
let filterTracks ce f = 
	ce.tracks <- List.filter f ce.tracks
	;;
	
let ce_hit ce (p, onlyworknet, netn_, hitsize_, hitz_, hitclear_) = 
	if ce.visible then (
		let (nn,hitsize2,hitz2,hitclear2) = 
		List.fold_left (fun (netn, hitsize, hitz, hitclear) t -> 
			t#hit (p, onlyworknet, netn, hitsize, hitz, hitclear)
		) (netn_, hitsize_, hitz_, hitclear_) ce.tracks in
		(* see if we hit any (immediate, first-level) instances *)
		(* hitting deeper nodes doesn't make sense, too confusing to edit.*)
		List.fold_left (fun (netn, hitsize, hitz, hitclear) ci -> 
			ci_hit ci (p, netn, hitsize, hitz, hitclear)
		) (nn, hitsize2, hitz2, hitclear2) (ce.cells)
	) else (netn_, hitsize_, hitz_, hitclear_)
	;;
	
let rec ce_updateBBX ce (cells:cell list) tm = 
	let bbx2 = List.fold_left (fun bbx t -> 
		bbxMerge bbx (t#getBBX ()) )
		(1e6, 1e6, -1e6, -1e6)
		ce.tracks in
	(* add in the cell instances, too *)
	ce.bbx <- List.fold_left (fun bbx (ci:cell_instance) -> 
		ci_updateBBX ci cells tm; 
		bbxMerge ci.bbx bbx)
		bbx2
		ce.cells 
	
and ci_updateBBX (ci:cell_instance) (cells:cell list) tm = 
	let bbxTransform (mx,my,nx,ny) mm = 
		let p0 = matrix_vector_mult mm [| mx ; my ; 1.0 |] in 
		let p1 = matrix_vector_mult mm [| nx ; ny ; 1.0 |] in 
		let p2 = matrix_vector_mult mm [| mx ; ny ; 1.0 |] in 
		let p3 = matrix_vector_mult mm [| nx ; my ; 1.0 |] in 
		let (o1,o2,o3,o4) = 
			(min (min p0.(0) p1.(0)) (min p2.(0) p3.(0))), 
			(min (min p0.(1) p1.(1)) (min p2.(1) p3.(1))), 
			(max (max p0.(0) p1.(0)) (max p2.(0) p3.(0))), 
			(max (max p0.(1) p1.(1)) (max p2.(1) p3.(1))) in
		(*for i = 0 to 2 do 
			printf "%f %f %f \n%!" mm.(i).(0) mm.(i).(1) mm.(i).(2)
		done; 
		printf "bbxTransform %s %f %f %f %f (%f %f) (%f %f)\n%!" ci.name o1 o2 o3 o4 p0.(0) p0.(1) p3.(0) p3.(1); *)
		(o1, o2, o3, o4); 
	in
	let mo = matrix_multiply tm ci.offset in
	let cr = try Some (List.find (fun ce -> ce.name = ci.name) cells)
		with Not_found -> None in
	(match cr with 
		| Some ce -> (
			ce_updateBBX ce cells (matrix_identity 3); 
			let bbx0 = ce.bbx in
			let rec acc n m bbx = 
				if n > 1 then (
					let m1 = matrix_multiply m ci.iter in
					acc (n-1) m1 (bbxMerge (bbxTransform bbx0 m1) bbx)
				) else bbx
			in
			let bbx1 = bbxTransform bbx0 mo in
			ci.bbx <- acc ci.n mo bbx1 )
		| _ -> ()); 
	(* update the (simple) graphics *)
	ci.gr#empty ();
	ci.gr#makeRectBBX ci.bbx ; 
	ci.gr#setColor (1.0, 0.8, 0.2); 
	ci.gr#setAlpha 0.1; 
	;;
	
let rec ci_accumulate (ci:cell_instance) (gr:grfx) (cells:cell list) lay tm = 
	let cr = try Some (List.find (fun ce -> ce.name = ci.name) cells)
		with Not_found -> None in
	(match cr with 
		| Some ce -> (
			let moff = matrix_multiply tm ci.offset in
			let rec accIter mp np = 
				if np > 0 then (
					ce_accumulate ce gr cells lay mp false; 
					accIter (matrix_multiply mp ci.iter) (np - 1)
				) else ()
			in
			accIter moff ci.n); 
		| _ -> printf "Cell %s not found \n%!" ci.name; 
	)
and ce_accumulate (ce:cell) (gr:grfx) (cells:cell list) lay tm toplevel = 
	if not toplevel then (
		(*let ox,oy = bbxCenter ce.bbx in
		let mctr = matrix_identity 3 in 
		mctr.(0).(2) <- -1.0 *. ox;
		mctr.(1).(2) <- -1.0 *. oy; 
		let munctr = matrix_identity 3 in
		munctr.(0).(2) <- ox;
		munctr.(1).(2) <- oy; 
		let tmm = matrix_multiply (matrix_multiply munctr tm) mctr in*)
		let tmm = tm in
		List.iter (fun t -> 
			if t#getLayer () == lay then (
				let g = t#getGrfx () in
				let v = g#getVerts () in
				List.iter(fun (x,y) -> 
					(* matrix - column product *)
					let xx = tmm.(0).(0) *. x +. tmm.(0).(1) *. y +. tmm.(0).(2) in
					let yy = tmm.(1).(0) *. x +. tmm.(1).(1) *. y +. tmm.(1).(2) in
					gr#appendVert (xx,yy)
				) v
			)
		) ce.tracks ; 
	); 
	printf "accumulating %s layer %d n=%d ..\n%!" ce.name lay (List.length ce.cells); 
	List.iter (fun ci -> ci_accumulate ci gr cells lay tm) ce.cells
	;;
	
(* global section -- better here than in the main file. *)
let gcells : cell list ref = ref []
let gCurrentCell : cell option ref = ref None
let gAccg = Array.init 32 (fun _ -> new grfx)
	
let empty () = 
	Array.iter (fun gr -> gr#empty () ) gAccg; 
	Array.iteri (fun lay gr -> gr#updateLayer lay ) gAccg; 
	Array.iter (fun gr -> gr#updateBBX () ) gAccg; 
	;;
	
let accumulate cells = 
	List.iter (fun ce -> ce_update ce) cells ; 
	Array.iter (fun gr -> gr#empty () ) gAccg; 
	Array.iteri (fun lay gr -> 
		List.iter (fun ce -> 
			if ce.visible then 
			ce_accumulate ce gr cells lay (matrix_identity 3) true
			else ()
		) cells ; 
	) gAccg ;
	Array.iteri (fun lay gr -> gr#updateLayer lay ) gAccg; 
	Array.iter (fun gr -> gr#updateBBX () ) gAccg; 
	(* and the bbx's for the cells / instances *)
	List.iter (fun ce -> ce_updateBBX ce cells (matrix_identity 3)) cells ;
	;;
let drawInstances bbox lay = 
	(* need to sort based on current layer *)
	if lay >= 0 && lay <= 31 then (
		(gAccg.(lay))#draw bbox
	) else true
	;;
