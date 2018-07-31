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

(* ocaml indexing: a.(row).(col) -- C-style, read-style. *) 
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
	for j = 0 to 2 do (* row *)
		let sp = Pcre.extract ~pat:"Tm ([\.\d-]+) ([\.\d-]+) ([\.\d-]+)" !line2 in
		for i = 0 to 2 do (* column *)
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
	mutable level : int ; 
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
		level = 0; 
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
		ci.gr#setAlpha 0.25; 
		ci.gr#setColor (1.0, 0.6, 0.4);
	) else (
		ci.gr#setAlpha 0.15; 
		if ci.hit then ci.gr#setColor (1.0, 0.3, 0.0)
		else ci.gr#setColor (0.0, 0.3, 0.8);
	); 
	ignore(ci.gr#draw bbx ~force:ci.moving); 
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
	(* the original cell must be visible *)
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
	line2 := input_line2 ic ; (* eat $TRACKS *)
	while not (Pcre.pmatch ~pat:"EndTRACK" !line2) do 
	(
		let t = new pcb_track in
		t#read ic !line2 ; 
		ce.tracks <- (t :: ce.tracks) ; 
		line2 := input_line2 ic ; 
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
	ce.tracks <- List.rev ce.tracks; 
	ce.cells <- List.rev ce.cells; 
	ce.visible <- false; (* default to invisible. *)
	ce
	;;
let ce_save ce oc = 
	fprintf oc "Cn %s\n" ce.name; 
	fprintf oc "$TRACKS\n";
	List.iter (fun t -> t#save oc) ce.tracks; 
	fprintf oc "$EndTRACKS\n"; 
	fprintf oc "$CELLINSTANCE\n"; 
	List.iter (fun ci -> ci_save ci oc) ce.cells; 
	fprintf oc "$EndCELLINSTANCE\n"; 
	;;
let ce_draw ce bbx lay firstLayer = 
	if ce.visible then (
		List.iter (fun t -> 
			if(t#getLayer() == lay) then
				t#draw bbx
		) ce.tracks; 
		if lay = firstLayer && !gmode = Mode_MoveCell then 
			List.iter (fun ci -> ci_draw ci bbx) ce.cells ; 
	);
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
let ce_prinfo ce printfn = 
	printfn ("Cell " ^ (ce.name) ^ "; tracks " ^ (soi (List.length ce.tracks)) ^ 
		"; cell instances " ^ (soi (List.length ce.cells))); 
	(List.length ce.tracks)
	;;
let ce_hit ce (p, onlyworknet, netn_, hitsize_, hitz_, hitclear_) = 
	if ce.visible then (
		let (nn,hitsize2,hitz2,hitclear2) = 
		List.fold_left (fun (netn, hitsize, hitz, hitclear) t -> 
			t#hit (p, onlyworknet, netn, hitsize, hitz, hitclear)
		) (netn_, hitsize_, hitz_, hitclear_) ce.tracks in
		(* see if we hit any (immediate, first-level) instances *)
		(* hitting deeper nodes doesn't make sense, too confusing to edit.*)
		if !gmode = Mode_MoveCell then (
			List.fold_left (fun (netn, hitsize, hitz, hitclear) ci -> 
				ci_hit ci (p, netn, hitsize, hitz, hitclear)
			) (nn, hitsize2, hitz2, hitclear2) (ce.cells)
		) else (nn, hitsize2, hitz2, hitclear2)
	) else (netn_, hitsize_, hitz_, hitclear_)
	;;
	
let ce_ci_hit_info ce info = 
  (* returns a string describing the cell that was hit -- if any. *)
  if ce.visible then (
		if !gmode = Mode_MoveCell then (
			List.fold_left (fun nfo ci -> 
        if ci.hit then ce.name ^ "/" ^ ci.name else nfo
			) info (ce.cells)
		) else info
	) else info
	
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
	
let trapezoidal_track t transfn = 
	let (sx,sy) = t#getStart () in
	let stw = t#getWidthSt () in
	let ex,ey = t#getEnd () in
	let enw = t#getWidthEn () in 
	let shape = t#getShape () in
	let dx = ex -. sx in
	let dy = ey -. sy in
	let len = sqrt(dx *. dx +. dy *. dy) in
	let (nx, ny) = ( dx /. len, dy /. len) in (*normalized line between them.*)
	let (mx, my) = (-1. *. ny , nx) in (*rotate pi/2 ccw *)
	let n = if shape = 0 then 16 else 2 in
	let t = ref (pi /. 2.0) in (* 90 deg, vertical.. *)
	let dt = pi /. foi(n-1) in
	let pnt t x y w = ( x +. cos(t)*.nx*.w +. sin(t)*.mx*.w, 
							y +. cos(t)*.ny*.w +. sin(t)*.my*.w) in
	let endcap sta en x y w = 
		for i = sta to en do (
			transfn (pnt !t x y w) ;
			t := !t +. dt; 
		)done; 
	in
	let stw2 = stw /. 2.0 in
	let enw2 = enw /. 2.0 in 
	(* rounded end cap @ start *)
	endcap 1 n sx sy stw2; 
	(* rounded end cap @ end *)
	t := pi /. -2.0; 
	endcap 1 n ex ey enw2; 
	;;
	
let ce_print_track (t:pcb_track) tmm gds2 gerber = 
  let transform (x,y) = 
    (* matrix - column product *)
    (tmm.(0).(0) *. x +. tmm.(0).(1) *. y +. tmm.(0).(2) , 
    tmm.(1).(0) *. x +. tmm.(1).(1) *. y +. tmm.(1).(2) )
  in
  let st = t#getStart () in
  let stw = t#getWidthSt () in
  let en = t#getEnd () in
  let enw = t#getWidthEn () in 
  let shape = t#getShape () in
  let lay = t#getLayer () in
  if glayerEn.(lay) then (
    (match gds2 with 
    | Some oc -> (
      let transform2 p scl = 
        let x,y = transform p in
        (* GDS units are nm *)
        let xx,yy = (iof (x *. scl)), (iof (y *. -1.0 *. scl)) in
          fprintf oc "%d: %d\n" xx yy; 
      in
      if stw = enw then (
        fprintf oc "PATH\n"; 
        fprintf oc "LAYER %d\n" lay; 
        fprintf oc "DATATYPE 0\n"; 
        fprintf oc "PATHTYPE %d\n" (if shape = 0 then 1 else 0); 
        fprintf oc "WIDTH %d\n" (iof (stw *. 1000000.0)); 
        fprintf oc "XY "; 
        transform2 st 1e6; 
        transform2 en 1e6; 
        fprintf oc "ENDEL\n\n"; 
      ) else ( 
      (* polygon output; have to duplicate code here as opengl order is different . *)
        fprintf oc "BOUNDARY\n"; 
        fprintf oc "LAYER %d\n" lay; 
        fprintf oc "DATATYPE 0\n"; 
        fprintf oc "XY "; 
        trapezoidal_track t (fun p -> transform2 p 1e6);
        fprintf oc "ENDEL\n\n"; 
      ); )
    | _ -> ()
    );
    (match gerber with 
    | Some (oc,apertures,globscl) -> (
      (* this is not efficient -- switches tool for each track.  eh, zip will compress it away *)
      let cnvt d = round (d *. 100000.0 *. globscl) in
      let gerbPrint x y flashcode = 
        let six = if x < 0.0 then "-" else "" in
        let siy = if y > 0.0 then "-" else "" in (*flip vertical axis .. not sure why.*)
        fprintf oc "X%s%07dY%s%07dD%s*\n" 
          six (cnvt (fabs x)) siy (cnvt (fabs y)) flashcode;
      in
      let getAper whr = 
        let w,h,r = whr in
        let aper = try Hashtbl.find apertures whr 
          with _ -> 
            printf "whr %f %f %d not found!\n" w h r; 
            10 in
        aper
      in
      let gerbTrack (sx,sy) (ex,ey) whr = 
        let aper =getAper whr in
        fprintf oc "G54D%d*\n" aper ; 
        gerbPrint sx sy "02"; 
        gerbPrint ex ey "01"; 
      in
      let (w,h,r) = t#getWHR () in
      (match r with 
      | 0 -> gerbTrack (transform st) (transform en) (w,h,r)
      | 1 -> (
        let aper = getAper (w,h,r) in
        fprintf oc "G54D%d*\n" aper ; 
        let mx,my = Pts2.scl (Pts2.add (transform st) (transform en)) 0.5 in
        gerbPrint mx my "03" )
      | _ -> (
        fprintf oc "G36*\n"; 
        let lst = ref [] in
        trapezoidal_track t (fun p -> 
          lst := (transform p) :: !lst); 
        let l2 = List.rev !lst in
        let sx,sy = List.hd l2 in 
        gerbPrint sx sy "02";
        fprintf oc "G01*\n"; 
        List.iter(fun (x,y) -> 
          gerbPrint x y "01"
        ) (List.tl l2); 
        gerbPrint sx sy "01";
        fprintf oc "G37*\n"; 
      )))
    | _ -> ()
    ); 
  );;
  
let rec ci_accumulate (ci:cell_instance) (gr:grfx) (cells:cell list) lay tm gds2 gerber = 
	let cr = try Some (List.find (fun ce -> ce.name = ci.name) cells)
		with Not_found -> None in
	(match cr with 
		| Some ce -> (
			let moff = matrix_multiply tm ci.offset in
			let rec accIter mp np = 
				if np > 0 then (
					ce_accumulate ce gr cells lay mp false gds2 gerber; 
					accIter (matrix_multiply mp ci.iter) (np - 1)
				) else ()
			in
			accIter moff ci.n); 
		| _ -> printf "Cell %s not found \n%!" ci.name; 
	)
and ce_accumulate (ce:cell) (gr:grfx) (cells:cell list) lay tm toplevel gds2 gerber = 
	(*let ox,oy = bbxCenter ce.bbx in
	let mctr = matrix_identity 3 in 
	mctr.(0).(2) <- -1.0 *. ox;
	mctr.(1).(2) <- -1.0 *. oy; 
	let munctr = matrix_identity 3 in
	munctr.(0).(2) <- ox;
	munctr.(1).(2) <- oy; 
	let tmm = matrix_multiply (matrix_multiply munctr tm) mctr in*)
	let transform (x,y) = 
    (* matrix - column product *)
    (tm.(0).(0) *. x +. tm.(0).(1) *. y +. tm.(0).(2) , 
    tm.(1).(0) *. x +. tm.(1).(1) *. y +. tm.(1).(2) )
  in
  let tmm = tm in (* copy. *)
	List.iter (fun t -> 
    if t#getLayer () = lay then (
      ce_print_track t tmm gds2 gerber; 
      if glayerEn.(lay) then (
        if not toplevel then (
          (* tracks are already displayed, editable. *)
          let g = t#getGrfx () in
          let v = g#getVerts () in
          List.iter (fun p -> 
            gr#appendVert (transform p)
          ) v ;
        ); 
      );
    )
	) ce.tracks ; 
	List.iter (fun ci -> ci_accumulate ci gr cells lay tm gds2 gerber) ce.cells
	;;
	
let ci_save_gds2 (ci:cell_instance) gds2 = 
  fprintf gds2 "AREF\n"; 
  fprintf gds2 "SNAME %s\n" ci.name; 
  fprintf gds2 "MAG 1\n"; 
  fprintf gds2 "ANGLE 0\n"; 
  fprintf gds2 "COLROW %d 0\n" ci.n; 
  (* following data appears to be vectors -- origin, col vec, row vec. *)
  (* these seem to start from inital translation spot. *)
  let trnsfrm a = iof (a *. 1e6) in
  let n = foi ci.n in
  let ox,oy = (trnsfrm ci.offset.(0).(2)), (trnsfrm ci.offset.(1).(2)) in
  let vx,vy = (trnsfrm (ci.iter.(0).(2)*.n)), (trnsfrm (ci.iter.(1).(2)*.n)) in
  fprintf gds2 "XY %d: %d\n" ox oy; 
  fprintf gds2 "%d: %d\n" (ox+vx) (oy+vy); 
  fprintf gds2 "%d: %d\n" (ox-vy) (oy+vx); (* rotate 90deg ccw *) 
  fprintf gds2 "ENDEL\n\n";
  ;;
	
let ce_save_gds2 (ce:cell) gds2 = 
  fprintf gds2 "BGNSTR\n"; (* structure or string?  not sure. *)
  fprintf gds2 "STRNAME %s\n\n" ce.name; 
  List.iter (fun ci -> 
    ci_save_gds2 ci gds2
  ) ce.cells; 
  let tmm = matrix_identity 3 in
  List.iter (fun t -> 
    ce_print_track t tmm (Some gds2) None ; 
  ) ce.tracks ; 
  fprintf gds2 "ENDSTR\n\n"; 
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
	
let accumulate cells gds2 gerber = 
	List.iter (fun ce -> ce_update ce) cells ; 
	Array.iter (fun gr -> gr#empty () ) gAccg; 
	Array.iteri (fun lay gr -> 
		List.iter (fun ce -> 
			if ce.visible then 
			ce_accumulate ce gr cells lay (matrix_identity 3) true gds2 gerber
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
let updateLayers () = 
	Array.iteri (fun lay gr -> gr#updateLayer lay ) gAccg; 
	;;
let ci_dialog (ci:cell_instance) top dispCB = 
  let dlog = Toplevel.create top in
  Wm.title_set dlog (ci.name^" - cell instance");
  
  let off_label = Message.create dlog ~width:200 
    ~text:"Offset matrix (initial transformation)" in
  let off_frame = Frame.create dlog in
  let off_vframe = Array.init 3 (fun _ -> 
    Frame.create off_frame) in
  let off_entries = Array.make_matrix 3 3 (Entry.create dlog) in
  for i = 0 to 2 do
    for j = 0 to 2 do
      let ent = Entry.create ~width:10 off_vframe.(i) in
      Entry.insert ~index:(`Num 0) ~text:(sof ci.offset.(j).(i)) ent; 
      off_entries.(j).(i) <- ent; 
    done; 
    Tk.pack ~fill:`Both ~expand:true ~side:`Top 
      [Tk.coe off_entries.(0).(i);Tk.coe off_entries.(1).(i);Tk.coe off_entries.(2).(i)]; 
  done;
  Tk.pack ~fill:`Both ~expand:true ~side:`Left 
    [Tk.coe off_vframe.(0);Tk.coe off_vframe.(1);Tk.coe off_vframe.(2)]; 
    
  let iter_label = Message.create dlog ~width:200 
    ~text:"Iteration matrix (sequential transformation)" in
  let iter_frame = Frame.create dlog in
  let iter_vframe = Array.init 3 (fun _ -> 
    Frame.create iter_frame) in
  let iter_entries = Array.make_matrix 3 3 (Entry.create dlog) in
  for i = 0 to 2 do
    for j = 0 to 2 do
      let ent = Entry.create ~width:10 iter_vframe.(i) in
      Entry.insert ~index:(`Num 0) ~text:(sof ci.iter.(j).(i)) ent; 
      iter_entries.(j).(i) <- ent; 
    done; 
    Tk.pack ~fill:`Both ~expand:true ~side:`Top 
      [Tk.coe iter_entries.(0).(i);Tk.coe iter_entries.(1).(i);Tk.coe iter_entries.(2).(i)]; 
  done;
  Tk.pack ~fill:`Both ~expand:true ~side:`Left 
    [Tk.coe iter_vframe.(0);Tk.coe iter_vframe.(1);Tk.coe iter_vframe.(2)]; 
    
  let n_frame = Frame.create dlog in
  let n_msg = Message.create n_frame ~width:140 ~text:"Number of copies" in
  let n_entry = Entry.create n_frame ~width:10 in
  Entry.insert ~index:(`Num 0) ~text:(soi ci.n) n_entry; 
  Tk.pack ~fill:`Both ~expand:true ~side:`Left [Tk.coe n_msg;Tk.coe n_entry]; 
    
  let apply_button = Button.create dlog ~text:"Apply"
  ~command:( fun () -> 
    for i = 0 to 2 do
      for j = 0 to 2 do
        ci.offset.(j).(i) <- fos (Entry.get off_entries.(j).(i));
        ci.iter.(j).(i) <- fos (Entry.get iter_entries.(j).(i)); 
      done; 
    done; 
    ci.n <- ios (Entry.get n_entry); 
    dispCB () ); in 
    
  Tk.pack ~fill:`Both ~expand:true ~side:`Top 
    [Tk.coe off_label; Tk.coe off_frame; Tk.coe iter_label; Tk.coe iter_frame; 
    Tk.coe n_frame; Tk.coe apply_button]; 
  ;;

let ce_dialog (ce:cell) top dispCB = 
  let dlog = Toplevel.create top in
  Wm.title_set dlog (ce.name^" - cell");
  let buttons = List.map (fun ci -> 
    Button.create dlog ~text:ci.name 
      ~command:(fun () -> ci_dialog ci top dispCB )
    ) ce.cells in
  printf "cells dialog %d subs\n%!" (List.length ce.cells); 
  Tk.pack ~fill:`Both ~expand:true ~side:`Top 
    (List.map Tk.coe buttons); 
  ;;
