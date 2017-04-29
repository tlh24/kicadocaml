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
  
let matrix_identity n = 
	let z = Array.make_matrix n n 0.0 in
	for i = 0 to n-1 do
		z.(i).(i) <- 1.0; 
	done; 
	z ;; 

class pcb_cell = 
object   
	val mutable m_name = ""
	val mutable m_visible = true 
	val mutable m_g = new grfx (* say, for showing the bounding box.. *)
	val mutable m_tracks : pcb_track list = []
	val mutable m_cells : string list = [] (* instances *)
	val mutable m_cells_offset : float array array list = [] 
	val mutable m_cells_n : int list = [] 
	val mutable m_cells_iter : float array array list = []
		(* transformation matrix (offset, iter) for each cell instance. *)
	
method getName () = m_name;
method setName s = m_name <- s; 
method getVisible () = m_visible;
method setVisible v = m_visible <- v;
method getCells () = m_cells ; 
method getCellsOffset () = m_cells_offset ; 
method getCellsN () = m_cells_n ; 
method getCellsIter () = m_cells_iter ; 
	
method addTrack track = (
	printf "adding track to %s\n%!" m_name;
	m_tracks <- track :: m_tracks; 
	track#update ()
)
method getTracks () = m_tracks
method filterTracks f = 
	m_tracks <- List.filter f m_tracks 

method read ic line = 
	(* starts with $CELL *)
	(* first, transformation matrix. *)
	let line2 = ref line in 
	let sp = Pcre.extract ~pat:"Cn (\w+)" !line2 in
	m_name <- sp.(1); 
	(* read in the tracks *)
	line2 := input_line2 ic ; (* eat $TRACKS *)
	while not (Pcre.pmatch ~pat:"EndTRACKS" !line2) do 
	(
		try( 
			let t = new pcb_track in
			t#read ic !line2 ; 
			m_tracks <- (t :: m_tracks) ; 
			line2 := input_line2 ic ; 
		) with _ -> line2 := input_line2 ic ; 
	)done ;
	line2 := input_line2 ic;  (* eat $CELLINSTANCE *)
	while not (Pcre.pmatch ~pat:"EndCELLINSTANCE" !line2) do 
	(
		try (
			let sp = Pcre.extract ~pat:"Ci (\w+)" !line2 in
			m_cells <- sp.(1) :: m_cells; 
			let read_matrix () = 
				line2 := input_line2 ic ; 
				let mat = matrix_identity 3 in
				for j = 0 to 2 do 
					let sp = Pcre.extract ~pat:"Tm ([\.\d-]+) ([\.\d-]+) ([\.\d-]+)" !line2 in
					for i = 0 to 2 do
						mat.(j).(i) <- fos sp.(1+i) ; 
					done ;
					line2 := input_line2 ic ;
				done;
				mat 
			in
			m_cells_offset <- read_matrix () :: m_cells_offset; 
			let sp = Pcre.extract ~pat:"Ni ([\.\d-]+)" !line2 in
			m_cells_n <- (ios sp.(1)) :: m_cells_n; 
			m_cells_iter <- read_matrix () :: m_cells_iter; 
		) with _ -> line2 := input_line2 ic ; 
	) done ;

method save oc = (
	fprintf oc "Cn %s\n" m_name; 
	fprintf oc "$TRACKS\n";
	List.iter (fun t -> t#save oc) m_tracks; 
	fprintf oc "$EndTRACKS\n"; 
	fprintf oc "$CELLINSTANCE\n"; 
	let rec print_instances cells offset n iter = 
		match cells with 
		| hd :: _ -> (
			fprintf oc "Ci %s\n" hd; 
			let print_matrix m = 
				for j = 0 to 2 do
					fprintf oc "Tm %f %f %f\n" m.(j).(0) m.(j).(1) m.(j).(2); 
				done;
			in
			print_matrix (List.hd offset); 
			fprintf oc "Ni %d\n" (List.hd n); 
			print_matrix (List.hd iter) ;
			print_instances (List.tl cells) (List.tl offset) (List.tl n) (List.tl iter); )
		| _ -> ()
	in
	print_instances (List.rev m_cells) (List.rev m_cells_offset) 
		(List.rev m_cells_n) (List.rev m_cells_iter); 
	fprintf oc "$EndCELLINSTANCE\n"; 
)
	
method draw bbx lay = (
	if m_visible then (
		List.iter (fun t -> 
			if(t#getLayer() == lay) then
				t#draw bbx
		) m_tracks; 
	);
)	
method update () = (
	List.iter (fun t -> 
		t#update ()
	) m_tracks; 
) 

method hit (p, onlyworknet, netn_, hitsize_, hitz_, hitclear_) = (
	if m_visible then 
	List.fold_left (fun (netn, hitsize, hitz, hitclear) t -> 
		t#hit (p, onlyworknet, netn, hitsize, hitz, hitclear)
	) (netn_, hitsize_, hitz_, hitclear_) m_tracks
	else (netn_, hitsize_, hitz_, hitclear_)
)
end

(* global section -- better here than in the main file. *)
let gcells : pcb_cell list ref = ref []
let gCurrentCell : pcb_cell option ref = ref None
let gAccg = Array.init 32 (fun _ -> new grfx)


let rec accumulate ce (gr:grfx) lay tm (cells : pcb_cell list) toplevel = 
	(* iterate over the cells, accumulating the vertex information if the layers match. *)
	(* that is, there is one accumulator per layer at the toplevel. *)
	(* cells is a toplevel list of all cells. *)
	if not toplevel then (
		List.iter (fun t -> 
			if t#getLayer () == lay then (
				let g = t#getGrfx () in
				let v = g#getVerts () in
				List.iter(fun (x,y) -> 
					(* matrix - column product *)
					let xx = tm.(0).(0) *. x +. tm.(0).(1) *. y +. tm.(0).(2) in
					let yy = tm.(1).(0) *. x +. tm.(1).(1) *. y +. tm.(1).(2) in
					gr#appendVert (xx,yy)
				) v
			)
		) (ce#getTracks ()) ; 
	); (* otherwise, the tracks are drawn conventionally *)
	printf "accumulating %s layer %d n=%d ..\n%!" (ce#getName ()) lay (List.length (ce#getCells ())); 
	let rec accCells cell offset n iter = 
		(match cell with 
		| ci :: _ -> (
			printf "cell instance %s\n%!" ci; 
			let m_off = matrix_multiply tm (List.hd offset) in
			let cr = try Some (List.find (fun a -> a#getName () = ci) cells)
				with Not_found -> None in
			match cr with 
			| Some cv -> (
				printf "found.\n%!"; 
				let rec accIter mp np = 
					if np > 0 then (
						accumulate cv gr lay mp cells false; 
						accIter (matrix_multiply mp (List.hd iter)) (np - 1)
					) else ()
				in
				accIter m_off (List.hd n); 
				accCells (List.tl cell) (List.tl offset) (List.tl n) (List.tl iter) )
			| _ -> () )
		| _ -> () )
	in
	accCells (ce#getCells()) (ce#getCellsOffset()) (ce#getCellsN()) (ce#getCellsIter()); 
	;;

(* accumulator -- try to draw lots of polygons fast! *)
let accumulateInstances cells = 
	Array.iter (fun gr -> gr#empty () ) gAccg; 
	Array.iteri (fun lay gr -> 
		List.iter (fun ce -> 
			if ce#getVisible () then 
			accumulate ce gr lay (matrix_identity 3) cells true
			else ()
		) cells ; 
	) gAccg ;
	Array.iteri (fun lay gr -> gr#updateLayer lay ) gAccg; 
	Array.iter (fun gr -> gr#updateBBX () ) gAccg; 
	;;

let drawInstances bbox lay = 
	(* need to sort based on current layer *)
	if lay >= 0 && lay <= 31 then (
		(gAccg.(lay))#draw bbox
	) else true
	;;

