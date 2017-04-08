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
	val mutable m_cells_tm : float array array list = [] 
		(* transformation matrices for each cell instance. *)
	
method getName () = m_name;
method setName s = m_name <- s; 
method getVisible () = m_visible;
method setVisible v = m_visible <- v;
	
method addTrack track = (
	printf "adding track to %s\n%!" m_name;
	m_tracks <- track :: m_tracks
)
method getTracks () = m_tracks

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
			line2 := input_line2 ic ; 
			let mat = matrix_identity 3 in
			for j = 0 to 2 do 
				let sp = Pcre.extract ~pat:"Tm ([\.\d-]+) ([\.\d-]+) ([\.\d-]+)" !line2 in
				for i = 0 to 2 do
					mat.(j).(i) <- fos sp.(1+i) ; 
				done ;
				line2 := input_line2 ic ;
			done;
			m_cells_tm <- mat :: m_cells_tm;
		) with _ -> line2 := input_line2 ic ; 
	) done ;

method save oc = (
	fprintf oc "Cn %s\n" m_name; 
	fprintf oc "$TRACKS\n";
	List.iter (fun t -> t#save oc) m_tracks; 
	fprintf oc "$EndTRACKS\n"; 
	fprintf oc "$CELLINSTANCE\n"; 
	List.iter2 (fun s m -> 
		fprintf oc "Ci %s\n" s; 
		for j = 0 to 2 do
			fprintf oc "Tm %f %f %f\n" m.(j).(0) m.(j).(1) m.(j).(2); 
		done;
	) m_cells m_cells_tm; 
	fprintf oc "$EndCELLINSTANCE\n"; 
)

method accumulate (gr:grfx) lay tm (cells : pcb_cell list) toplevel = 
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
		) m_tracks ; 
	); 
	List.iter2 (fun ci mi -> 
		let cr = try Some (List.filter (fun a -> a#getName () == ci) cells)
			with Not_found -> None in
		match cr with 
		| Some (hd::_) -> 
			let m = matrix_multiply tm mi in
			hd#accumulate gr lay m cells false
		| _ -> ()
	) m_cells m_cells_tm;
	
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
	List.fold_left (fun (netn, hitsize, hitz, hitclear) t -> 
		t#hit (p, onlyworknet, netn, hitsize, hitz, hitclear)
	) (netn_, hitsize_, hitz_, hitclear_) m_tracks
)
end
	
(* accumulator -- try to draw lots of polygons fast *)

let accumulateAll (cells : pcb_cell list) = 
	let accg = Array.init 32 (fun i -> 
		let gr = new grfx in 
		List.iter (fun c -> 
			c#accumulate gr i (matrix_identity 3) cells true
		) cells ; 
		gr) in
	accg; 
