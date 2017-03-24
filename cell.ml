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
	val mutable m_tm = matrix_identity 3
	val mutable m_g = new grfx (* say, for showing the bounding box.. *)
	val mutable m_tracks : pcb_track list = []
	val mutable m_cells : string list = [] (* instances *)
	
method getName () = m_name;
method setName s = m_name <- s; 
method getVisible () = m_visible;
method setVisible v = m_visible <- v;
	
method addTrack track = (
	m_tracks <- track :: m_tracks
)

method read ic line = 
	(* starts with $CELL *)
	(* first, transformation matrix. *)
	for j = 0 to 2 do 
		let sp = Pcre.extract ~pat:"Tm ([\.\d-]+) ([\.\d-]+) ([\.\d-]+)" line in
		for i = 0 to 2 do
			m_tm.(j).(i) <- fos sp.(1+i) ; 
		done
	done;
	let line2 = ref (input_line2 ic) in (* $TRACKS *)
	(* read in the tracks *)
	while not (Pcre.pmatch ~pat:"\$EndTRACKS" !line2) do 
	(
		let t = new pcb_track in
		t#read ic !line2 ; 
		m_tracks <- (t :: m_tracks) ; 
		line2 := input_line2 ic ; 
	)done ;
	line2 := input_line2 ic;  (* $CELLINSTANCE *)
	while not (Pcre.pmatch ~pat:"\$EndCELLINSTANCE" !line2) do 
	(
		let t = new pcb_track in
		t#read ic !line2 ; 
		m_tracks <- (t :: m_tracks) ; 
		line2 := input_line2 ic ; 
	)done ;

method accumulate (gr:grfx) lay tm (cells : pcb_cell list) = 
	(* iterate over the cells, accumulating the vertex information if the layers match. *)
	(* that is, there is one accumulator per layer at the toplevel. *)
	let m = matrix_multiply tm m_tm in
	List.iter (fun t -> 
		if t#getLayer == lay then (
			let g = t#getGrfx () in
			let v = g#getVerts () in
			List.iter(fun (x,y) -> 
				(* matrix - column product *)
				let xx = m.(0).(0) *. x +. m.(0).(1) *. y +. m.(0).(2) in
				let yy = m.(1).(0) *. x +. m.(1).(1) *. y +. m.(1).(2) in
				gr#appendVert (xx,yy)
			) v
		)
	) m_tracks ; 
	List.iter (fun ci -> 
		let cr = try Some (List.filter (fun a -> a#getName () == ci) cells)
			with Not_found -> None in
		match cr with 
		| Some (hd::_) -> hd#accumulate gr lay m cells
		| _ -> ()
	) m_cells ;
	
method draw bbx = (
	if m_visible then (
		List.iter (fun t -> 
			t#draw bbx
		) m_tracks; 
	); 
)
	
end
