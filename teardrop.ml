 (* teardrop all vias on the board *)
open Track
open Printf
open Drc
open Comm

let teardrop intracks modules = 
	let newtracks = ref [] in
	(* iterate over all tracks, select the vias *)
	let vias,tracks = List.partition (fun t -> t#getType () = Track_Via) intracks in
	List.iter (fun v -> 
		(* find all tracks that are connected to this via *)
		let p = v#getStart () in
		let r = (v#getWidth ()) /. 2.0 in
		let net = v#getNet () in
		List.iter (fun t -> 
			let st = t#getStart () in
			let en = t#getEnd () in
			let w = (t#getWidth()) /. 2.0 in
			let lay = t#getLayer () in
			let tdrop s e = 
				(* teardrop the start *)
				let d = Pts2.sub e s in
				(* figure out an appropriate teardrop length *)
				let l = (r *.(r -. w))/.(r -. 2.0 *. w) in (* small angle approx *)
				let l2 = min l (2.0 *. r) in
				let d2 = Pts2.scl (Pts2.norm d) l2 in
				let x = (snd d2),(fst d2)*.(-1.0) in (* rotated *)
				let x2 = Pts2.scl (Pts2.norm x) (r -. w) in
				let addtrack d3 x3 = 
					let track = new pcb_track in
					track#setStart (Pts2.add p x3); 
					track#setEnd (Pts2.add p d3); 
					track#setNet net; 
					track#setWidth (w *. 2.0); 
					track#setLayer lay ; 
					track#update (); 
					(* check to see if this will fit *)
					if not (testdrc2 track intracks modules) then (
						newtracks := track :: !newtracks; 
					)
				in
				addtrack d2 x2; 
				addtrack d2 (Pts2.scl x2 (-1.0)); 
			in
			if Pts2.distance st en > r && r > 2.4 *. w then (
				if Pts2.distance p st < r then tdrop st en;
				if Pts2.distance p en < r then tdrop en st; 
			); 
		) tracks; 
	) vias ; 
	!newtracks
	;; 
	
let unteardrop intracks = 
	let vias,tracks = List.partition (fun t -> t#getType () = Track_Via) intracks in
	(* use the visible flag to mark tracks for removal *)
	List.iter (fun t -> t#setVisible true) tracks; 
	(* mark any track that hits a via and is too short *)
	List.iter (fun v -> 
		(* find all tracks that are connected to this via *)
		let p = v#getStart () in
		let r = (v#getWidth ()) /. 2.0 in
		List.iter (fun t -> 
			let st = t#getStart () in
			let w = (t#getWidth()) /. 2.0 in
			let check a = 
				let d = Pts2.distance p a in
				let d2 = d -. (r -. w) in (* r-w is where the teardrops should be *)
				fabs d2 < 0.002 
			in
			(* the starts will always be in the via *)
			if check st then t#setVisible false
		) tracks; 
	) vias; 
	List.filter (fun t-> t#getVisible ()) intracks
	;;