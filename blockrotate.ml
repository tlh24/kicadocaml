open Printf
open Comm
open Mod
open Track

(* rotate a block of items - tracks and modules - about their collective center *)
(* note note! you cannot drag and rotate at the same time -- 
this is rooted in the way things are moved (w/ an extra variable, m_move) - 
perhaps we should do away with this extraneous variable (which allows us to use
fast OpenGL transformations), and focus on proper data representation? *)

let rotate mods tracks = 
	(* first apply the move so far... *)
	
	(* next figure out the collective center *)
	let bbx0 = if List.length mods > List.length tracks then (
		(List.hd mods)#getBBX false;  
	) else (
		(List.hd tracks)#getDrcBBX(); 
	) in
	let bbx1 = List.fold_left 
		(fun bbx t -> bbxMerge (t#getDrcBBX()) bbx) 
		bbx0 tracks in 
	let bbx2 = List.fold_left 
		(fun bbx m -> bbxMerge (m#getBBX false) bbx) 
		bbx1 mods in 
	let center = bbxCenter bbx2 in
	printf "blockrotate: center @ %f %f\n%!" (fst center) (snd center); 
	(* iterate over the modules, rotate them about their centers
	then rotate about the collective center *)
	let cnt = ref 0 in
	List.iter (fun m -> 
		m#rotate () ; 
		m#move (Pts2.rotateMove (Pts2.sub (m#getCenter false) center) );
		m#setMoving false ; (* to apply the move *)
		incr cnt; 
	) mods ; 
	(* do the same for the tracks *)
	List.iter (fun t -> 
		t#setU 0.0 ; 
		t#move (Pts2.rotateMove (Pts2.sub (t#getStart ()) center) ); 
		t#applyMove (); 
		if t#getType () = Track_Track then (
			t#setU 1.0 ; 
			t#move (Pts2.rotateMove (Pts2.sub (t#getEnd ()) center) ); 
			t#applyMove (); 
		); 
		t#update (); 
	) tracks ; 
	;;