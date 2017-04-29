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
		m#rotate (-1e26,-1e26) ; (* to be sure no pads are hit *)
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
	
(* mirror a block of items -- only tracks, does not make sense for modules! *)

let mirror tracks vertical = 
	(* get collective center. *)
	if (List.length tracks) > 0 then (
		let bbx0 = (List.hd tracks)#getDrcBBX() in
		let bbx1 = List.fold_left 
			(fun bbx t -> bbxMerge (t#getDrcBBX()) bbx) 
			bbx0 tracks in 
		let cx, cy = bbxCenter bbx1 in
		printf "blockmirror: center @ %f %f\n%!" cx cy; 
		let flip (x,y) = (* this just calculates the move *)
			if vertical then (
				0.0, (2.0 *. (cy -. y))
			) else (
				(2.0 *. (cx -. x)), 0.0
			)
		in
		List.iter (fun t -> 
			t#setU 0.0 ; 
			t#move (flip (t#getStart ())); 
			t#applyMove (); 
			if t#getType () = Track_Track then (
				t#setU 1.0 ; 
				t#move (flip (t#getEnd ())); 
				t#applyMove (); 
			); 
			t#update (); 
		) tracks ; 
	) else (
	 printf "mirror: no tracks!\n%!"
	)
	
let scale tracks sclf = 
	if (List.length tracks) > 0 then (
		let bbx0 = (List.hd tracks)#getDrcBBX () in
		let bbx1 = List.fold_left 
			(fun bbx t -> bbxMerge (t#getDrcBBX()) bbx) 
			bbx0 tracks in 
		let center = bbxCenter bbx1 in
		printf "blockscale: center @ %f %f\n%!" (fst center) (snd center); 
		(* iterate over tracks, scale the endpoints, scale the width. *)
		List.iter (fun t -> 
			let doscl a = Pts2.add (Pts2.scl (Pts2.sub a center) sclf) center in
			let st2 = doscl (t#getStart ()) in
			let en2 = doscl (t#getEnd ()) in
			let w2 = (t#getWidth ()) *. sclf in
			t#setStart st2 ; 
			t#setEnd en2 ; 
			t#setWidth w2 ; 
			t#update (); 
		) tracks; 
	)
