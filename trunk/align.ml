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
open Comm
open Tk

(* module for performing alignment operations on a series of pcb modules *)

let center mods = 
	let (sx,sy) = List.fold_left (fun (x,y) m ->
		let (cx,cy)= m#getCenter false in
		(cx +. x), (cy +. y)
	) (0.0, 0.0) mods in
	let len = foi (List.length mods) in
	(sx /. len, sy /. len)
	;;
let alignX mods = 
	(* align vertically *)
	let cx, _ = center mods in
	List.iter (fun m -> 
		(* calculate how much to move .. *)
		let x, _ = m#getCenter false in
		m#move ( cx -. x, 0.0); 
		m#setMoving false ; 
	) mods 
	;;
let alignY mods = 
	(* align vertically *)
	let _, cy = center mods in
	List.iter (fun m -> 
		(* calculate how much to move .. *)
		let _, y = m#getCenter false in
		m#move ( 0.0, cy -. y ); 
		m#setMoving false ; 
	) mods 
	;;
	
let distribute mods pack unpack = 
	(* distribute the y-centers vertically. *)
	let cl = List.map (fun m -> 
		let c = pack (m#getCenter false) in
		(c, m)
	) mods in
	let cs = List.sort (fun (a,_) (b,_) -> compare_float a b) cl in
	let len = (foi (List.length mods)) -. 1.0 in
	let sta,_ = List.hd cs in
	let fin,_ = List.hd (List.rev cs) in
	let q = ref 0.0 in
	q := sta ; 
	List.iter (fun (c,m) -> 
		m#move ( unpack( !q -. c ) ); 
		m#setMoving false ; 
		q := !q +. ( (fin -. sta) /. len ) ;
	) cs
	;;
	
let distributeX mods = distribute mods fst (fun a -> (a, 0.0) )
	;;
	
let distributeY mods = distribute mods snd (fun a -> (0.0, a) )
	;;