(* tries to "pull" available modules toward a selectetd component 
based on net connectivity *)

open Printf
open Comm
open Pad
open Mod
open Track
open Ratnest

let filtermods modules tracks = (
	(* return all modules that do not have a track attached to one of their pins *)
	(* or are not hit *)
	printf "Pull.filtermods: input %d modules %d tracks\n%!" 
		(List.length modules) (List.length tracks);
	let trx = List.rev_append 
		(List.rev_map (fun t -> t#getStart(), t#getLayer()) tracks)
		(List.rev_map (fun t -> t#getEnd(), t#getLayer()) tracks) in
	List.filter (fun m -> 
		not ((List.exists (fun p-> 
			let c = p#getCenter() in 
			let sx,sy = bbxWH (p#getBBX ()) in
			let siz = (min sx sy)/. 2.0 in
			let s = siz *. siz in
			let layers = p#getLayers() in
			List.exists (fun (tc,lay)-> 
				if (Pts2.distance2 tc c) < s then
					(List.exists (fun a -> a=lay) layers)
				else false
			) trx
		) (m#getPads()) ) || (m#getHit()))
	) (List.filter (fun m -> 
		List.length (m#getPads()) < 5
	) modules)
) ;;

let pull magnet modules = (
	(* move the modules in list toward magnet if they share nets *)
	(* first get a set of all nets -- don't move based on large nets, e.g. gnd *)
	let h = Hashtbl.create 200 in
	List.iter (fun m -> 
		List.iter (fun p -> 
			let n = p#getNet () in
			let cnt = try Hashtbl.find h n with _ -> 0 in
			Hashtbl.replace h n (cnt+1)
		) (m#getPads())
	) modules; 
	List.iter (fun m-> 
		let vec = ref (0.0, 0.0) in
		let mov = m#getMove () in
		List.iter (fun p->
			let n = p#getNet () in
			if n > 0 then (
				List.iter (fun p2 -> 
					let cnt = try Hashtbl.find h n with _ -> 0 in
					if n = (p2#getNet()) && cnt < 40 then (
						let ctr = Pts2.add (p#getCenter()) mov in
						let v = Pts2.sub (p2#getCenter()) ctr in
						vec := Pts2.add v !vec; 
					)
				) (magnet#getPads())
			)
		) (m#getPads()) ; 
		(* move the module, saturating *)
		let len = Pts2.length !vec in
		if len > 0.0 then (
			let v2 = Pts2.scl !vec (0.03 /. (max len 1.0)) in
			if not (m#getMoving()) then m#setMoving true; 
			m#moveSum v2; 
		);
	) modules
) ;;

let stop modules = (
	printf "Pull.stop! \n%!";
	List.iter (fun m -> 
		if (m#getMoving()) then m#setMoving false
	) modules
) ;;