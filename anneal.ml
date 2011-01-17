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

(* in order to make this simple/efficient, should have a better way of representing the data *)
(* mod.ml has become too heavy *)
(* all we really need is the bounding box, the pad locations, the netnumbers of the pads *)
(* then can keep the pads linked through a linked list *)
let deg2rad mr = ((mr/.180.0)*.3.14159265)

class a_pad = object (self)
	val mutable m_parent = new a_mod
	val mutable m_conn : 'a_pad list = []
	val mutable mx = 0.0 (* offset from center of module *)
	val mutable my = 0.0
	val mutable m_net = 0
	val mutable m_name = ""
	
	method setParent m = m_parent <- m (* parent's transform, actually. *)
	method setPos (kx,ky) = mx <- kx; my <- ky; 
	method setNet n = m_net <- n;
	method getNet () = m_net;
	method setName n = m_name <- n; 
	method getName () = m_name
	method setConn (conn : a_pad list) = m_conn <- conn; 
	method getPos () = m_parent#transform (mx,my) ; 
	method cost () = (* calculate e.g. for deciding on a rotation *)
		let mypos = self#getPos () in
		List.fold_left (fun s p -> 
			s +. (Pts2.distance mypos (p#getPos ()))
		) 0.0 m_conn
	method suggest () = (* vectoral suggested move *)
		(* printf "------\n%!"; *)
		let mypos = self#getPos () in
		let v = List.fold_left (fun s p -> 
			let d = Pts2.sub (p#getPos ()) mypos in
			Pts2.add s d
		) (0.0,0.0) m_conn in
		let len = List.length m_conn in
		let scl = if len > 0 then 1.0 /. (foi len) else 0.0 in 
		Pts2.scl v scl
	method suggest_closest () = (* again, vectoral suggested move, but only the closest ala MST*)
		let mypos = self#getPos () in
		let q = List.fold_left (fun s p -> 
			let d = Pts2.sub (p#getPos ()) mypos in
			if Pts2.length2 d < Pts2.length2 s then d else s
		) (1e10,1e10) m_conn in
		if Pts2.length2 q > 1e10 then (0.0,0.0) else q
	method draw () = 
		(* returns a list of lines (float*float*float*float) *)
		let mypos = self#getPos () in
		List.map (fun con -> 
			(mypos , con#getPos())
		) m_conn

end and a_mod = object
	val mutable m_mod = new pcb_module
	val mutable m_offset = 0.0,0.0 (* subtract this when setting m_mod's position *)
	val mutable mx = 0.0
	val mutable my = 0.0
	val mutable mr = 0.0
	
	val mutable mw = 0.0 (* width/2 *)
	val mutable mh = 0.0 (* height/2 *)
	val mutable m_pads : 'a_pad list = []
	val mutable m_name = ""
	val mutable m_lock = false; 
	
	method setMod m = m_mod <- m
	method getMod () = m_mod
	method setPos (kx,ky) = mx <- kx; my <- ky; 
	method setRot r = mr <- r;
	method getRot () = mr;
	method setPads (pds: a_pad list) = m_pads <- pds
	method getPads () = m_pads
	method setWH (w,h) = mw <- w; mh <- h; 
	method getWH () = mw,mh
	method setOffset o = m_offset <- o; 
	method setName n = m_name <- n; 
	method getName () = m_name
	method setLock n = m_lock <- n; 
	method getLock () = m_lock
	method move (kx,ky) = mx <- mx +. kx; my <- my +. ky; 
	method transform k = (* transform pad coordinates to global *)
		Pts2.add (rotate2 ~angle:(deg2rad mr) k ) (mx,my)
	method getBBX () = 
		let w,h = if fabs(cos(deg2rad mr)) > 0.707 then mw,mh else mh,mw in
		mx -. w, my -. h, mx +. w, my +. h
	method update () = (* send the new loc to the parent *)
		m_mod#setRot (iof mr * 10); 
		m_mod#setPos (Pts2.sub (mx,my) (rotate2 ~angle:(deg2rad mr) m_offset )); 
		m_mod#setMoving false; 
	method draw pl = 
		List.fold_left (fun lst p -> 
			List.rev_append ( p#draw()) lst
		) pl m_pads
end

(* now see where they are hitting, and move them accordingly *)
let hitAll loop amods renderLines = (
	let mvcnt = ref 1 in
	let onemore = ref true in
	let hitpass = ref (if loop then (-50) else 18) in
	while !mvcnt > 0 && !hitpass < 20 do (
		mvcnt := 0;
		List.iter (fun m1 -> 
			List.iter (fun m2 -> 
				if m1 <> m2 then (
					let bbx1 = m1#getBBX () in
					let bbx2 = m2#getBBX () in
					(*printf "-------\n"; 
					let printbbx (x,y,xx,yy) r = 
						printf "(%f,%f %f,%f) siz %f %f r %f\n%!" 
						x y xx yy (xx-.x) (yy-.y) r in
					printbbx bbx1 (m1#getRot()); 
					printbbx bbx2 (m2#getRot()); *)
					if bbxIntersect bbx1 bbx2 then (
						(* move the modlues accordingly *)
						let w1,h1 = bbxWH bbx1 in
						let w2,h2 = bbxWH bbx2 in
						let x1,y1 = bbxCenter bbx1 in
						let x2,y2 = bbxCenter bbx2 in
						let dx,dy = x2 -. x1,y2 -. y1 in
						let calcmov d w = if fabs d < w then ((0.5*.w -. (fabs d)) *. (fsign d)) else 0.0 in
						let mx,my = (calcmov dx (w1 +. w2)),(calcmov dy (h1 +. h2)) in
						(* note, you only need one of these moves - rectanguar objects*)
						let move = if (fabs mx) < (fabs my) && mx <> 0.0 then mx, (0. *.my) else (0. *. mx), my in
(* 								printf "move %f,%f\n%!" (fst move) (snd move) ; *)
						if not (m1#getLock ()) then (
							if not (m2#getLock ()) then (
								(* larger objects move less! *)
								let frac1 = (w1 *. h1) /. ((w1 *. h1) +. (w2 *. h2)) in
								let frac2 = (w2 *. h2) /. ((w1 *. h1) +. (w2 *. h2)) in
								m1#move (Pts2.scl move (-1.2 *. frac2)); (* tweak this constant ? *)
								m2#move (Pts2.scl move (1.2 *. frac1)); 
							) else ( (* m2 locked *)
								m1#move (Pts2.scl move (-1.2)); 
							)
						) else (
							if not (m2#getLock ()) then (
								m2#move (Pts2.scl move 1.2); 
							)
						);
						incr mvcnt;
						(* clearly, this will have to run for a while ... *)
					)
				)
			) amods; 
		) amods; 
		incr hitpass; 
		renderLines();
		(* make sure we have one good pass at the end. *)
		if !mvcnt <> 0 then onemore := true; 
		if !mvcnt = 0 && !onemore then (
			onemore := false; 
			incr mvcnt; 
		);
	) done; 
) ;;

let makeAmods mods maxnet = 
	List.map (fun m -> 
		let am = new a_mod in
		am#setMod m; 
		m#update () ;
		am#setPos (m#getCenter false); 
		am#setRot (foi (m#getRot ())/.10.0); 
		am#setName (m#getRef()); 
		m#setRot 0; 
		m#setPos (0.0, 0.0); 
		m#update () ; (* to get a proper bbx, will change back later *)
		let w, h = Pts2.scl (bbxWH (m#getBBX false)) 0.75 in
		let offset = bbxCenter (m#getBBX false) in
		am#setWH (w,h); 
		am#setOffset offset;
		(*printf "module %s w %f h %f rot %f\n%!" (m#getRef()) w h (am#getRot()); *)
		am#setPads (List.map (fun p ->
			let ap = new a_pad in
			ap#setParent am; 
			ap#setPos (Pts2.sub (p#getPos ()) offset);
			ap#setName (p#getPadName()); 
			let net = p#getNet () in
			ap#setNet net;
			if net > !maxnet then maxnet := net ; 
			(* set the conected pads later *)
			ap
		) (m#getPads ()) ); 
		am
	) (List.filter (fun m -> m#getVisible()) mods)


let doAnneal mods render stemp etemp passes nlock = (
	let k = ref 0 in
	List.iter (fun m -> m#setMoving true) mods; 
	(* first, copy the data structures (bad, I know, but my mind is limited.. *)
	let maxnet = ref 0 in
	let amods = makeAmods mods maxnet in
	(* now need to make an array of lists of pads connected to each net *)
 	let nets = Array.mapi (fun i j -> 
		List.fold_left (fun j1 m -> 
			List.fold_left (fun j2 p -> 
				if p#getNet () = i then p::j2 else j2
			) j1 (m#getPads ())
		) j amods; 
	) ( Array.make (!maxnet+1) [] ) in 
	(* ...and go back and give the pads these lists *)
	List.iter (fun m -> 
		List.iter (fun p -> 
			let n = p#getNet() in
			(* don't self-reference! *)
			if(n >= 0 && n <= !maxnet) then (
				let lst = nets.(n) in
				if List.length lst < 120 then ( (* ignore the larger nets *)
					p#setConn (List.filter (fun pp -> pp <> p) lst); 
				)
			)
		) (m#getPads()); 
	) amods; 
	let renderAll () = 
		List.iter (fun m -> 
			m#update (); 
		) amods;
		render (fun _ -> false); 
	in
	let renderLines () = 
		let cb () = 
			let lines = List.fold_left (fun lst m -> 
				m#draw lst) [] amods in
			let len = List.length lines in
			let rawv = Raw.create_static `float (4 * len) in
			let i = ref 0 in
			List.iter (fun ((sx,sy),(ex,ey)) ->
				Raw.set_float rawv ~pos:(4* !i + 0) sx ; 
				Raw.set_float rawv ~pos:(4* !i + 1) sy ; 
				Raw.set_float rawv ~pos:(4* !i + 2) ex ; 
				Raw.set_float rawv ~pos:(4* !i + 3) ey ; 
				incr i ; 
			) lines ; 
			GlDraw.color ~alpha:0.2 (1. , 1. , 1. ); 
			GlMat.push() ; 
			GlMat.translate ~x:(0.) ~y:(0.) ~z:(0.99) (); 
			GlArray.vertex `two rawv; 
			GlArray.draw_arrays `lines 0 (len * 2) ; 
			GlMat.pop() ; 
			Raw.free_static rawv;
			true
		in
		render cb ;
	in 
	(* now make a list of modules that can be moved. *)
	let amods2 = List.filter (fun m -> List.length (m#getPads ()) < nlock) amods in
	List.iter (fun m -> m#setLock true) amods;
	List.iter (fun m -> m#setLock false) amods2; 
	while !k < passes do (
		let temp = etemp +. (stemp -. etemp) *. ((foi (passes - !k))/.(foi passes)) in
		Printf.printf "pass %d temperature %f\n%!" !k temp; 
		(* inject som noise *)
		List.iter (fun m -> 
			(* use box-muller transform to make them gaussian *)
			(* http://www.taygeta.com/random/gaussian.html *)
			let x1 = ref 0.0 in
			let x2 = ref 0.0 in
			let w = ref 2.0 in
			while !w >= 1.0 do (
				x1 := 2.0 *. (Random.float 1.0) -. 1.0;
				x2 := 2.0 *. (Random.float 1.0) -. 1.0; 
				w := !x1 *. !x1 +. !x2 *. !x2 ; 
			) done; 
			w := sqrt( (-2.0 *. log(!w)) /. !w); 
			let y1 = !x1 *. !w *. temp in
			let y2 = !x2 *. !w *. temp in
			m#move (y1,y2);
		) amods2; 
		renderLines ();

		(* move them towards other conneted elements *)
		List.iter (fun m -> 
			let sug = List.fold_left (fun s p -> 
				Pts2.add s (p#suggest())
			) (0.0,0.0) (m#getPads()) in
			let len = List.length (m#getPads()) in
			let mv = Pts2.scl sug (1.0/.((foi len) *. (1.2 +. (Random.float 1.0)))) in
 			(* printf "move: %f %f (%f long)\n%!" (fst mv) (snd mv) (Pts2.length mv); *) 
			m#move mv; 
			(* m#update (); 
			render (); *)
		) amods2 ;
		renderLines (); 
		
		(* rotate if that will help *)
		List.iter (fun m -> 
			let getCost () = List.fold_left (fun s p -> 
				s +. p#cost()
			) 0.0 (m#getPads()) in
			let costs = List.map (fun r -> m#setRot r; (r,getCost())) [0.0;90.0;180.0;270.0] in
			(*List.iter(fun a -> 
				printf "rot %f cost %f\n%!" (fst a) (snd a); 
			) costs; *)
			let sort = List.sort (fun a b -> compare (snd a) (snd b)) costs in
			let best = List.hd sort in
			(*printf "best %f\n%!" (fst best); *)
			m#setRot (fst best); 
		) amods2 ; 
		renderLines (); 
		
		
		if temp < 0.1 then hitAll (!k = (passes-1)) amods renderLines; (* doesn't make sense to do the 'hit' when 
			normal movement is greater than displacement movement *)
		incr k; 
	) done; 
	renderAll () 
) ;;