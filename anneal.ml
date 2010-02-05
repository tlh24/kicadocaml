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
	
	method setParent m = m_parent <- m (* parent's transform, actually. *)
	method setPos (kx,ky) = mx <- kx; my <- ky; 
	method setNet n = m_net <- n;
	method getNet () = m_net;
	method setConn (conn : a_pad list) = m_conn <- conn; 
	method getPos () = m_parent#transform (mx,my) ; 
	method cost () = (* calculate e.g. for deciding on a rotation *)
		let mypos = self#getPos () in
		List.fold_left (fun s p -> 
			s +. (Pts2.distance mypos (p#getPos ()))
		) 0.0 m_conn
	method suggest () = (* vectoral suggested move *)
		let mypos = self#getPos () in
		let v = List.fold_left (fun s p -> 
			Pts2.add s (Pts2.sub (p#getPos ()) mypos)
		) (0.0,0.0) m_conn in
		Pts2.scl v (foi (List.length m_conn))
end and a_mod = object
	val mutable m_mod = new pcb_module
	val mutable mx = 0.0
	val mutable my = 0.0
	val mutable mr = 0.0
	val mutable mw = 0.0 (* width/2 *)
	val mutable mh = 0.0 (* height/2 *)
	val mutable m_pads : 'a_pad list = []
	
	method setMod m = m_mod <- m
	method getMod () = m_mod
	method setPos (kx,ky) = mx <- kx; my <- ky; 
	method setRot r = mr <- r;
	method setPads (pds: a_pad list) = m_pads <- pds
	method getPads () = m_pads
	method setWH (w,h) = mw <- w; mh <- h; 
	method move (kx,ky) = mx <- mx +. kx; my <- my +. ky; 
	method transform k = (* transform pad coordinates to global *)
		Pts2.add (rotate2 ~angle:(deg2rad mr) k ) (mx,my)
	method getBBX () = 
		let w,h = if cos(deg2rad mr) > 0.707 then mw,mh else mh,mw in
		mx -. w, my -. h, mx +. w, my +. h
	method update () = (* send the new loc to the parent *)
		m_mod#setRot (iof mr); 
		m_mod#move (mx,my); 
		m_mod#setMoving false; 
end

let doAnneal mods render = 
	let temp = 0.002 in (* in mils *)
	let k = ref 0 in
	List.iter (fun m -> m#setRot 0) mods; 
	List.iter (fun m -> m#update ()) mods; 
	List.iter (fun m -> m#setMoving true) mods; 

	(* first, copy the data structures (bad, I know, but my mind is limited.. *)
	let maxnet = ref 0 in
	let amods = List.map (fun m -> 
		let am = new a_mod in
		am#setMod m; 
		am#setPos (m#getPos ()); 
		am#setRot (foi (m#getRot ())); 
		m#setRot 0; 
		m#update () ; (* to get a proper bbx, will change back later *)
		am#setWH (Pts2.scl (bbxWH (m#getBBX false)) 0.5); 
		am#setPads (List.map (fun p ->
			let ap = new a_pad in
			ap#setParent am; 
			ap#setPos (p#getPos ());
			let net = p#getNet () in
			ap#setNet net;
			if net > !maxnet then maxnet := net ; 
			(* set the conected pads later *)
			ap
		) (m#getPads ()) ); 
		am
	) mods in
	(* now need to make an array of lists of pads connected to each net *)
	let nets = Array.mapi (fun i j -> 
		List.fold_left (fun j1 m -> 
			List.fold_left (fun j2 p -> 
				if p#getNet () = i then p::j2 else j2
			) j1 (m#getPads ())
		) j amods; 
	) ( Array.make !maxnet [] ) in
	(* ...and go back and give the pads these lists *)
	List.iter (fun m -> 
		List.iter (fun p -> 
			let n = p#getNet() in
			(* don't self-reference! *)
			p#setConn (List.filter (fun pp -> pp <> p) (nets.(n))); 
		) (m#getPads()); 
	) amods; 
	while !k < 10 do (
		List.iter (fun m -> 
			m#move (((Random.float temp) -. temp/.2.),((Random.float temp) -. temp/.2.)); 
			m#update (); 
		) amods; 
		render (); 
		(* now see where they are hitting, and move them accordingly *)
		List.iter (fun m1 -> 
			let bbx1 = m1#getBBX () in
			List.iter (fun m2 -> 
				let bbx2 = m2#getBBX () in
				if bbxIntersect bbx1 bbx2 then (
					(* move the modlues accordingly *)
					let w1,h1 = bbxWH bbx1 in
					let w2,h2 = bbxWH bbx1 in
					let x1,y1 = bbxCenter bbx1 in
					let x2,y2 = bbxCenter bbx2 in
					let dx,dy = x2 -. x1,y2 -. y1 in
					let calcmov d w = if fabs d < w then ((w -. (fabs d)) *. (fsign d)) else 0.0 in
					let mx,my = (calcmov dx (w1 +. w2)),(calcmov dy (h1 +. h2)) in
					(* note, you only need one of these - rectanguar objects*)
					let move = if (fabs mx) < (fabs my) then mx, 0.0 else 0.0, my in
					m1#move (Pts2.scl move (-0.25)); (* tweak this ? *)
					m2#move (Pts2.scl move 0.25); 
					(* clearly, this will have to run for a while ... *)
				)
			) amods; 
		) amods; 
		List.iter (fun m -> 
			m#update (); 
		) amods; 
		render (); 
		incr k; 
		Printf.printf "pass %d\n%!" !k; 
	) done; 