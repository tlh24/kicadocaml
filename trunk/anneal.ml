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
		(* printf "suggest: list length %d\n%!" (List.length m_conn);
		printf " sum: %f,%f\n%!" (fst v) (snd v); *)
		Pts2.scl v (foi (List.length m_conn))
end and a_mod = object
	val mutable m_mod = new pcb_module
	val mutable m_offset = 0.0,0.0 (* subtract this when setting m_mod's position *)
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
	method getRot () = mr;
	method setPads (pds: a_pad list) = m_pads <- pds
	method getPads () = m_pads
	method setWH (w,h) = mw <- w; mh <- h; 
	method getWH () = mw,mh
	method setOffset o = m_offset <- o; 
	method move (kx,ky) = mx <- mx +. kx; my <- my +. ky; 
	method transform k = (* transform pad coordinates to global *)
		Pts2.add (rotate2 ~angle:(deg2rad mr) k ) (mx,my)
	method getBBX () = 
		let w,h = if cos(deg2rad mr) > 0.707 then mw,mh else mh,mw in
		mx -. w, my -. h, mx +. w, my +. h
	method update () = (* send the new loc to the parent *)
		m_mod#setRot (iof mr * 10); 
		m_mod#setPos (Pts2.sub (mx,my) (rotate2 ~angle:(deg2rad mr) m_offset )); 
		m_mod#setMoving false; 
end

let doAnneal mods render temp passes = 
	let k = ref 0 in
	List.iter (fun m -> m#setMoving true) mods; 

	(* first, copy the data structures (bad, I know, but my mind is limited.. *)
	let maxnet = ref 0 in
	let amods = List.map (fun m -> 
		let am = new a_mod in
		am#setMod m; 
		m#update () ;
		am#setPos (m#getCenter false); 
		am#setRot (foi (m#getRot ())/.10.0); 
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
			let net = p#getNet () in
			ap#setNet net;
			if net > !maxnet then maxnet := net ; 
			(* set the conected pads later *)
			ap
		) (m#getPads ()) ); 
		am
	) (List.filter (fun m -> m#getVisible()) mods) in
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
				if List.length lst < 12 then ( (* ignore the larger nets *)
					p#setConn (List.filter (fun pp -> pp <> p) lst); 
				)
			)
		) (m#getPads()); 
	) amods; 
	let renderAll () = 
		List.iter (fun m -> 
			m#update (); 
		) amods; 
		render (); 
	in
	while !k < passes do (
		(* inject som noise *)
		List.iter (fun m -> 
			m#move (((Random.float temp) -. temp/.2.),((Random.float temp) -. temp/.2.)); 
		) amods; 
		(* renderAll (); *)

		(* move them towards other conneted elements *)
		List.iter (fun m -> 
			let sug = List.fold_left (fun s p -> 
				Pts2.add s (p#suggest())
			) (0.0,0.0) (m#getPads()) in
			let len = List.length (m#getPads()) in
			let mv = Pts2.scl sug (1.0/.((foi len) *. (8.0 +. (Random.float 5.0)))) in
			m#move mv; 
		) amods ; 
		
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
		) amods ; 
		(* renderAll (); *)
		
		(* now see where they are hitting, and move them accordingly *)
		let hitAll () = 
			let mvcnt = ref 1 in
			let passes = ref 0 in
			while !mvcnt > 0 && !passes < 20 do (
				mvcnt := 0;
				List.iter (fun m1 -> 
					List.iter (fun m2 -> 
						if m1 <> m2 then (
							let bbx1 = m1#getBBX () in
							let bbx2 = m2#getBBX () in
							(* let printbbx (x,y,xx,yy) = printf "(%f,%f %f,%f)\n%!" x y xx yy in
							printbbx bbx1; 
							printbbx bbx2; *)
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
								let move = if (fabs mx) < (fabs my) then mx, (0. *.my) else (0. *. mx), my in
								(* printf "move %f,%f\n%!" (fst move) (snd move) ; *)
								m1#move (Pts2.scl move (-0.75)); (* tweak this ? *)
								m2#move (Pts2.scl move 0.75); 
								incr mvcnt;
								(* clearly, this will have to run for a while ... *)
							)
						)
					) amods; 
				) amods; 
				incr passes; 
			) done; 
		in
		hitAll(); 
		renderAll(); 
		
		incr k; 
		Printf.printf "pass %d\n%!" !k; 
	) done; 