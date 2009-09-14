open Comm
open Grfx
open Modtext
open Shape
open Pad
open Mod
open Track

let testdrc track tracks modules = 
	(* see if a track intersects any pads or tracks that are not on its net.  *)
	(* returns true if there is a violation *)
	(* plus the point on 'track' where the violation occured *)
	(* plus the point on the offended track closest to this violation *)
	let net = track#getNet() in
	let lay = track#getLayer() in
	let st = track#getStart() in
	let en = track#getEnd() in
	let w = (track#getWidth()) *. 0.5 in
	let bbx = track#getDrcBBX() in
	let suggest = ref (en , (0., 0.)) in
	let isvia = track#isVia() in
	let (_, violation) = try ( 
		(List.find (fun t -> 
			if bbxIntersect bbx (t#getDrcBBX()) then (
				if t#getNet() != net && (t#getLayer() = lay || isvia || t#isVia()) then (
					let d,e,f = Pts2.linedistance st en 
						(t#getStart()) (t#getEnd()) in
					let w2 = t#getWidth() *. 0.5 in
					if d < w +. w2 +. !gclearance then (
						t#setHighlight true ; 
						(* adjust e to satisfy DRC *)
						(*print_endline ("suggested E: " ^ (sof (fst e)) ^ " " ^ (sof (snd e))) ;
						print_endline ("suggested F: " ^ (sof (fst f)) ^ " " ^ (sof (snd f))) ; *)
						let fe = Pts2.norm (Pts2.sub e f) in
						let ee = Pts2.add f (Pts2.scl fe (w +. w2 +. !gclearance +. 0.0001)) in
						(*print_endline ("suggesting: " ^ (sof (fst ee)) ^ " " ^ (sof (snd ee))) ;*)
						suggest := ee,f ; 
						true
					) else ( false) ; 
				) else ( false )
			) else ( false )
		) tracks), true 
	) with Not_found -> (track, false) in
	if (not violation) then (
		(* search through the pads *)
		let pad = ref (List.hd ((List.hd modules)#getPads())) in
		let violation2 = List.exists (fun m -> 
			if bbxIntersect bbx (m#getDrcBBX()) then (
				let p,f = m#testdrc st en w net lay suggest in
				if f then pad := p; 
				f
			) else false
		) modules 
		in
		if violation2 then !pad#setHighlight true ;
		violation2, !suggest
	) else true, !suggest (* track violation *)
	;;	

let testdrc2 track tracks modules = 
	(* for when we don't want suggestions to change the endpoint *)
	let violation, _ = testdrc track tracks modules in
	violation
	;;
let testdrcAll tracks modules top rendercb () = 
	print_endline "note: DRC check should be performed after propagating netcodes to all tracks"; 
	print_endline "(or after pad connectivity test)"; 
	(* test all the tracks & make a dialog for centering on the violations*)
	(* does not check spacing between pads and other pads *)
	let drcerr = ref [] in
	let trac = ref (!tracks) in
	List.iter (fun t-> 
		let violation, (s1,s2) = testdrc t !trac !modules in
		if violation then (
			drcerr := ((Pts2.scl (Pts2.add s1 s2) 0.5),t,(Pts2.distance s1 s2)) :: !drcerr ; 
		); 
		trac := List.tl !trac ; (* we have already tested one against all, no need to do it again*)
	) !tracks ; 
	if List.length !drcerr > 0 then (
	let dlog = Toplevel.create top in
		Wm.title_set dlog "DRC errors" ;
		(* make a series of buttons -- but only the first 30 errors *)
		let err = ref [] in
		let min a b = if a < b then a else b in
		for i = 1 to (min 30 (List.length !drcerr)) do (
			err := ((List.nth !drcerr (i-1)) :: !err) ; 
		) done ; 
		let cnt = ref 0 in
		let buttons = List.map (fun (p,t,d) -> 
			incr cnt ; 
			Button.create ~text:((soi !cnt) ^ ": " ^ (sof (fst p))^", "^(sof (snd p))^" d="^(sof d))
				~command:(fun () -> 
					gpan := (Pts2.scl p (-1.0)); 
					ignore(testdrc t !tracks !modules) ; (* to set highlight *)
					t#setHighlight true ;
					rendercb (); )
				dlog) !err ; 
		in
		Tk.pack ~fill:`Both ~expand:true ~side:`Top buttons ; 
	) else (
		print_endline "no DRC (spacing) errors encountered!!"; 
	)
	;;
 
 let rec pushdrc track tracks modules tab = 
 	(* recursive search for set of movements 
 	that would allow 'track' to be placed *)
 	(* like test DRC, returns true on violation *)
 	(* and also returns a (possibly discarded) suggestion *)
 	(* 		this suggest feature has not been tested... *)
 	let net = track#getNet() in
	let lay = track#getLayer() in
	let st = track#getStart() in
	let en = track#getEnd() in
	let w = (track#getWidth()) *. 0.5 in
	let bbx = track#getDrcBBX() in
	let suggest = ref (en , (0., 0.)) in
	let isvia = track#isVia() in
	(* first see if the track hits a pad. 
	if it does, then the move will not work *)
	let pad = ref (List.hd ((List.hd modules)#getPads())) in
	let violation2 = List.exists (fun m -> 
			if bbxIntersect bbx (m#getDrcBBX()) then (
				let p,f = m#testdrc st en w net lay suggest in
				if f then pad := p; 
				f
			) else false
		) modules 
	in
	if violation2 then(
		!pad#setHighlight true ;
		true
	) else (
		(* otherwise, this track has not hit any non-moveable pads, so we can try to 
		move some of the things that it hit & see if that ameliorates the problem *)
		(* find all the tracks that we may have hit. *)
		let suggestions2 = ref [] in
		let tracks2 = List.filter (fun t -> 
			if bbxIntersect bbx (t#getDrcBBX()) then (
					if t#getNet() != net && (t#getLayer() = lay || isvia || t#isVia()) then (
						let d,e,f = Pts2.linedistance st en 
							(t#getStart()) (t#getEnd()) in
						let w2 = t#getWidth() *. 0.5 in
						if d < w +. w2 +. !gclearance then (
							t#setHighlight true ; 
							suggestions2 := ( (e,f) :: !suggestions2) ; 
							true
						) else ( false) ; 
					) else ( false )
				) else ( false )
			) tracks 
		in
		(* tracks2 = tracks with violation *)
		(* if any of these violations are actual intersections, 
		then the move / push is not going to work. *)
		if List.exists (fun t -> 
				let d,_, _ = Pts2.intersect st en (t#getStart()) (t#getEnd()) in
				d <= 0.0) tracks2 then (
			print_endline (tab ^ "intersection found " ^ (soi (List.length tracks2))); 
			true
		) else (
			print_endline (tab ^ "tracks in volation: " ^ (soi (List.length tracks2)) ); 
			(* new method: move each of the tracks in the violated list based on the move and u 
			parameters, back out if it doesn't work, and return true if any of them are violators. *)
			let count = ref 0 in
			if List.length tracks2 = 0 then false else(
				List.exists2 (fun track (e,f) -> 
					incr count; 
					let tab2 = tab ^ (soi !count) ^ "/" in
					(* using e, f & the desired distance between tracks, compute a vectoral move *)
					let w2 = track#getWidth() *. 0.5 in
					let w22 = w2 *. w2 in
					let lay2 = track#getLayer() in
					let net2 = track#getNet() in 
					let ef = Pts2.norm (Pts2.sub f e) in
					let ff = Pts2.add e (Pts2.scl ef (w +. w2 +. !gclearance +. 0.0001)) in
					let move = Pts2.sub ff f in
					let st2 = track#getStart() in
					let en2 = track#getEnd() in
					let u = if track#isVia() then ( 0.0
						) else if Pts2.parallel en2 st2 st en then (
							0.5 (* if they are parallel or nearly so, then set the hit-point to the middle*)
						) else (
							Pts2.relativelen (Pts2.sub en2 st2) (Pts2.sub f st2)
						)
					in
					print_endline ( tab2 ^ " ( u = " ^ (sof u) ^ " w = " ^ (sof w2)); 
					(* need to get the tracks connected to violated tracks, move them too *)
					(* see if there is a via at the start/end.  if so, move on all layers *)
					let viastart = (track#isVia()) || List.exists (fun t-> t#isVia() && t != track &&
						Pts2.distance2 st2 (t#getStart() ) < w22) tracks in
					let viaend = List.exists (fun t-> t#isVia() &&  t != track &&
						Pts2.distance2 en2 (t#getStart() ) < w22) tracks in
					(* find all tracks connected to start and end *)
					(* partition so we don't have to count twice *)
					let nettracks = List.filter (fun t -> 
						t != track && t#getNet() = net2) tracks in
					let hitstartstart,tracks4 = List.partition (fun t -> 
						if (t#getLayer() = lay2 || viastart)  then (
							Pts2.distance2 st2 (t#getStart() ) < w22
						) else false ) nettracks in
					let hitstartend,tracks5 = List.partition (fun t -> 
						if (t#getLayer() = lay2 || viastart) && not (t#isVia()) then (
							Pts2.distance2 st2 (t#getEnd() ) < w22
						) else false ) tracks4 in
					let hitendstart,tracks6 = List.partition (fun t -> 
						if (t#getLayer() = lay2 || viaend)  then (
							Pts2.distance2 en2 (t#getStart() ) < w22
						) else false ) tracks5 in
					let hitendend,_ = List.partition (fun t -> 
						if (t#getLayer() = lay2 || viaend) && not (t#isVia()) then (
							Pts2.distance2 en2 (t#getEnd() ) < w22
						) else false ) tracks6 in
					print_endline (tab2 ^ " tracks connected start : " ^ 
						(soi ( (List.length hitstartstart) + (List.length hitstartend) )) ); 
					print_endline (tab2 ^ " tracks connected end : " ^ 
						(soi ( (List.length hitendstart) + (List.length hitendend) )) ); 
					let movedtrax = (track :: (List.rev_append 
						(List.rev_append hitstartstart hitstartend)
						(List.rev_append hitendstart hitendend) ) ) in
					List.iter (fun t -> t#setDirty true ) movedtrax ; 
					let moveAll mv uu = 
						if uu < 0.9 then ( 
							track#setStart (Pts2.add (track#getStart()) mv) ; 
							let st3 = track#getStart() in
							List.iter (fun t -> t#setStart st3) hitstartstart ; 
							List.iter (fun t -> t#setEnd st3) hitstartend ; 
						); 
						if uu > 0.1 then ( 
							track#setEnd (Pts2.add (track#getEnd()) mv) ; 
							let en3 = track#getEnd() in
							List.iter (fun t -> t#setStart en3) hitendstart ; 
							List.iter (fun t -> t#setEnd en3) hitendend ; 
						)
					in
					moveAll move u; 
					let unmove = Pts2.scl move (-1.0) in
					let count2 = ref 0 in
					(* test the motion *)
					print_endline ( tab2 ^ " try move with u"); 
					if List.exists (fun t -> incr count2; 
							pushdrc t tracks modules (tab2 ^ (soi !count2) ^ "/")) movedtrax then (
						moveAll unmove u ; 
						if  u < 0.9  && u > 0.1 then (
							(* try moving just the start *)
							moveAll move 0.0 ; 
							count2 := 0; 
							print_endline ( tab2 ^ " try moving just start"); 
							if List.exists (fun t -> incr count2; pushdrc t tracks modules (tab2 ^ (soi !count2) ^ "/")) movedtrax then (
								moveAll unmove 0.0 ; 
								(* try moving just the end *)
								moveAll move 1.0 ; 
								count2 := 0; 
								print_endline ( tab2 ^ " try moving just end"); 
								if List.exists (fun t -> incr count2; pushdrc t tracks modules (tab2 ^ (soi !count2) ^ "/")) movedtrax then (
									moveAll unmove 1.0 ; 
									(* we moved them back, so they are no longer dirty *)
									List.iter (fun t -> t#setDirty false ) movedtrax ; 
									print_endline ( tab2 ^ " violation )"); 
									true
								) else false
							) else false
						) else (
							print_endline ( tab2 ^ " violation in start/end move )"); 
							(* but it didn't work, so say that *)
							true
						)
					) else (
						print_endline ( tab2 ^ " no violation initial move! )"); 
						false (* o hey it worked! *)
					)
				) tracks2 (List.rev !suggestions2)
			) (* length tracks2 > 0 *)
		) (* intersection found *)
	) (* pad violation *)
	;;
	
let enlargeDrc tracks modules maxwidth increment renderfun = 
	(* see if we can make any track larger, up to maxwidth, 
	without violating DRC. *)
	(* this is somewhat order-dependent, so start by rasing the 
	minimum width slowly *)
	(* really need this to *not* change the size of tracks leaving pads - 
	this may cause solder bridges. well, I can add that later. *)
	let startw = List.fold_left (fun mn t -> min mn (t#getWidth())) 1.0 tracks in
	let rec enlarge1 tracks1 w =
		Printf.printf "enlargeDRC with %d, width %f \n%!" (List.length tracks1) w; 
		if (List.length tracks1) > 0  && w <= maxwidth then (
			let tracks2 = List.filter (fun t -> 
				(* see if this track can be enlarged a bit .. *)
				(* but do not make it smaller! *)
				let oldw = t#getWidth() in
				if oldw < w then (
					t#setWidth w; 
					t#update() ; 
					let violation = testdrc2 t tracks modules in
					if violation then (
						t#setWidth oldw; 
						t#update() ; 
						false
					) else true
				) else true (* keep it around until we know there is a violation .. *)
			) tracks1 in
			renderfun (); t
			enlarge1 tracks2 (w +. increment)
		) else ()
	in
	enlarge1 tracks startw 
	;;
	