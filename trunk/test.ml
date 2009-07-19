let insidetri a b c d = 
	cross (sub b a) (sub d a) >= 0.0 &&
	cross (sub c b) (sub d b) >= 0.0 &&
	cross (sub a c) (sub d c) >= 0.0 ;;
	
let insidetri2 a b c d = 
	dot (sub b a) (sub c a) > dot (sub b a) (sub d a) &&
	dot (sub c b) (sub a b) > dot (sub c b) (sub d b) &&
	dot (sub a c) (sub b c) > dot (sub a c) (sub d c)
	
let insidetri2 (x1,y1) (x2,y2) (x3,y3) (x,y) = 
	(* use barycentric coordinates *)
	let a = x1 -. x3 in
	let b = x2 -. x3 in
	let c = y1 -. y3 in
	let d = y2 -. y3 in
	let det = (a*.d -. b*.c) in
	if abs_float det > 0.00001 then (
		let e = x -. x3 in
		let f = y -. y3 in
		let b1,b2 = (d*.e -. b*.f) /. det , (a*.f -. c*.e) /. det in
		b1 >= 0.0 && b1 <= 1.0 && b2 >= 0.0 && b2 <= 1.0
		(* the equality is for when the vertex is on an edge of the triangle. *)
	) else false

insidetri (-1.0,-2.0) (1.0,0.0) (-1.0,2.0) (0.0,0.0);; (* should be true *)
insidetri (-1.0,-2.0) (1.0,0.0) (-1.0,2.0) (2.0,0.0);; (* should be false *)
insidetri (-1.0,-2.0) (1.0,0.0) (-1.0,2.0) (-2.0,0.0);; (* should be false *)
insidetri (-2.0,0.0) (2.0,0.0) (0.0,2.0) (1.0,1.0);;  (* is true .. *)
insidetri (-2.0,0.0) (2.0,0.0) (0.0,2.0) (-1.0,1.0);; (* is true ... *)

let bindMouseSelect () = 
	(* method: let the user drag the cursor to make a transparent rectangle
	when the mouse is released, select all modules & tracks that intersect 
	the box, and keep them selected as long as 'shift' is held down. 
	upon the next click, move the modules and tracks. (using existing logic). *)
	(* use gmode to return to the old state once shift is released *)
	let modules = ref [] in 
	let safemove = ref (0. , 0.) in
	let startPos = ref (0. , 0.) in
	let tracks = ref [] in
	bind ~events:[`ButtonPressDetail(1)] ~fields:[`MouseX; `MouseY] ~action:
	(fun ev -> 
		let (sx,sy) calcCursPos ev !gpan true; 
		tracks := [] ;
		if not !gbutton3pressed then (
			List.iter (fun m-> m_setHit false) !gmodules; 
			List.iter (fun t -> t#setMoving false) !gtracks ; 
			
			gbutton1pressed := true ; 
			let actionMove evinf = 
				let (px,py) = calcCursPos evinf !gpan true in
				gselectRect := ((fmin sx px),(fmin sy py),(fmax sx px),(fmax sy py)) ; 
				(* iterate through the modules & update selected *)
				List.iter (fun m-> 
					if bbxIntersect (m#getBBX ())  !gselectRect then 
						m#setHit true 
					else m#setHit false
				) !gmodules ; 
				(* same for the tracks *)
				List.iter (fun t-> 
					if t#selectHit !gselectRect then 
						t#setMoving true
					else t#setMoving false
				) !gtracks ; 
				render togl ; 
			in
			bind ~events:[`Motion] ~fields:[`MouseX; `MouseY] ~action:actionMove top ; 
			oldActionMove := actionMove ; 
		)
	) top ; 
	bind ~events:[`ButtonReleaseDetail(1)] ~fields:[`MouseX; `MouseY] ~action:
	(fun evinf ->
		gbutton1pressed := false ;
		gselectRect := (1e99,1e99,1e99,1e99) ; 
		bind ~events:[`Motion] ~fields:[`MouseX; `MouseY] ~action:updatecurspos top;
		oldActionMove := updatecurspos ; 
		updatecurspos ev ; 
	) top ; 
	let shiftRelease evinf = 
		let s = match gmode with
			| Mode_MoveModule -> "move module"
			| Mode_MoveText -> "move text"
			| Mode_MoveTrack -> "move track"
			| Mode_AddTrack -> "add track"
			| _ -> ""
		in
		updateMode s ; (* this will reset the button callbacks. *)
	in
	bind ~events:[`KeyPressDetail("Shift_R")] ~action:shiftRelease top ;
	bind ~events:[`KeyReleaseDetail("Shift_R")] ~action:shiftRelease top ;
in 

let bindMouseMoveModule () = 
		gmode := Mode_MoveModule ;
		let modules = ref [] in 
		let safemove = ref (0. , 0.) in
		let startPos = ref (0. , 0.) in
		let tracks = ref [] in
		bind ~events:[`ButtonPressDetail(1)] ~fields:[`MouseX; `MouseY] ~action:
		(fun ev -> 
			startPos :=  calcCursPos ev !gpan true; 
			tracks := [] ;
			if not !gbutton3pressed then (
				modules := List.filter (fun m -> m#getHit() ) !gmodules ; 
				(* maybe do a selection of layer here.. later. *)
				List.iter (fun m -> m#setMoving true ) !modules ;
				(* move any tracks with ends on the pads, 
				snap them to the center of the pads, (if possible), and DRC test. *)
				List.iter (fun m-> 
					let pos = m#getPos() in
					let _, txtht = m#txthit () in
					if not txtht then (
						List.iter (fun p -> 
							List.iter (fun t-> 
								if p#hasLayer (t#getLayer()) then (
									let oldstart = t#getStart() in
									let oldend = t#getEnd() in
									let hit1,snap1 = p#pointInPad (Pts2.sub oldstart pos) in
									if hit1 then (
										t#setStart (Pts2.add pos snap1); 
										(* if the snap creates a violation, then don't snap *)
										if testdrc2 t !gtracks !gmodules then t#setStart oldstart ; 
										t#setU 0. ; 
										t#setMoving true ; 
										tracks := (t :: !tracks); 
									)else(
										let hit2,snap2 = p#pointInPad (Pts2.sub oldend pos) in
										if hit2 then (
											t#setEnd (Pts2.add pos snap2); 
											(* if the snap creates a violation, then don't snap *)
											if testdrc2 t !gtracks !gmodules then t#setEnd oldend ; 
											t#setU 1. ; 
											t#setMoving true ; 
											tracks := (t :: !tracks); 
										); 
									); 
								) ; 
							) !gtracks; 
						) (m#getPads()) ; 
					); 
				) !modules ; 
	
				gbutton1pressed := true ; 
				let actionMove evinf = 
					let prescurspos = calcCursPos evinf !gpan true in
					(* try the move; if it causes any problems, back up. *)
					gdrag :=  Pts2.sub prescurspos !startPos ; 
					List.iter (fun m -> m#move !gdrag ) !modules ; 
					List.iter (fun t -> t#move !gdrag ) !tracks ; 
					let violation = 
						(* note: don't have to update the graphics to test DRC. *)
						List.exists (fun t -> testdrc2 t !gtracks !gmodules; ) !tracks in
					if violation then (
						List.iter (fun m -> m#move !safemove ) !modules ; 
						List.iter (fun t -> t#move !safemove ) !tracks ; 
					) else (
						safemove := !gdrag ; 
					); 
					(*need to update the graphics for the tracks (not needed for modules) *)
					List.iter (fun t-> t#update (); ) !tracks ; 
					render togl ; 
				in
				bind ~events:[`Motion] ~fields:[`MouseX; `MouseY] ~action:actionMove top ; 
				oldActionMove := actionMove ; 
			)
		) top ; 

		bind ~events:[`ButtonReleaseDetail(1)] ~fields:[`MouseX; `MouseY] ~action:
		(fun evinf ->
			gbutton1pressed := false ;
			bind ~events:[`Motion] ~fields:[`MouseX; `MouseY] ~action:updatecurspos top;
			oldActionMove := updatecurspos ; 
			(* need to update the moved tracks, if there were any *)
			List.iter (fun m -> m#setMoving false; ) !modules ; 
			(* need to update the moved tracks, if there were any *)
			List.iter (fun t-> t#setMoving false; t#setHit false; ) !tracks ; 
			gratsnest#updateTracksAll !gtracks ; 
			updatecurspos evinf ; 
		) top ; 
		(* unbind the v-key *)
		bind ~events:[`KeyPressDetail("v")] ~action:(fun ev -> ()) top; 
	in