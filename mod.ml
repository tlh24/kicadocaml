
open Printf
open Comm
open Grfx
open Modtext
open Shape
open Pad

class shape3d = 
object
(* this is a very simple class - just stores the string contents of the section *)
	val mutable m_str = [] ; 
	method read ic = 
		printf "reading shape3d\n%!" ;
		let line = ref (input_line ic) in
		while not (Pcre.pmatch ~pat:"\$EndSHAPE3D" !line ) do (
			m_str <- !line :: m_str ; 
			line := input_line ic ; 
		) done ; 
		m_str <- List.rev m_str ;

	method save oc = 
		fprintf oc "$SHAPE3D\n"; 
		List.iter (fun s-> fprintf oc "%s\n" s) m_str ; 
		fprintf oc "$EndSHAPE3D\n"; 
end

class pcb_module =
object (self)
	val mutable m_x = 0.
	val mutable m_y = 0.
	val mutable m_rot = 0
	val mutable m_layer = 0
	val mutable m_LastEditTime = ""
	val mutable m_TimeStamp = ""
	val mutable m_statusText = ""
	val mutable m_libRef = ""
	val mutable m_doc = ""
	val mutable m_keyword = ""
	val mutable m_path = ""
	(*val mutable m_timestamp = "" *)
	val mutable m_cntRot90 = 0
	val mutable m_cntRot180 = 0
	val mutable m_attributes = ""
	val mutable m_pads : 'pcb_pad list = []
	val mutable m_texts : 'pcb_modtext list = []
	val mutable m_shapes : 'pcb_shape list = []
	val mutable m_shapes3d : 'shape3d list = []
	val mutable m_move = (0. , 0.); 
	val mutable m_moving = false
	val mutable m_g = new grfx
	val mutable m_hit = false 
	val mutable m_washit = false
	val mutable m_visible = true
	val mutable m_updateCallback = (fun () -> () )
	val mutable m_drcBBX = (0. , 0. , 0. , 0.)
	val mutable m_deleteAttachedTracks = false
	
	method setVisible b = m_visible <- b ;
	method getVisible () = m_visible ;
	method setUpdateCallback f = m_updateCallback <- f ; 
	method update () = (
		(* print_endline("updating pads:" ^ (string_of_int (List.length m_pads))); *)
		m_move <- (0. , 0. ) ;
		List.iter (fun p -> p#update m_x m_y) m_pads ; 
		List.iter (fun p -> p#setPart ( self#getRef() ) ) m_pads ; 
		List.iter (fun p -> p#update m_rot m_x m_y) m_shapes ; 
		List.iter (fun p -> p#update m_rot m_x m_y ) m_texts ; 
		(* draw ourself simply, with a rectangle containing all the pads *)
		let (lx, ly, hx,hy) = List.fold_left (fun a b -> 
			let aminx,aminy,amaxx,amaxy = a in
			let bminx,bminy,bmaxx,bmaxy = b#getBBX() in
			let min c d = if c < d then c else d in
			let max c d = if c > d then c else d in
			((min aminx bminx), (min aminy bminy),(max amaxx bmaxx),(max amaxy bmaxy))
		) ((List.hd m_pads)#getBBX()) m_pads in
		m_g#updateLayer true m_layer ; 
		m_g#setAlpha 0.125 ;
		m_g#empty(); 
		m_g#makeRectFloat ((lx +. hx)/.2.) ((ly +. hy)/.2.) ((hx -. lx)/.2.) ((hy -. ly)/.2.) ; 
		let g = !gclearance in
		m_drcBBX <- (lx -. g, ly -. g, hx +. g, hy +. g) ; 
		m_updateCallback () ; 
		(* don't need to rotate or translate that rect *)
	)
	method copy () = (
		(* makes a copy of all sub-objects.
		should be called after Oo.copy on this object 
		otherwise we keep references to each sub-object...*)
		let padsnew = List.map Oo.copy m_pads in
		List.iter (fun p -> p#copy()) padsnew;  (* update will (have to) be called later. *)
		m_pads <- padsnew; 
		let textsnew = List.map Oo.copy m_texts in
		List.iter (fun p -> p#copy()) textsnew;
		m_texts <- textsnew; 
		let shapesnew = List.map Oo.copy m_shapes in
		List.iter (fun p -> p#copy()) shapesnew;
		m_shapes <- shapesnew; 
		let shapes3dnew = List.map Oo.copy m_shapes3d in
		(* List.iter (fun p -> p#copy()) shapes3dnew; just a string. no need for deep copy *)
		m_shapes3d <- shapes3dnew; 
		m_g <- new grfx ; 
	)
	method updateLayers () = (
		List.iter (fun p -> p#updateLayers () ) m_pads ; 
		List.iter (fun p -> p#updateLayers () ) m_texts ; 
		List.iter (fun p -> p#updateLayers () ) m_shapes ; 
		m_g#updateLayer true m_layer ; 
	)
	method hasLayer lay = List.exists (fun p -> p#hasLayer lay) m_pads ; 
		(* module has the layer if at least one pad has the layer *)
		(* we don't worry about text here .. for now *)
	method getLayer () = m_layer
	
	method getRef () = (List.find (fun t -> t#getType() = 0 ) m_texts)#getText()
	method getValue () = (List.find (fun t -> t#getType() = 1 ) m_texts)#getText()
	method getLibRef () = m_libRef  (* aka the footprint *)
	method getFoot () = m_libRef
	method getPath () = m_path
	
	method setRef rf = (List.find (fun t -> t#getType() = 0 ) m_texts)#setText rf
	method setValue rf = (List.find (fun t -> t#getType() = 1 ) m_texts)#setText rf
	
	method setDeleteAttachedTracks b = m_deleteAttachedTracks <- b;
	method getDeleteAttachedTracks () = m_deleteAttachedTracks ;
	
	method clearHit () = m_hit <- false ; 
	
	method hit p hithold onlyworknet netnum hitsize hitz clearhit = (
		(* if any of the pads are hit, then this module is hit *)
		(* pads can only be hit if you are on the right layer *)
		(* modules can be hit from any layer fi you are in mod-move mode*)
		if not m_moving && m_visible then (
			(* texts can be moved semi-independently from the module, 
			hence need to check these independently. *)
			let (hittext, hitsize2, hitz2, clearhit2) = 
				if !gmode = Mode_MoveText then ( (*don't select texts in track modes.. *)
					List.fold_left 
					(fun (ht, hs, hz, clrhit) t -> t#hit (p, self#clearHit, ht, hs, hz, clrhit))
					(false, hitsize, hitz, clearhit) m_texts 
				) else (false, hitsize, hitz, clearhit)
			in
			(* now check everything that moves with the module - the body & the pads *)
			let (hitpad, netnum3, hitsize3, hitz3, clearhit3) = 
				List.fold_left (fun (hit,nn,siz,z,clrhit) pad ->
					pad#hit p onlyworknet hit nn siz z clrhit
				) (false, netnum, hitsize2, hitz2, clearhit2) m_pads
			in
			let ms = m_g#getBBXSize () in
			let mz = glayerZ.(m_layer) in
			let hitself = m_g#hit p && 
				( ms < hitsize3 ) &&
				!gmode <> Mode_MoveText in
			(* hold the hit signal if shift is depressed *)
			m_washit <- m_hit ; 
			m_hit <- hitself || hitpad || hittext || (hithold && m_washit); 
			if hitself then (
				clearhit3 () ; (*clear the previous hit record, we are smaller *)
				(netnum3, ms, mz, self#clearHit)
			) else 
				(netnum3, hitsize3, hitz3, clearhit3)
		) else (netnum, hitsize, hitz, clearhit)
	)
	method getHit () = m_hit 
	method setHit b = m_hit <- b ; 
	method txthit () =  (
		if m_visible then 
			try (List.find (fun t -> t#getHit() ) m_texts), true 
			with Not_found -> (List.hd m_texts), false
		else  (List.hd m_texts), false
	)
	method toggleTextShow () = (
		let txt,found = self#txthit() in
		if found then (
			let b = txt#getShow() in
			txt#setShow (not b) ; 
			txt# updateColor(); 
		); 
	)
	method crossprobe () = (
		if self#getHit () then (
			if not (List.exists (fun p -> p#crossprobe ()) m_pads) then (
				let txt = self#getRef() in
				let s = ("$PART: " ^ txt ^ "\n" ) in
				!gcrossprobe s ; 
			)
		)
	)
	method setMoving b = ( (* b is bool for moving or not *)
		let (txt, found) = self#txthit () in
		if b then (
			(* need to see if any of our texts are hit -- if so, 
			pass the move to them *)
			if found then (
				txt#setMoving b ; 
			) else (
				(*if we are starting a new move *)
				ignore( self#crossprobe () ); 
				m_move <- (0. , 0.); 
			)
		) else (
			(* if we were moved but are no longer now *)
			if found then (
				txt#setMoving b ; 
			) else (
				let (x, y) = m_move in
				m_x <- m_x +. x ; 
				m_y <- m_y +. y ;
				m_move <- (0. , 0.); 
				self#update () ; 
			)
		) ;
		m_moving <- b ; (*important!*)
		(* m_moving is true even if only one of our texts are moving. *)
	)
	method getMoving () = m_moving ; 
	method move m = (
		let (txt, found) = self#txthit() in
		if found then (
			txt#move m ;
		) else (
			m_move <- m ;
		)
	)
	method moveSum m = (
		m_move <- Pts2.add m_move m; 
	)
	method flip () = (
		(* flip about the X axis *)
		m_layer <- flip_layer m_layer ;  
		List.iter (fun p -> p#flip ()) m_pads; 
		List.iter (fun t -> t#flip ()) m_texts; 
		List.iter (fun s -> s#flip ()) m_shapes; 
		self#update ()
	)
	method getRot () = m_rot
	method setRot r = (
		m_rot <- r ; 
		List.iter (fun p -> p#setRot r) m_pads; 
		(*the shapes do not need to be rotated (apparently) *)
	)
	method getBBX movin = (
		let (ox,oy,ohx,ohy) = m_g#getBBX () in
		let (mx,my) = if movin then m_move else (0.0, 0.0) in
		ox +. mx, oy +. my, ohx +. mx, ohy +. my
	)
	method getCenter movin = (
		let (ox,oy,ohx,ohy) = m_g#getBBX () in
		let (mx,my) = if movin then m_move else (0.0, 0.0) in
		(ohx +. ox) /. 2. +. mx,  (ohy +. oy) /. 2. +. my
	)
	method rotate p = (
		if m_washit then (
			if !gmode = Mode_MoveText then (
				let (txt, found) = self#txthit() in
				if found then (
					txt#rotate () ; 
				)
			); 
			if !gmode = Mode_MoveModule then (
				(* don't rotate if we hit a pad - this is used for switching layers
				and it's too confusing to have it rotate modules too *)
				let (hitpad, _, _, _, _) = 
					List.fold_left (fun (hit,nn,siz,z,clrhit) pad ->
						pad#hit p false hit nn siz z clrhit
					) (false, 0, 100.0, -2.0, (fun ()->())) m_pads
				in
				if not hitpad then (
					let getOffset () = 
						Pts2.sub (self#getCenter false ) (m_x,m_y)
					in
					let (ofx, ofy) = getOffset () in
					self#setRot (m_rot + 900) ; 
					if m_rot > 3600 then m_rot <- m_rot - 3600 ;
					(* rotation by +90 deg is pretty simple -- *)
					let (px,py) = Pts2.sub (ofx,ofy) ((1.) *. ofy, (-1.) *. ofx) in
					m_x <- m_x +. px ; 
					m_y <- m_y +. py ; 
					let oldmove = m_move in
					self#update ();
					m_move <- oldmove ; (* the move is actually applied when you release the mouse.*)
					(* this lets you drag and rotate at the same time *)
				) else (
					printf "To rotate a module, click on the body an not any of the pads\n%!"; 
				)
			) ;
		) ; 
	)
	method draw bboxin = (
		let bbox = if m_moving then (-1e20, -1e20, 1e20, 1e20) else bboxin in
		(*  because the bounding box is not recalculated while moving, 
			hence module will disappear if you pan and move at the same time. *)
		if bbxIntersect bbox (m_g#getBBX()) && m_visible then (
			if m_moving then (
				GlMat.push () ; 
				GlMat.translate ~x:(fst m_move) ~y:(snd m_move) ~z:0. (); 
				List.iter (fun p -> p#move()) m_pads ; 
			) ;  
			m_g#draw ~hit:m_hit bbox;  
			(* update the module text before the pad text, so that we can clear *)
			if m_hit then (
				let s =  ref m_libRef in
				List.iter (fun t -> 
					let x = t#getText () in
					s := !s ^ " " ^ x ; 
				) m_texts ; 
				s := !s ^ "\n" ; 
				!ginfodisp( !s ) ; 
			) ; 
			List.iter (fun p -> p#draw bbox ) m_pads ; 
			List.iter (fun p -> p#draw bbox ) m_shapes ; 
			if !gdrawText then (List.iter (fun p -> p#draw bbox m_hit) m_texts ); 
			(* highlight the text if the module is selected *)
			(* this makes it a bit easier to see. *)
			if m_moving then (
				GlMat.pop () ; 
			); 
			(* m_washit <- ((move || hithold) && m_washit) || m_hit ; *)
		); 
	)
	method testdrc st en width2 net lay suggest = (
		(* let (pad, violation) = *)
		if m_visible then (
			try (List.find (fun p -> 
				p#testdrc st en width2 net lay suggest m_move
			) m_pads), true
			with Not_found -> (List.hd m_pads), false
		) else (List.hd m_pads), false
	)
	method getDrcBBX () = (
		if m_moving then bbxTranslate m_drcBBX m_move
		else m_drcBBX
	)
	method getTimeStamp () = ( m_TimeStamp )
	method setPath pth = ( m_path <- pth ; )
	method pathHas pth = ( Pcre.pmatch ~pat:pth m_path )
	method pathLast () = ( Pcre.extract ~pat:"\/([^\/]+)$" m_path ).(1)
	method pathSheet () =  ( Pcre.extract ~pat:"\/([^\/]+)\/[^\/]+$" m_path ).(1)
	method sheetNameHas sn = List.exists (fun p -> p#sheetNameHas sn) m_pads
	method textHas sn = (
		List.exists (fun t -> 
			Pcre.pmatch ~pat:sn (t#getText () ) ) m_texts
	)
	method getPos () = Pts2.add m_move (m_x,m_y)
	method setPos (x,y) = (
		m_x <- x; 
		m_y <- y; 
	)
	method getPadNets () = (
		List.filter (fun n -> n != 0) ( (* only return non-zero nets.  (0 = noconnect) *)
			List.map (fun p -> p#getNet () ) m_pads
		)
	)
	method getPadAssoc () = (
		(* return list that associates module timestamp ^ padname with netnum. *)
		(* again, only return non-zero associations. *)
		let modts = self#pathLast () in
		List.filter (fun (_, nn) -> nn != 0)
			(List.map (fun p-> 
				( ( modts ^ "_" ^ p#getPadName() ), p#getNet () ) 
			) m_pads )
	)
	method getPads () = m_pads
	method getTexts () = m_texts
	method read ic = (
		(*note: don't bother with the $MODULE <libref> line --
			the information is duplicated in the Li <libref> line *)
		let parse pattern  = 
			let line = input_line2 ic in
			Pcre.extract ~pat:pattern line
			(* with Not_found -> [| |] , false *)
		in
		let parse_line1  = 
			let sp = parse "Po ([\d-]+) ([\d-]+) (\d+) (\d+) (\w+) (\w+) ([^ ]+)" in
			m_x <- fois (ios sp.(1)); 
			m_y <- fois (ios sp.(2)); 
			m_rot <- ios sp.(3); 
			m_layer <- ios sp.(4);
			m_LastEditTime <- sp.(5); 
			m_TimeStamp <- sp.(6); 
			m_statusText <- sp.(7); 
		in
		let parse_line2 = 
			let sp = parse "Li (.+)" in
			m_libRef <- sp.(1) ; 
		in
		let parse_line3 = 
			(*note, timestamp already read in the first line *)
			(*there are two optional entries here : 
			Cd = documentation. 
			Kw = keyword. *)
			let d = ref (input_line2 ic) in  
			while not (Pcre.pmatch ~pat:"^Sc" !d) do (
				m_doc <- (try (Pcre.extract ~pat:"^Cd (.+)" !d).(1)
					with Not_found -> m_doc ); 
				m_keyword <- (try (Pcre.extract ~pat:"^Kw (.+)" !d).(1)
					with Not_found -> m_keyword ); 
				d := input_line2 ic ; 
			) done;
			(* m_timestamp <- try (Pcre.extract ~pat:"^Sc (.+)" !d).(1)
					with Not_found -> "" ; *)
		in
		let parse_line4 = 
			try 
				let sp = parse "AR (.+)" in
				m_path <- sp.(1) ; 
			with Not_found -> ()  (*for modules that are inserted after schematic, AR will be blank *)
		in
		let parse_line5 = 
			let sp = parse "Op (\d+) (\d+)" in
			m_cntRot90 <- ios sp.(1) ; 
			m_cntRot180 <- ios sp.(2) ;
		in
		let parse_remaining = 
			let line = ref (input_line2 ic) in
			let add_shape _ = 
				let shape = new pcb_shape in
				shape#read ic !line Shape_Segment ; 
				m_shapes <- (shape :: m_shapes) ; 
			in
			while not (Pcre.pmatch ~pat:"\$EndMODULE" !line ) do
			(
				let c = (Pcre.extract ~pat:"^([^ \d]+)" !line).(1) in
				(
				match c with 
					| "$PAD" -> (
						let pad = new pcb_pad in
						pad#read ic; 
						m_pads <- (pad :: m_pads ) ; 
						)
					| "$SHAPE" -> (
						let sh3 = new shape3d in
						sh3#read ic ; 
						m_shapes3d <- sh3 :: m_shapes3d ;
						)
					| "T" -> ( 
						let text = new pcb_modtext in
						text#read !line m_rot; 
						m_texts <- (text :: m_texts) ; 
						)
					| "At" -> (
						m_attributes <- (Pcre.extract ~pat:"^At (.+)" !line).(1) ; 
					)
					| "DS" -> add_shape Shape_Segment ;
					| "DC" -> add_shape Shape_Circle ; 
					| "DA" -> add_shape Shape_Arc ; 
					| "DP" -> add_shape Shape_Polygon ; 
					| _ -> () 
				); 
				line := input_line2 ic ; 
			)
			done; 
		in
		parse_line1 ; 
		parse_line2 ; 
		parse_line3 ; 
		parse_line4 ; 
		parse_line5 ;
		parse_remaining ; 
	)
	method save oc = (
		fprintf oc "$MODULE %s\n" m_libRef ; 
		fprintf oc "Po %d %d %d %d %s %s %s\n" 
			(iofs m_x) (iofs m_y) m_rot m_layer m_LastEditTime m_TimeStamp m_statusText ; 
		fprintf oc "Li %s\n" m_libRef ; 
		if (String.length m_doc) > 0 then (
			fprintf oc "Cd %s\n" m_doc ; 
		) ; 
		if (String.length m_keyword) > 0 then (
			fprintf oc "Kw %s\n" m_keyword ; 
		) ; 
		fprintf oc "Sc %s\n" m_TimeStamp ; 
		fprintf oc "AR %s\n" m_path ; 
		fprintf oc "Op %d %d 0\n" m_cntRot90 m_cntRot180 ; 
		if (String.length m_attributes) > 0 then (
			fprintf oc "At %s\n" m_attributes ; 
		) ; 
		List.iter (fun t -> t#save oc) (List.rev m_texts) ; 
		List.iter (fun s -> s#save oc) (List.rev m_shapes) ; 
		List.iter (fun p -> p#save oc) (List.rev m_pads) ; 
		List.iter (fun s -> s#save oc) (List.rev m_shapes3d) ; 
		fprintf oc "$EndMODULE  %s\n" m_libRef ; (*two spaces there for some reason *)
		flush oc ; 
	)
	method edit top = (
		(* right now we can only edit text .. *)
		let (txt, found) = self#txthit () in
		if found then txt#edit top ; 
	)
	method editValue top = (
		(* change the module's value.. *)
		if m_hit then (
			(* text type 1 is value. *)
			let t = try Some (List.find (fun txt -> txt#getType () = 1) m_texts)
				with _ -> None in
			(match t with 
				| Some tt -> tt#edit top
				| None -> ()
			)
		)
	)
end;;