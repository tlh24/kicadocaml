open Printf
open Comm
open Grfx

type pcb_pad_shape = Pad_Circle | Pad_Rect | Pad_Oval | Pad_Trapezoid
type pcb_pad_type = Pad_STD | Pad_SMD | Pad_CONN | Pad_HOLE | Pad_MECA
	
class pcb_pad = 
object (self)
	val mutable m_padname = ""
	val mutable m_shape : 'pcb_pad_shape = Pad_Circle
	val mutable m_x = 0.
	val mutable m_y = 0.
	val mutable m_sx = 0.
	val mutable m_sy = 0.
	val mutable m_dsx = 0 (*delta size, not sure what it's for*)
	val mutable m_dsy = 0
	val mutable m_rot = 0
	val mutable m_drill = 0.
	val mutable m_drillOffX = 0.
	val mutable m_drillOffY = 0.
	val mutable m_drillSx = 0.
	val mutable m_drillSy = 0.
	val mutable m_type : 'pcb_pad_type = Pad_STD
	val mutable m_layers : int list = [] 
	val mutable m_netnum = 0
	val mutable m_netname = ""
	val mutable m_part = ""
	val mutable m_g = new grfx
	val mutable m_gtext = new grfx
	val mutable m_moveCallback = (fun () -> () )
	val mutable m_selCallback = (fun () -> () )
	val mutable m_hit = false
	val mutable m_washit = false 
	val mutable m_highlight = false 
	val mutable m_connect = false
	method getConnect() = m_connect
	method setConnect b = m_connect <- b
	method setHighlight b = m_highlight <- b ;
	method getZ () = m_g#getZ ()
	
	method update rot x y = (
		m_g#updateLayers m_layers ; (* this sets the color *)
		(* make ground pads a little greenish *)
		if m_netname = "GND" then (
			let cr,cg,cb = m_g#getColor() in
			m_g#setColor (cr -. 0.1, cg +. 0.1, cb -. 0.1) ;
		); 
		m_g#empty(); 
		m_gtext#empty(); 
		m_gtext#setColor (0.0, 0.0, 0.0) ;  (* has no layer so set color manually *)
		(
		match m_shape with
			| Pad_Rect -> m_g#makeRect 0.0 0.0 m_sx m_sy ; 
			| Pad_Oval -> (
				match m_drill with 
					| 0. -> m_g#makeOval (0.0,0.0) m_sx m_sy ;
					| _ -> m_g#makeOvalRing 16 (0.0,0.0) m_sx m_sy m_drill ;
				)
				
			| _ -> (
				match m_drill with 
					| 0. -> m_g#makeCircle 0.0 0.0 m_sx m_sy ; 
					| _ -> m_g#makeRing (0.0,0.0) m_drill m_sx ; 
				)
		) ; 
		
		(* get the bounding box so we know how big to make the text *)
		let (minx,miny,maxx,maxy) = m_g#getBBX () in
		let dx = maxx -. minx in 
		let dy = maxy -. miny in
		let sx = dx /. (1.5 *. foi (String.length m_padname)) in
		let sx2 = min (dy /. 1.5) sx in
		let width = sx2 /. 10.0 in
		(* making text actually translates the other (actual) pad *)
		m_gtext#makeText 0.0 0.0 0 0. 0. 0 
			(sx2-. width) 
			(sx2 *. 1.33 -. width) 
			width m_padname ; 
		m_g#rotateTranslate m_rot m_x m_y; 
		m_g#rotateTranslate rot x y ; 
		m_gtext#rotateTranslate m_rot m_x m_y; 
		m_gtext#rotateTranslate rot x y ; 
		m_moveCallback () ; 
	)
	method updateLayers () = m_g#updateLayers m_layers ; 
	method copy () = (
		m_g <- new grfx; 
		m_gtext <- new grfx;
		(* we can keep refs to the other instance variables. *)
	)
	method clearHit () = (* printf "clearhit %s\n%!" m_padname;*) m_hit <- false ;
	method getHit () = m_hit ; 
	method hit p onlyworknet ja netnum hitsize hitz clearhitin = (
		m_washit <- m_hit ; 
		let ms = m_g#getBBXSize () in
		let mz = m_g#getZ () in
		if ( mz > hitz || (mz = hitz && ms < hitsize) ) then (
			(*don't hit if we are not displayed*)
			let en = List.fold_left (fun b lay -> (b || glayerEn.(lay))
			) false m_layers in
			(* don't update the hit variable if mouse button 1 is down *)
			m_hit <- (m_g#hit p) && en && (not onlyworknet || netnum = m_netnum); 
			if m_hit then (
				(* printf "snapped to pad %s\n%!" m_padname; *)
				clearhitin (); (*clear the previous hit record, we are smaller *)
				gsnapped := bbxCenter ( m_g#getBBX() ) ;
				true, m_netnum, ms, mz, self#clearHit
			)else ja, netnum, hitsize, hitz, clearhitin
		)else ja, netnum, hitsize, hitz, clearhitin
	)
	method crossprobe () = (
		if m_hit then (
			let s = ("$PART: " ^ m_part ^ "\n" ^ "$PAD: " ^ m_padname ^ "\n" ) in
			!gcrossprobe s ; 
			true
		) else false
	)
	method pointInPad rot p = (
		(* need to rotate the point into our coordinate system *)
		(* it is assumed that it is already translated into our coord sys *)
		(* coordinate system: world -> module -> pad *)
		let a = (foi (rot)) /. -572.9577951308 in (* remove module rotation (negative) *)
		let (px,py) = rotate2 ~angle:a p in
		let sx = m_sx *. 0.5 in
		let sy = m_sy *. 0.5 in
		(* remove pad rotation *)
		let a2 = (foi (m_rot)) /. -572.9577951308 in 
		let (qx,qy) = rotate2 ~angle:a2 (Pts2.sub (px,py) (m_x,m_y))in
		let hit = match m_shape with 
			| Pad_Rect
			| Pad_Oval -> ( qx < sx && qx > (-1.0 *. sx) && qy < sy && qy > (-1.0 *. sy) )
			| _ -> (Pts2.length (qx,qy) ) < sx
		in
		(* return boolean hit + where to snap to *)
		(* don't forget to rotate back into world-coords! *)
		(hit,( rotate2 ~angle:(-1. *. a) (m_x,m_y)) ) (* really should make this more consistent *)
	)
	method getLayers () = m_layers
	method hasLayer lay = List.exists ((=) lay) m_layers ; 
	method addLayer lay = m_layers <- lay :: m_layers; 
	method getLayer () = ( (* just returns the first copper layer - not useful for through-hole*)
		List.find (fun l -> l>=0 && l<=15) m_layers
	)
	method draw bbox = (
		if !gcurnet = m_netnum && !gcurnet != 0 then (
			m_g#setAlpha 0.78 ;
			m_gtext#setAlpha 0.68 
		) else ( 
			if m_netnum != 0 then (
				m_g#setAlpha 0.55 ;
				m_gtext#setAlpha 0.45 
			) else (
				m_g#setAlpha 0.35 ;
				m_gtext#setAlpha 0.25 
			)
		); 
		ignore(if m_highlight then (
			m_highlight <- false; 
			m_g#draw ~hit:true ~hcolor:(0.4, 1., 1.) bbox ; (* cyan, for DRC *)
		) else (
			m_g#draw ~hit:m_hit bbox ; 
		));
		if m_hit then (
			!ginfodispappend( "pad:" ^ m_padname ^ " net:" ^ (string_of_int m_netnum) ^ 
			" netname:" ^ m_netname ^ "\n") ;
		) ; 
		if !gshowPadNumbers then (
			(* also should push it to the foreground, in case depth buffer is on. *)
			GlMat.push() ; 
			GlMat.translate ~x:(0.0) ~y:(0.0) ~z:(1.0/.80.0) (); 
			ignore(m_gtext#draw ~hit:m_hit bbox); 
			GlMat.pop(); 
		); 
		if m_hit then self#select () ; 
	)
	method sheetNameHas sn = Pcre.pmatch ~pat:sn m_netname
	method testdrc st en width2 net lay suggest move = (
		(* returns bool DRC error *)
		(* also returns the two points of error in 'suggest', the first corresponding to the 
		track , and the second to the pad *)
		(* width2 is half the actual track width *)
		(* have to use the bounding box here *)
		let c = Pts2.add move (bbxCenter (m_g#getBBX() ) ) in (* this in global coords, so ok*)
		let (xl2,yl2,xh2,yh2) = m_g#getBBX() in
		let xl,yl = Pts2.add (xl2,yl2) move in
		let xh,yh = Pts2.add (xh2,yh2) move in
		let lengthzero = Pts2.distance2 st en = 0. in
		let testrect () = 
			(* the algorithms should robustly handle both tracks and vias *)
			let ar = [|
				(Pts2.linedistance st en (xl,yl) (xl,yh) ) ;  (*vertical left *)
				(Pts2.linedistance st en (xh,yl) (xh,yh) ) ; (* vertical right *)
				(Pts2.linedistance st en (xl,yl) (xh,yl) ) ; (* horizontal bottom *)
				(Pts2.linedistance st en (xl,yh) (xh,yh) )  (* horizontal top *)
				|] in
			let min = ref 1e24 in
			let ee = ref (0. ,0.) in
			let ff = ref (0. ,0.) in
			Array.iter (fun (l,e,f) -> 
				if l < !min then (
					min := l; 
					ee := e; 
					ff := f; 
				); 
			) ar; 
			(* print_endline ("pad testdrc: min = " ^ (sof !min)) ; *)
			let violation = !min < width2 +. !gclearance  in
			if violation then (
				(* adjust e to satisfy DRC *)
				let fe = Pts2.norm (Pts2.sub !ee !ff) in
				let e = Pts2.add !ff (Pts2.scl fe (width2 +. !gclearance +. 0.0001)) in
				suggest := (e,!ff); 
			); 
			violation
		in
		let testcircle () = 
			let d,e,f = Pts2.closestpointdistance st en c false in
			let width = (m_sx *. 0.5) in
			let violation = d < width +. width2 +. !gclearance in
			if violation then (
				let fe = Pts2.norm (Pts2.sub e f) in
				let e = Pts2.add f (Pts2.scl fe (width +. width2 +. !gclearance)) in
				suggest := (e,f); 
			); 
			violation
		in
		if net != m_netnum &&  ((List.exists (fun n -> n=lay) m_layers) || lengthzero) then (
			match m_shape with
				| Pad_Rect -> testrect () 
				| Pad_Oval -> testrect () (* yea.. really should do two circles and one rect .
						or an appropriately sized track, 
						also, need this to work with pad rotations, bleh *)
				| _ -> testcircle ()
		) else false
	)
	method getNet () = m_netnum 
	method setNet n = m_netnum <- n ;
	method setNetName nn = m_netname <- nn ; 
	method getPadName () = m_padname 
	method getShape () = m_shape 
	method getPos () = m_x,m_y
	method getSx () = if m_rot = 900 || m_rot = 2700 then m_sy else m_sx 
	method getSy () = if m_rot = 900 || m_rot = 2700 then m_sx else m_sy 
	method getDrill () = m_drill ; 
	method getBBX () = m_g#getBBX()
	method getCenter () = (bbxCenter (m_g#getBBX() ) ) 
	method setMoveCallback f = m_moveCallback <- f 
	method setSelCallback f = m_selCallback <- f 
	method select () = m_selCallback () ; 
	method setPart s = m_part <- s 
	(* move just updates the rats' nest *)
	(* call update to actually move the pad *)
	method move () = 
		m_moveCallback () ; 
		m_selCallback () ; 
	method flip () = (
		(* flip about the X-axis, ala pcbnew *)
		(* update the layers, too. *)
		m_y <- m_y *. (-1.); 
		m_drillOffY <- m_drillOffY *. (-1.); 
		m_layers <- List.map flip_layer m_layers ; 
		(* the callee is responsible for calling update w/ appropriate params *)
	)
	method read ic rot = (
		(* rot is the parent module rotation - 
		rotations are in global coordinates in the file *)
		let parse pattern = 
			let line = input_line2 ic in
			Pcre.extract ~pat:pattern line
		in
		(* first line should be eg. `Sh "2" R 236 354 0 0 900` *)
		let parse_line1 = 
			let sp = parse "Sh \"([^\"]*)\" (\w) (\d+) (\d+) (\d+) (\d+) ([-\d]+)"  in
			m_padname <- sp.(1); 
			m_shape <- (
				match sp.(2) with
					| "C" -> Pad_Circle
					| "R" -> Pad_Rect
					| "O" -> Pad_Oval
					| "T" -> Pad_Trapezoid
					| _    -> Pad_Circle
			);
			m_sx <- fois (ios sp.(3)); 
			m_sy <- fois (ios sp.(4)); 
			m_dsx <- ios sp.(5) ; 
			m_dsy <- ios sp.(6) ; 
			m_rot <- (ios sp.(7)) - rot ; 
			if m_rot < 0 then m_rot <- m_rot + 3600 ; 
			if m_rot > 3600 then m_rot <- m_rot - 3600 ; 
		in
		let parse_line2 = 
			let line = input_line2 ic in
			try (
				let sp = Pcre.extract ~pat:"Dr (\d+) ([-\d]+) ([-\d]+) (\d+) (\d+)" line in
				m_drill <- fois (ios sp.(1)); 
				m_drillOffX <- fois (ios sp.(2)); 
				m_drillOffY <- fois (ios sp.(3)); 
				m_drillSx <- fois (ios sp.(4)); 
				m_drillSy <- fois (ios sp.(5)); 
			) with _ -> (
				let sp = Pcre.extract ~pat:"Dr (\d+) ([-\d]+) ([-\d]+)" line in
				m_drill <- fois (ios sp.(1)); 
				m_drillOffX <- fois (ios sp.(2)); 
				m_drillOffY <- fois (ios sp.(3)); 
				m_drillSx <- 0.0; 
				m_drillSy <- 0.0; 
			)
			(* warning -- I'm not handling oval drill here! *)
		in
		let parse_line3 = 
			let sp = parse "At (\w+) N ([\d\w]+)" in
			m_type <- (
				match sp.(1) with
					| "STD" -> Pad_STD
					| "SMD" -> Pad_SMD
					| "CONN" -> Pad_CONN
					| "HOLE" -> Pad_HOLE
					| "MECA" -> Pad_MECA
					| _ -> Pad_STD
			); 
			m_layers <- string_to_layers sp.(2); 
			(*print_endline ( "found pad with the following layers : " ^ sp.(2) ); *)
			(*List.iter (fun s -> print_endline( ": " ^ (string_of_int s))) m_layers *)
		in
		let parse_line4 = 
			let sp = parse "Ne (\d+) \"([^\"]*)\"" in
			m_netnum <- ios sp.(1) ; 
			m_netname <- sp.(2) ; 
		in
		let parse_line5 =
			let sp = parse "Po ([\d-]+) ([\d-]+)" in
			m_x <- fois(ios sp.(1)) ;
			m_y <- fois(ios sp.(2)) ; 
		in
		parse_line1; 
		parse_line2; 
		parse_line3; 
		parse_line4; 
		parse_line5; 
		ignore( input_line2 ic); (*read the last line, $EndPAD *)
		(*function should return unit *)
	)
	method save oc rot = (
		let ra = m_rot + rot in (* convert back to global coords *)
		let rb = mod2 ra 3600 in
		fprintf oc "$PAD\n" ; 
		fprintf oc "Sh \"%s\" %s %d %d %d %d %d\n"
			m_padname 
			(match m_shape with 
				| Pad_Circle -> "C"
				| Pad_Rect -> "R" 
				| Pad_Oval -> "O"
				| Pad_Trapezoid -> "T" )
			(iofs m_sx) (iofs m_sy) m_dsx m_dsy rb ; 
		fprintf oc "Dr %d %d %d" (iofs m_drill) (iofs m_drillOffX) (iofs m_drillOffY) ; 
		if m_shape = Pad_Oval then fprintf oc " %d %d" (iofs m_drillSx) (iofs m_drillSy); 
		fprintf oc "\n"; 
		fprintf oc "At %s N %8.8lX\n"
			(match m_type with
				| Pad_STD -> "STD" 
				| Pad_SMD -> "SMD"
				| Pad_CONN -> "CONN"
				| Pad_HOLE -> "HOLE"
				| Pad_MECA -> "MECA" )
			(layers_to_int32 m_layers) ; 
		fprintf oc "Ne %d \"%s\"\n" m_netnum m_netname ; 
		fprintf oc "Po %d %d\n" (iofs m_x) (iofs m_y) ; 
		fprintf oc "$EndPAD\n"
	)
end;;