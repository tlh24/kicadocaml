open Printf
open Comm
open Grfx

type pcb_pad_shape = Pad_Circle | Pad_Rect | Pad_Oval | Pad_Trapezoid
type pcb_pad_type = Pad_STD | Pad_SMD | Pad_CONN | Pad_HOLE | Pad_MECA
	
class pcb_pad = 
object (self)
	val mutable m_padname = ""
	val mutable m_shape : 'pcb_pad_shape = Pad_Circle
	val mutable m_x = 0
	val mutable m_y = 0
	val mutable m_sx = 0
	val mutable m_sy = 0
	val mutable m_dsx = 0 (*delta size, not sure what it's for*)
	val mutable m_dsy = 0
	val mutable m_rot = 0
	val mutable m_drill = 0
	val mutable m_drillOffX = 0
	val mutable m_drillOffY = 0
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
	method update x y = (
		m_g#updateLayers m_layers ; (* this sets the color *)
		(* make ground pads a little greenish *)
		if m_netname = "GND" then (
			let cr,cg,cb = m_g#getColor() in
			m_g#setColor (cr -. 0.1, cg +. 0.1, cb -. 0.1) ;
		); 
		m_g#empty(); 
		m_gtext#empty(); 
		(* m_gtext#setColor (0.4, 0.85, 0.25) ;  (* has no layer so set color manually *) *)
		m_gtext#setColor (0.0, 0.0, 0.0) ;  (* has no layer so set color manually *)
		(
		match m_shape with
			| Pad_Rect -> m_g#makeRect m_x m_y m_sx m_sy ; 
			| Pad_Oval -> (
				match m_drill with 
					| 0 -> m_g#makeOval (Pts2.foist (m_x,m_y)) 
						(fois m_sx) (fois m_sy) ; 
					| _ -> m_g#makeOvalRing 16 (Pts2.foist (m_x,m_y)) 
						(fois m_sx) (fois m_sy) (fois m_drill); 
				)
				
			| _ -> (
				match m_drill with 
					| 0 -> m_g#makeCircle m_x m_y m_sx m_sy ; 
					| _ -> m_g#makeRing (Pts2.foist (m_x,m_y)) m_drill m_sx ; 
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
		m_gtext#makeText m_x m_y 0 0 0 0 
			(sx2-. width) 
			(sx2 *. 1.33 -. width) 
			width m_padname ; 
			
		m_g#rotateTranslate m_rot x y ; 
		m_gtext#rotateTranslate m_rot x y ; 
		m_moveCallback () ; 
	)
	method updateLayers () = m_g#updateLayers m_layers ; 
	method copy () = (
		m_g <- new grfx; 
		m_gtext <- new grfx;
		(* we can keep refs to the other instance variables. *)
	)
	method hit (p, onlyworknet, ja, netnum) = 
		m_washit <- m_hit ; 
		(*don't hit if we are not displayed *)
		let (en,active) = List.fold_left (fun (b,c) lay -> 
			( (b || glayerEn.(lay)),(c || !glayer = lay) )
		) (false,false) m_layers in
		(* don't update the hit variable if mouse button 1 is down *)
		m_hit <- (m_g#hit p) && en && (not onlyworknet || netnum = m_netnum); 
		if m_hit && (not m_washit) then (
			(*then this is a new 'hit' - do cross probing. *)
			(* only do cross=probing on the pins ; 
			cross-probing on the modules is too annoying *)
			let s = ("$PART: " ^ m_part ^ "\n" ^ "$PAD: " ^ m_padname ^ "\n" ) in
			!gcrossprobe s ; 
		); 
		if m_hit && active then (
			(* print_endline "snapped to pad!" ; *)
			gsnapped := bbxCenter ( m_g#getBBX() ) ;
		) ;
		if m_hit then true, m_netnum
		else ja, netnum
		
	method pointInPad p = (
		(* need to rotate the point into our coordinate system *)
		(* it is assumed that it is already translated into our coord sys *)
		let a = (foi m_rot) /. -572.9577951308 in
		let (px,py) = rotate2 ~angle:a p in
		let x = fois m_x in
		let y = fois m_y in
		let sx = (fois m_sx) *. 0.5 in
		let sy = (fois m_sy) *. 0.5 in
		let hit = match m_shape with 
			| Pad_Rect -> ( x -. sx < px && px < x +. sx && y -. sy < py && py < y +. sy)
			| _ -> (Pts2.distance (px,py) (x,y)) < sx
		in
		(* return boolean hit + where to snap to *)
		(* don't forget to rotate back into world-coords! *)
		(hit,( rotate2 ~angle:(-1. *. a) (x,y)) )
	)
	method hasLayer lay = List.exists ((=) lay) m_layers ; 
	method addLayer lay = m_layers <- lay :: m_layers; 
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
		if m_highlight then (
			m_highlight <- false; 
			m_g#draw ~hit:true ~hcolor:(0.4, 1., 1.) bbox ; (* cyan, for DRC *)
		) else (
			m_g#draw ~hit:m_hit bbox ; 
		);
		if m_hit then (
			!ginfodispappend( "pad:" ^ m_padname ^ " net:" ^ (string_of_int m_netnum) ^ 
			" netname:" ^ m_netname ^ "\n") ;
		) ; 
		if !gshowPadNumbers then (
			(* also should push it to the foreground, in case depth buffer is on. *)
			GlMat.push() ; 
			GlMat.translate ~x:(0.0) ~y:(0.0) ~z:(0.99) (); 
			m_gtext#draw ~hit:m_hit bbox ; 
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
		let c = Pts2.add move (bbxCenter (m_g#getBBX() ) ) in
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
			let width = ((fois m_sx) *. 0.5) in
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
				| Pad_Oval -> testrect () (* yea.. really should do two circles and one rect .. eh. *)
				| _ -> testcircle ()
		) else false
	)
	method setRot r = m_rot <- r ; 
	method getNet () = m_netnum 
	method setNet n = m_netnum <- n ;
	method setNetName nn = m_netname <- nn ; 
	method getPadName () = m_padname 
	method getShape () = m_shape 
	method getPos () = Pts2.fois m_x m_y
	method getSx () = if m_rot = 900 || m_rot = 2700 then fois m_sy else fois m_sx 
	method getSy () = if m_rot = 900 || m_rot = 2700 then fois m_sx else fois m_sy 
	method getDrill () = fois m_drill ; 
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
	method read ic = (
		let parse pattern = 
			let line = input_line2 ic in
			Pcre.extract ~pat:pattern line
		in
		(* first line should be `Sh "2" R 236 354 0 0 900` *)
		let parse_line1 = 
			let sp = parse "Sh \"([^\"]*)\" (\w) (\d+) (\d+) (\d+) (\d+) (\d+)"  in
			m_padname <- sp.(1); 
			m_shape <- (
				match sp.(2) with
					| "C" -> Pad_Circle
					| "R" -> Pad_Rect
					| "O" -> Pad_Oval
					| "T" -> Pad_Trapezoid
					| _    -> Pad_Circle
			);
			m_sx <- ios sp.(3) ; 
			m_sy <- ios sp.(4) ; 
			m_dsx <- ios sp.(5) ; 
			m_dsy <- ios sp.(6) ; 
			m_rot <- ios sp.(7) ; 
		in
		let parse_line2 = 
			let sp = parse "Dr (\d+) (\d+) (\d+)" in
			m_drill <- ios sp.(1) ; 
			m_drillOffX <- ios sp.(2) ; 
			m_drillOffY <- ios sp.(3) ; 
			(* warning -- I'm not handling oval pads here! *)
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
			m_x <- ios sp.(1) ;
			m_y <- ios sp.(2) ; 
		in
		parse_line1; 
		parse_line2; 
		parse_line3; 
		parse_line4; 
		parse_line5; 
		ignore( input_line2 ic); (*read the last line, $EndPAD *)
		(*function should return unit *)
	)
	method save oc = (
		fprintf oc "$PAD\n" ; 
		fprintf oc "Sh \"%s\" %s %d %d %d %d %d\n"
			m_padname 
			(match m_shape with 
				| Pad_Circle -> "C"
				| Pad_Rect -> "R" 
				| Pad_Oval -> "O"
				| Pad_Trapezoid -> "T" )
			m_sx m_sy m_dsx m_dsy m_rot ; 
		fprintf oc "Dr %d %d %d\n" m_drill m_drillOffX m_drillOffY ; 
		fprintf oc "At %s N %8.8lX\n"
			(match m_type with
				| Pad_STD -> "STD" 
				| Pad_SMD -> "SMD"
				| Pad_CONN -> "CONN"
				| Pad_HOLE -> "HOLE"
				| Pad_MECA -> "MECA" )
			(layers_to_int32 m_layers) ; 
		fprintf oc "Ne %d \"%s\"\n" m_netnum m_netname ; 
		fprintf oc "Po %d %d\n" m_x m_y ; 
		fprintf oc "$EndPAD\n"
	)
end;;