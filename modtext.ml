open Printf
open Comm
open Grfx

class pcb_modtext = 
object (self)
	val mutable m_type = 0
	val mutable m_x = 0 (* our location relative to the parent module *)
	val mutable m_y = 0
	val mutable m_sx = 0
	val mutable m_sy = 0
	val mutable m_rot = 0
	val mutable m_ox = 0 (*module offset (location)*)
	val mutable m_oy = 0 (*module offset*)
	val mutable m_orot = 0 (*owning module rotation*)
	val mutable m_width = 0
	val mutable m_mirror = false
	val mutable m_show = true
	val mutable m_layer = 0
	val mutable m_textMirror = ""
	val mutable m_text = ""
	val mutable m_g = new grfx
	val mutable m_hit = false
	val mutable m_washit = false
	val mutable m_moving = false
	val mutable m_move = (0. , 0.)
	method updateColor () = 
		if m_show then (
			m_g#setAlpha 0.5
		) else (
			m_g#setAlpha 0.15
		)
	method update rot ox oy = (
		m_g#empty () ; 
		self#updateLayers () ; 
		m_orot <- rot ; 
		m_ox <- ox; 
		m_oy <- oy; 
		m_g#makeText m_x m_y m_rot m_ox m_oy m_orot (fois m_sx) (fois m_sy) 
			((fois m_width) *. 0.5) m_text ; 
		self#updateColor(); 
	)
	method update2 () = 
		self#update m_orot m_ox m_oy ; 
	method updateLayers () = 
		(* if we are hidden, set Z to 0.001 *)
		m_g#updateLayer (not m_show) m_layer ;
	method getText () = m_text
	method getType () = m_type
	method getSize () = Pts2.fois m_sx m_sy
	method setSize (x,y) = 
		m_sx <- iofs x; 
		m_sy <- iofs y; 
	method getPos () = Pts2.fois m_x m_y
	method setPos (x,y) = 
		m_x <- iofs x; 
		m_y <- iofs y; 
	method getWidth () = fois m_width ; 
	method setWidth w = m_width <- iofs w ; 
	method getShow () = m_show; 
	method setShow b = m_show <- b; 
	method getRot () = m_rot; 
	method setRot b = m_rot <- b; 
	
	method hit (p, (superClearHit:unit->unit), ja, hitsize, clearhit) = (
		if not m_moving && glayerEn.(m_layer) then (
			let selfsize = m_g#getBBXSize () in
			if selfsize < hitsize then (
				m_washit <- m_hit ; 
				m_hit <- (m_g#hit p); 
				if m_hit then (
					clearhit () ; (*clear the previous hit record, we are smaller *)
					let hid = if not m_show then "hidden," else "" in
					let s = Printf.sprintf "text,%s sx:%4.4f sy:%4.4f w:%4.4f \n%s" 
						hid (fois m_sx) (fois m_sy) (fois m_width) m_text  in
					!ginfodisp s; 
					(* make a custom function that clears both us & our parent module 
					if we happen to be overruled (have the hit variable cleared *)
					(true, selfsize, (fun () -> self#clearHit(); superClearHit()) )
				) else ( 
					(ja, hitsize, clearhit) 
				)
			) else (ja, hitsize, clearhit) 
		) else (ja, hitsize, clearhit) 
	)
	method clearHit () = m_hit <- false ; 
	method getHit () = m_hit
	method draw bbox highlight = (
		if m_moving then ( 
			GlMat.push(); 
			GlMat.translate ~x:(fst m_move) ~y:(snd m_move) ~z:0. (); 
		); 
		m_g#draw ~hit:(m_hit || highlight) bbox; 
		if m_moving then (
			GlMat.pop() ; 
		) ; 
	)
	method setMoving b = (
		if b then (
			m_move <- (0. , 0.); 
		) else (
			let a = (foi m_orot) /. -572.9577951 in
			let (x, y) = Pts2.iofs( rotate2 ~angle:a m_move) in
			m_x <- m_x + x ; 
			m_y <- m_y + y ;
			m_move <- (0. , 0.); 
			self#update m_orot m_ox m_oy ; 
		);
		m_moving <- b ; (*important!*)
	)
	method move m = m_move <- m ;
	method rotate () = (
		m_rot <- (m_rot + 900); 
		if m_rot > 3600 then m_rot <- m_rot - 3600 ;
		let oldmove = m_move in
		self#update m_orot m_ox m_oy ; 
		m_move <- oldmove ; (* the move is actually applied when you release the mouse.*)
	)
	method read line orot = (
		let parse_line1 = 
			let sp =  Pcre.extract ~pat:"T(\d) ([\d-]+) ([\d-]+) (\d+) (\d+) (\d+) (\d+) (\w+) (\w+) (\d+)[^\"]+\"([^\"]+)\"" line in
			m_type <- ios sp.(1) ; 
			m_x <- ios sp.(2) ;
			m_y <- ios sp.(3) ;
			m_sx <- ios sp.(4) ;
			m_sy <- ios sp.(5) ;
			m_rot <- (ios sp.(6)) - orot ; (* this is the way pcbnew saves the board files *)
			m_width <- ios sp.(7) ;
			m_mirror <- (
				match sp.(8) with
					| "N" -> true
					| _ -> false
				); 
			m_show <- (
				match sp.(9) with 
					| "I" -> false
					| _ -> true
				); 
			m_layer <- ios sp.(10) ;
			m_textMirror <- "N" ; 
			m_text <- sp.(11) ; 
		in
		parse_line1 ; 
	)
	method save oc = (
		fprintf oc "T%d %d %d %d %d %d %d %s %s %d %s\"%s\"\n" 
			m_type m_x m_y m_sx m_sy (m_rot+m_orot) m_width
			(if m_mirror then "N" else "M" )
			(if m_show then "V" else "I")
			m_layer m_textMirror m_text
	)
	method edit (top:Widget.toplevel Widget.widget) = (
		let fs = Toplevel.create top in
		Wm.title_set fs "edit text" ; 
		(* radiobuttons for show/hide *)
		let showframe = Frame.create fs in
		let showvar = Textvariable.create ~on:showframe () in
		Textvariable.set showvar (if m_show then "show" else "hide" );
		let showcallback () = 
			m_show <- ((Textvariable.get showvar)="show") ; 
		in
		let mkradio label = 
			Radiobutton.create ~indicatoron:true ~text:label 
			~value:label ~variable:showvar 
			~command:showcallback (* this one is immediately applied *)
			showframe
		in
		let show = mkradio "show" in
		let hide = mkradio "hide" in
		Tk.pack ~side:`Left ~fill:`X [show ; hide] ; 
		let makeEntry container iv label = 
			(* cb is a function that should take one string argument *)
			let frm = Frame.create container in
			let msg = Message.create ~text:label  ~width:20 frm in
			let entry = Entry.create ~width:20 frm in
			Entry.insert ~index:(`Num 0) ~text:iv entry ; 
			Tk.pack ~side:`Left ~fill:`X [Tk.coe msg ; Tk.coe entry] ; 
			(* return the frame and a function that can get the 
			update value *)
			(frm, (fun() -> Entry.get entry) )
		in
		let textframe, txtcb = makeEntry fs m_text "value" in
		let xframe, xcb = makeEntry fs (sof (fois m_sx)) "x_size" in
		let yframe, ycb = makeEntry fs (sof (fois m_sy)) "y_size" in
		let wframe, wcb = makeEntry fs (sof (fois m_width)) "line_width" in
		
		(* apply button *)
		let applyframe = Frame.create fs in
		let cancel = Button.create applyframe ~text:"Cancel"
			~command:(fun () -> Tk.destroy fs ; ) in
		let apply = Button.create applyframe ~text:"Apply" 
			~command:(fun () -> 
				m_text <- txtcb () ; 
				m_sx <- iofs (fos (xcb ()) ); 
				m_sy <- iofs (fos (ycb ()) ); 
				m_width <- iofs (fos (wcb())) ; 
				self#update m_orot m_ox m_oy  ; 
				Tk.destroy fs ; 
			) in
		Tk.pack ~side:`Left ~fill:`X [cancel ; apply] ; 
		Tk.pack ~side:`Top ~fill:`Y 
			[showframe; textframe ; xframe ; yframe; wframe; applyframe] ; 
	)
end;;