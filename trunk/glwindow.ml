open Tk
open Printf
open Comm

let foi = float_of_int
let iof = int_of_float
let first (a,_,_) = a;;
let scnd (_,a,_) = a;;
let thrd (_,_,a) = a ;;

(* moveActionStack is a list of callback functions for listening to mouse move. *)
(* when you start listening, you push a function on the list ; *)
(* when you stop, you pop it off & set to the next lowest (if it exists) *)
let moveActionStack = ref [ (fun (_:eventInfo) -> ()) ] (* list of (fun (ev:eventInfo) -> () ) *)

let popActionStack () = 
	let nul = (fun _ -> ()) in
	if List.length !moveActionStack > 0 then (
		moveActionStack := List.tl !moveActionStack ; 
		if List.length !moveActionStack > 0 then (
			List.hd !moveActionStack 
		) else (
			nul
		)
	) else nul
	;;
	
let pushActionStack action = 
	moveActionStack := (action :: !moveActionStack) ; 
	() 
	;;

class glwindow = 
object (self)
	(* use currying to provide the runtime-created arguments *)
	val mutable m_fixedOrtho = true ; (*fixed aspect ratio *)
	val mutable m_fixedY = false ; 
	val mutable m_windowsize = 10, 10
	val mutable m_pan = 0.0, 0.0
	val mutable m_oldpan = 0.0, 0.0
	val mutable m_zoom = 1.0 , 1.0, 1.0
	val mutable m_curs = 0.0 , 0.0
	val mutable m_oldcurs = 0.0 , 0.0
	val mutable m_renderCB = []
	val mutable m_button3pressed = false
	val mutable m_xrot = 0.0
	val mutable m_yrot = 0.0
	
	method getPan () = m_pan
	method setPan p = m_pan <- p
	method getZoom () = m_zoom 
	method setZoom z = m_zoom <- z
	method setFixedOrtho b = m_fixedOrtho <- b 
	method setFixedY b = m_fixedY <- b
	method setRenderCB cb = 
		m_renderCB <- cb :: m_renderCB ; 
	
	method calcCursorPos ev pan = 
		let w, h = m_windowsize in
		let ar = if m_fixedOrtho then 1.0 else (foi w)/.(foi h) in 
		let w2, h2 = (foi w) /. 2. , (foi h) /. 2. in
		let x = ((foi ev.ev_MouseX) -. w2) /. w2 *. ar in
		let y = ((foi ev.ev_MouseY) -. h2) /. h2 *. -1. in
		((x /. (first m_zoom)) -. (fst pan)) , ((y /. (scnd m_zoom)) -. (snd pan))
		
	method updateCursorPos ev = 
		m_curs <- self#calcCursorPos ev m_pan
	method cursorPos ev =  (* this one returns the cursor position & updates internal store*)
		m_curs <- self#calcCursorPos ev m_pan ; 
		m_curs
		
	method getBBX () = 
		let center = Pts2.scl m_pan (-1.0) in
		let (ax, ay) = Pts2.add center (-1.0/.(first m_zoom), -1.0/.(scnd m_zoom)) in
		let (bx, by) = Pts2.add center (1.0/.(first m_zoom), 1.0/.(scnd m_zoom)) in
		(ax, ay, bx, by)
		
	method render togl =
		Togl.make_current togl ; 
(* 		GlClear.accum ~alpha:0.0 (0.0, 0.0, 0.0) ;  *)
(* 		GlFunc.accum `load 0.0 ;  *)
		GlClear.color (0.0, 0.0, 0.0) ~alpha:0.0;
		GlClear.clear [`color; `depth];
		Gl.disable `depth_test ;
		Gl.enable `blend ; 
		GlMat.mode `modelview ; 
		GlMat.load_identity () ; 
		GlMat.rotate ~angle:m_yrot ~x:0.0 ~y:1.0 ~z:0.0 () ; 
		GlMat.rotate ~angle:m_xrot ~x:1.0 ~y:0.0 ~z:0.0 () ; 
		GlMat.scale ~x:(first m_zoom) ~y:(scnd m_zoom) ~z:(1.0) () ; 
		GlMat.translate ~x:(fst m_pan) ~y:(snd m_pan) ~z:(0.0) (); 
		(* callbacks *)
		List.iter (fun f -> f() ) m_renderCB ; 
		Gl.flush ();
(* 		GlFunc.accum `accum 0.5 ;  *)
(* 		GlFunc.accum `return 1.0 ;  *)
		Togl.swap_buffers togl ; 
		
	method frustrum fovy aspect zmin zmax = 
		let ymax = zmin *. tan(fovy *. 3.14159 /. 360.0 ) in
		let ymin = -1.0 *. ymax in
		let xmin = ymin *. aspect in
		let xmax = ymax *. aspect in
		GlMat.frustum ~x:(xmin,xmax) ~y:(ymin, ymax) ~z:(zmin, zmax) ; 

	method setZClip togl zmin zmax = 
		Togl.make_current togl ; 
		GlMat.mode `projection ; 
		GlMat.load_identity () ; 
		let ar = (foi (fst m_windowsize)) /. (foi (snd m_windowsize)) in
		if m_fixedOrtho then ((* slow view is strictly 2x2 *)
			GlMat.ortho ~x:(-1. , 1.) ~y:(-1. ,1. ) ~z:(zmin, zmax);
		) else (
			self#frustrum 45.0 ar (-1.0) 1.0 ;
			(* GlMat.ortho ~x:(-1. *. ar, 1. *. ar) ~y:(-1. ,1. ) ~z:(zmin, zmax)  ; *)
		);
	
	method reshape togl = 
		Togl.make_current togl ; 
		m_windowsize <- (Togl.width togl) , (Togl.height togl);
		GlDraw.viewport ~x:0 ~y:0 ~w:(Togl.width togl) ~h:(Togl.height togl);
		self#setZClip togl (-10.0) 10.0 ; 
		GlMat.mode `modelview ; 
		GlMat.load_identity () ; 
		GlFunc.blend_func ~src:`src_alpha ~dst:`one;
		(* http://pyopengl.sourceforge.net/documentation/manual/glBlendFunc.3G.html *)
		GlArray.enable `vertex;
		self#render togl ; 
		
	method bind (top : Widget.toplevel Widget.widget) togl = 
		(* bind mouse events *)
		bind ~events:[`ButtonPressDetail(3)] ~fields:[`MouseX; `MouseY] ~action:
			(fun ev -> 
				m_oldcurs <-  self#calcCursorPos ev m_pan; 
				m_oldpan <- m_pan ;
				m_button3pressed <- true ;
				let action evinf = 
					let prescurs = self#calcCursorPos evinf m_oldpan in
					let newpan = Pts2.add (Pts2.sub prescurs m_oldcurs) m_oldpan in
					m_pan <-  (fst newpan), (if m_fixedY then (snd m_pan) else (snd newpan)); 
					self#render togl
				in
				bind ~events:[`Motion] ~fields:[`MouseX; `MouseY] ~action:action top ; 
				pushActionStack action ; 
			) top ;
		bind ~events:[`ButtonReleaseDetail(3)]  ~fields:[`MouseX; `MouseY] ~action:
			(fun ev -> 
				self#updateCursorPos ev ; 
				m_button3pressed <- false ;
				bind ~events:[`Motion] ~fields:[`MouseX; `MouseY] 
					~action:(popActionStack ()) top;
				self#render togl ;
			) top ; 
		bind ~events:[`KeyPressDetail("Up")] ~action:
			(fun _ -> m_xrot <- m_xrot +. 2.0; self#render togl ;) top ; 
		bind ~events:[`KeyPressDetail("Down")] ~action:
			(fun _ -> m_xrot <- m_xrot -. 2.0; self#render togl ;) top ; 
		bind ~events:[`KeyPressDetail("Left")] ~action:
			(fun _ -> m_yrot <- m_yrot +. 2.0; self#render togl ;) top ; 
		bind ~events:[`KeyPressDetail("Right")] ~action:
			(fun _ -> m_yrot <- m_yrot -. 2.0; self#render togl ;) top ; 
			
		let doZoom ev zm = 
			self#updateCursorPos ev ; 
			let steps = 1.0 in
			let z = exp((log zm) /. steps) in
			for i = 1 to (iof steps) do (
				let l = Pts2.add m_pan m_curs in
				let l2 = (fst l) , (if m_fixedY then 0.0 else (snd l)) in
				let s = 1. /. z -. 1. in
				m_pan <- Pts2.add (Pts2.scl l2 s) m_pan; 
				m_zoom <- (first m_zoom) *. z , 
					(if m_fixedY then (scnd m_zoom) else (scnd m_zoom) *. z) , 
					(thrd m_zoom) *. z; 
				self#render togl ; 
			) done ; 
		in
		bind ~events:[`ButtonPressDetail(5)] 
			~fields:[`MouseX; `MouseY] ~action:(fun ev -> doZoom ev (1. /. 1.2) ) top; 
		bind ~events:[`ButtonPressDetail(4)] 
			~fields:[`MouseX; `MouseY] ~action:(fun ev -> doZoom ev 1.2) top;
		(* bind the render / reshape functions *)
		Togl.display_func togl ~cb:(fun () -> self#render togl );
		Togl.reshape_func togl ~cb:(fun () -> self#reshape togl );
		(* fit it to the window *)
		pack ~fill:`Both ~expand:true [togl];
end
