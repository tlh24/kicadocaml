(* module for interacting with GlDraw.quads -- 
 circles, rings, tracks, squares, colors etc. *) 
open Comm (*common functions *)

class grfx = 
object (self)
	val mutable verts : (float * float) list = [] 
	val mutable rawv  =  Raw.create_static `float 1
	val mutable numverts = 0
	val mutable color : (float * float * float)  = ( 1. , 1. , 1. )
	val mutable alpha : float  = 0.5
	val mutable m_z : float  = 0.0 
	val mutable bbx : (float * float * float * float)  = ( 0. , 0. , 1. , 1. )

method empty () = verts <- []

method setAlpha a = alpha <- a 
method getColor() = color ;
method setColor c = color <- c 
method getZ () = m_z  
method setZ a = m_z <- a 

method updateRaw () = 
	(* update the raw list *)
	if(numverts > 3) then (
		if (Raw.length rawv) != (3* numverts) then (* then should reallocate *)
		Raw.free_static rawv ; 
		rawv <- Raw.create_static `float (3 * numverts) ; 
		let i = ref 0 in
		List.iter (fun (x,y) ->
			Raw.set_float rawv ~pos:(3* !i + 0) x ; 
			Raw.set_float rawv ~pos:(3* !i + 1) y ; 
			Raw.set_float rawv ~pos:(3* !i + 2) m_z ; 
			incr i ; 
		) verts ; 
	) ; 
	
method updateLayer  ?(z=0.001) overridez layer = 
	if glayerEn.(layer) then (
		color <- layer_to_color layer ; 
	) else (
		color <- (0. , 0., 0. ); 
	); 
	if overridez then  (
		m_z <- z ; 
	)else(
		m_z <- glayerZ.(layer);
	); 
	self#updateRaw (); 
	
method updateLayers layers = 
	let (rr,gg,bb,zz) = List.fold_left (fun (cr,cg,cb,z2) lay -> 
		if glayerEn.(lay) then (
			let (r,g,b) = layer_to_color lay in
			let z3 = glayerZ.(lay) in
			let z4 = if z3 > z2 then z3 else z2 in
			(cr +. r, cg +. g, cb +. b, z4)
		) else (cr,cg,cb,z2)
	) (0. , 0., 0., 0.) layers in
	let saturate (r,g,b) = 
		((clamp r 0. 1.),(clamp g 0. 1.),(clamp b 0. 1.))
	in
	color <- saturate(rr,gg,bb); 
	m_z <- zz; 
	self#updateRaw() ; 
	
method updateBBX () = 
	(* let's update the bounding box *)
	let (minx, miny) = try List.fold_left (
		fun (x,y) (mx,my) -> 
			let ix = if x < mx then x else mx in
			let iy = if y < my then y else my in
			(ix, iy) ) (List.hd verts) verts 
		with _ -> (0.0, 0.0) in
	let (maxx, maxy) = try List.fold_left (
		fun (x,y) (mx,my) -> 
			let ix = if x > mx then x else mx in
			let iy = if y > my then y else my in
			(ix, iy) ) (List.hd verts) verts 
		with _ -> (0.0, 0.0) in
	bbx <- (minx, miny, maxx, maxy) ; 
	numverts <- List.length verts ; 
	
method getBBX () = bbx

method getBBXSize () = 
	let (minx, miny, maxx, maxy) = bbx in
	(maxx -. minx) *. (maxy -. miny )
	
method makeRect x y w h = 
	let fx, fy, fw, fh = (convert_units x y w h) in
	self#makeRectFloat fx fy fw fh; 

method makeRectFloat fx fy fw fh = 
	verts <- [ (fx -. fw , fy -. fh ) ; (fx -. fw , fy +. fh ) ; (fx +. fw , fy +. fh ) ; (fx +. fw , fy -. fh ) ] ;
	self#updateBBX (); 
	self#updateRaw() ; 
	
method makeCircle x y w h = 
	(* for convienence, will make this drawable with quads not triangles. *)
	let (fx, fy, fw, fh) = convert_units x y w h in
	let n = 10 in
	let t = ref (pi /. foi((n+1) * 2)) in
	let dt = pi /. foi(n+1) in
	let cos_ t = cos(t) *. fw in
	let sin_ t = sin(t) *. fh in
	for i = 1 to n do (
		verts <- ( (fx -. cos_(!t), fy -. sin_(!t)) :: verts) ; 
		verts <- ( (fx -. cos_(!t+.dt), fy -. sin_(!t+.dt)) :: verts) ; 
		verts <- ( (fx -. cos_(!t+.dt), fy +. sin_(!t+.dt)) :: verts) ; 
		verts <- ( (fx -. cos_(!t), fy +. sin_(!t)) :: verts) ; 
		t := !t +. dt ; 
	) done; 
	self#updateBBX (); 
	self#updateRaw() ; 
	(* print_endline( "initializing verts: " ^ string_of_int(List.length b.verts)) *)

method makeRing (fx,fy) id od = 
	(* again, make drawable with quads. *)
	let (fid, fod) = (fois id)*. 0.5, (fois od)*. 0.5 in
	let n = 10 in
	let t = ref 0. in
	let dt = pi /. (foi n) in
	let pnt d a = 
		 ( (fx -. d *. cos(a)), (fy +. d *. sin(a) ) ) in
	for i = 1 to 2*n do (
		verts <- ( (pnt fid !t) :: verts ) ;  
		verts <- ( (pnt fid (!t +. dt)) :: verts ) ;  
		verts <- ( (pnt fod (!t +. dt)) :: verts ) ;  
		verts <- ( (pnt fod !t) :: verts ) ;
		t := !t +. dt ; 
	) done ;
	self#updateBBX  (); 
	self#updateRaw() ; 
	(* print_endline( "initializing verts: " ^ string_of_int(List.length b.verts)) *)

method makeTrackInt sx sy ex ey width = 
	self#makeTrack (Pts2.foist (sx,sy)) (Pts2.foist (ex,ey)) (fois width) ; 

method makeTrack (sx,sy) (ex,ey) width = 
	(*functionally the same as verts_circle_make, though the middle is stretched out. *)
	let dx = ex -. sx in
	let dy = ey -. sy in
	let len = sqrt(dx *. dx +. dy *. dy) /. (0.5 *. width) in
	let (nx, ny) = ( dx /. len, dy /. len) in (*line between them normalized to width.*)
	let (mx, my) = (-1. *. ny , nx) in (*rotate pi/2 ccw *)
	let n = 6 in
	let t = ref (pi /. foi((n+1) * 2)) in
	let dt = pi /. foi(n+1) in
	let pnt t x y = ( x -. cos(t)*.nx +. sin(t)*.mx, y -. cos(t)*.ny +. sin(t)*.my) in
	let endcap sta en x y = 
		for i = sta to en do (
			verts <- ( (pnt (-1. *. !t) x y ) :: verts ) ; 
			verts <- ( (pnt (-1. *. !t -. dt) x y ) :: verts ) ; 
			verts <- ( (pnt ( 1. *. !t +. dt) x y ) :: verts ) ; 
			verts <- ( (pnt ( 1. *. !t) x y ) :: verts ) ; 
			t := !t +. dt ; 
		)done; 
	in
	(* rounded end cap @ start *)
	endcap 1 (n/2) sx sy ; 
	(* center section *)
	verts <- ( (pnt (-1. *. !t) sx sy ) :: verts ) ; 
	verts <- ( (pnt (-1. *. !t) ex ey ) :: verts ) ; 
	verts <- ( (pnt ( 1. *. !t) ex ey ) :: verts ) ; 
	verts <- ( (pnt ( 1. *. !t) sx sy ) :: verts ) ; 
	(* rounded end cap @ end *)
	endcap (n/2) n ex ey ; 
	self#updateBBX (); 
	self#updateRaw() ; 
method makeOval o w h = 
	if w > h then (
		self#makeTrack 
			(Pts2.add o (w *. -0.5,0.0)) 
			(Pts2.add o (w *. 0.5,0.0)) h
	) else (
		self#makeTrack 
			(Pts2.add o (0.0,h *. -0.5)) 
			(Pts2.add o (0.0,h *. 0.5)) w
	)
method makeOvalRing n (ox,oy) w h id = 
	(* make this drawable with quads. *)
	(* if it were a real oval, this would not be so hard.. !*)
	let hemiring od theta (dx,dy) = 
		(* make a hemiring starting at theta *)
		let pnt d a = 
			( (ox +. 0.5 *. d *. cos(a)), (oy +. 0.5 *. d *. sin(a) ) ) in
		let displace a nn = 
			let (ddx,ddy) = if nn = 1 || nn = n+1 then
				(0.0, 0.0) else (dx,dy) in
			Pts2.add (pnt od a) (ddx, ddy)
		in
		let t = ref theta in
		let dt = pi /. (foi n) in
		for i = 1 to n do (
			verts <- (pnt id !t) :: verts ; 
			verts <- (pnt id (!t +. dt)) :: verts ; 
			verts <- (displace (!t +. dt) (i+1)) :: verts ; 
			verts <- (displace !t i) :: verts ; 
			t := !t +. dt ; 
		) done ; 
	in
	if w > h then ((* horizontal oval *)
		let cw = 0.5 *. (w -. h) in
		hemiring h (0.5 *. pi) (-1.0 *. cw, 0.0) ; 
		hemiring h (-0.5 *. pi) (1.0 *. cw, 0.0) ; 
	) else ( (* vertical oval *)
		let ch = 0.5 *. (h -. w) in
		hemiring w pi (0.0, -1.0 *. ch) ; 
		hemiring w 0.0 (0.0, 1.0 *. ch) ; 
	) ;
	self#updateBBX (); 
	self#updateRaw () ; 
	
method makeChar ch sy sx width xoff = (
	(* note the x & y sizes are reversed, because coordinates are stored in 
	(y, x) pairs in grfonte.ml *)
	let lst = Grfonte.shapes.(ch) in
	let oldx = ref 42 in
	let oldy = ref 42 in
	let trans (dx, dy) = 
		let fx = ((foi dx) +. xoff ) *. (sx /. 10.) in
		let fy = ((foi dy) -. 4.5)*. (sy /. -9.) in
		(fx, fy)
	in
	let rect a b width = 
		let (ax, ay) = a in
		let (bx, by) = b in
		let n = Pts2.scl( Pts2.norm( Pts2.sub b a ) ) width in
		(*need to rotate this *)
		let nx = -1. *. (snd n) in
		let ny = (fst n) in
		let (ox, oy) = Pts2.scl (nx, ny) 0.5 in
		let lx = 0.866 *. (fst n) in
		let ly = 0.866 *. (snd n) in
		verts <- ( (ax -. lx +. ox , ay -. ly +. oy) :: verts ) ; 
		verts <- ( (ax -. lx -. ox , ay -. ly -. oy) :: verts ) ; 
		verts <- ( (ax -. nx , ay -. ny) :: verts ) ; 
		verts <- ( (ax +. nx , ay +. ny) :: verts ) ; 
		
		verts <- ( (ax +. nx , ay +. ny) :: verts ) ; 
		verts <- ( (ax -. nx , ay -. ny) :: verts ) ; 
		verts <- ( (bx -. nx , by -. ny) :: verts ) ; 
		verts <- ( (bx +. nx , by +. ny) :: verts ) ; 
		
		verts <- ( (bx +. lx +. ox , by +. ly +. oy) :: verts ) ; 
		verts <- ( (bx +. lx -. ox , by +. ly -. oy) :: verts ) ; 
		verts <- ( (bx -. nx , by -. ny) :: verts ) ; 
		verts <- ( (bx +. nx , by +. ny) :: verts ) ; 
	in
	Array.iter (fun (py, px) -> 
		if (( !oldx != 42 ) && ( px != 42 )) then (
			rect (trans (!oldx, !oldy)) (trans (px, py)) width ; 
		) ; 
		oldx := px; 
		oldy := py; 
	) lst ; 
)

method makeText x y rot ox oy orot sx sy width text = (
	let a = Array.init (String.length text) (fun i -> int_of_char(String.get text i)) in
	let off = (foi (String.length text)) *. -5. in
	Array.iteri (fun i ch ->
		let xoff = (foi i) *. 10. +. off in
		self#makeChar ch sx sy width xoff
	) a ; 
	self#rotateTranslate rot x y ; 
	self#rotateTranslate orot ox oy ;
)

method rotateTranslate orient x y = 
	let a = (foi orient) /. 572.9577951 in
	let translate (xa, ya) = (xa +. (fois x), ya +. (fois y))  in
	(* translate after rotating the pad verticies *)
	verts <- List.rev_map (rotate2 ~angle:a) verts ;
	verts <- List.rev_map translate verts ; 
	self#updateBBX (); 
	self#updateRaw() ; 

method hit (x,y) = 
	let mx, my, xx, xy = bbx in
	(x > mx && x < xx && y > my && y < xy ) 
	
method draw ?(hit=false) ?(hcolor=(1.,1.,1.)) bbox = 
	(* do some visibility checking  .. *)
	if  bbxIntersect bbox bbx  && color != (0. , 0. , 0.) then (
		let col = 
			if hit then 
				hcolor
			else
				color
			in
		GlDraw.color ~alpha col ; 
		GlArray.vertex `three rawv; 
		GlArray.draw_arrays `quads 0 numverts ; 
	); 
end
	