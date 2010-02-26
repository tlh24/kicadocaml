open Printf
open Pcre
open Tk
open Comm
open Grfx
open Track 
open Mod
open Pad

let fudge = 1.06 (* yea.. a fudge factor. *)
class zone = 
object (self)
	val mutable m_net = 0
	val mutable m_netname = ""
	val mutable m_layer = 0
	val mutable m_clearance = 0.005 
	(* this is the minimum distance between the zone edges and a track / pad *)
	val mutable m_minthick = 0.005
	val mutable m_Z = 0.0
	val mutable m_hit = false
	val mutable m_moving = false
	val mutable m_hitN = 0
	val mutable m_hitEdge = -1
	(* minimum thickness of the zone.  kicad draws the edges of each polygon
	using a stroke of this width.  hence, when meshing our zones, we add this to the offset.
	therefore the rendering and meshing of zones here will have larger holes than in kicad or 
	in the gerbers that kicad produces. *)
	val mutable m_corners = [||] (* array of arrays of coordinates, float*float*int  *)
	val mutable m_poly = [] (* list of poly coordinates, float*float*int*int 
							-- this is read in from the board file *)
	val mutable m_tris = [] (* list of triangles, ((float*float)*(float*float)*(float*float)) 
							-- this is computed from the triangularization / filtering *)
	val mutable m_rawv = [] (* list of raw arrays for the polys *)
	val mutable m_rawv_tri = Raw.create_static `float 1 
	val mutable m_rawv_fill = Raw.create_static `float 1 
	val mutable m_g = new grfx (* used to store/compute color and Z-pos *)
	
	val mutable m_timestamp = ""
	val mutable m_aux = "" 
	val mutable m_options = ""
	
	method getLayer () = m_layer
	method getHit () = m_hit
	method setMoving m = m_moving <- m

	method set_corners pts = (
		m_corners <- Array.make 1 [||] ; (* array of arrays *)
		m_corners.(0) <- (Array.map (fun (x,y) -> (x,y,0)) 
			(Array.of_list pts)); 
	)
	method free () = ( (* required by ocaml 3.11 & lablgl 1.05, blah. *)
		List.iter(fun rw -> 
			Raw.free_static rw ; 
		) m_rawv ; 
		m_rawv <- [] ; (* empty list of Raw *)
		Raw.free_static m_rawv_tri ; 
		m_rawv_tri <- Raw.create_static `float 1 ; 
		Raw.free_static m_rawv_fill ; 
		m_rawv_fill <- Raw.create_static `float 1 ; 
	)
	method updateCorners () = (
		m_g#empty () ; 
		m_rawv <- [] ; 
		let i = ref 0 in
		(* printf "updating poly lines length %d\n%!" len ; *)
		let x0 = ref 0.0 in
		let y0 = ref 0.0 in
		Array.iter (fun corners -> 
			let len = Array.length corners in
			if len > 2 then (
				let (x2,y2,_) = corners.(0) in
				x0 := x2 ; y0 := y2 ;
				let len = (Array.length corners) in
				(* Printf.printf "zone update corners %d\n%!" len;*)
				let rawv = Raw.create_static `float (4 * len) in
				i := 0 ; 
				Array.iteri (fun j (x1,y1,_) -> 
					if j > 0 then (
						Raw.set_float rawv ~pos:(4 * !i + 0) !x0 ; 
						Raw.set_float rawv ~pos:(4 * !i + 1) !y0 ; 
						Raw.set_float rawv ~pos:(4 * !i + 2) x1 ; 
						Raw.set_float rawv ~pos:(4 * !i + 3) y1 ;
						incr i ; 
					); 
					x0 := x1 ; 
					y0 := y1 ; 
				) corners ; 
				(* close the outline *)
				Raw.set_float rawv ~pos:(4 * !i + 0) !x0 ; 
				Raw.set_float rawv ~pos:(4 * !i + 1) !y0 ; 
				Raw.set_float rawv ~pos:(4 * !i + 2) x2 ; 
				Raw.set_float rawv ~pos:(4 * !i + 3) y2 ;
				x0 := x2 ; 
				y0 := y2 ; 
				incr i; 
				m_rawv <- rawv :: m_rawv ; 
				(*  make little squares to let you drag around the corners *)
				let w = 0.01 /. 2.0 in
				Array.iter (fun  (x1,y1,_) -> 
					m_g#makeRectFloat ~accumulate:true x1 y1 w w ; 
				) corners ; 
			);
		) m_corners ; 
		(* now need to manage the color ; use grfx to compute this *)
		m_g#updateLayer m_layer; (* also calls update raw *)
		m_g#updateBBX () ; 
		m_g#setZ 0.0 ; 
		m_g#setAlpha 0.6 ; 
	)
	method update () = (
		self#free () ; 
		self#updateCorners ();
		self#updateLayers (); 
		let i = ref 0 in
		if List.length m_tris > 2 then (
			let len = (List.length m_tris * 3) in (* number of lines *)
			Raw.free_static m_rawv_tri ; 
			m_rawv_tri <- Raw.create_static `float (len*4) ; 
			Printf.printf "zone update tris %d\n%!" len; 
			i := 0 ; 
			List.iter ( fun (a,b,c) -> 
				let line d e = 
					Raw.set_float m_rawv_tri ~pos:(4 * !i + 0) (fst d) ; 
					Raw.set_float m_rawv_tri ~pos:(4 * !i + 1) (snd d) ; 
					Raw.set_float m_rawv_tri ~pos:(4 * !i + 2) (fst e) ; 
					Raw.set_float m_rawv_tri ~pos:(4 * !i + 3) (snd e) ; 
					incr i ; 
				in
				line a b ; 
				line b c ; 
				line c a ; 
			) m_tris ; 
			(* make the fill triangles *)
			let len = (List.length m_tris ) in (* number of triangles *)
			Raw.free_static m_rawv_fill ; 
			m_rawv_fill <- Raw.create_static `float (len * 6) ; 
			i := 0 ; 
			List.iter (fun (a,b,c) -> 
				let point d = 
					Raw.set_float m_rawv_fill ~pos:(2 * !i + 0) (fst d); 
					Raw.set_float m_rawv_fill ~pos:(2 * !i + 1) (snd d); 
					incr i ; 
				in
				point a ; point b; point c; 
			) m_tris ; 
			(* don't draw polylines if there are triangles *)
		) else if List.length m_poly > 2 then (
			(* draw polylines *)
			let (x2,y2,_,_) = (List.hd m_poly) in
			let x0 = ref 0.0 in
			let y0 = ref 0.0 in
			x0 := x2 ; y0 := y2 ;
			let len = (List.length m_poly) -1 in
			i := 0 ; 
			let rawv = Raw.create_static `float (4 * len) in
			List.iter (fun (x1,y1,_,_) -> 
				Raw.set_float rawv ~pos:(4 * !i + 0) !x0 ; 
				Raw.set_float rawv ~pos:(4 * !i + 1) !y0 ; 
				Raw.set_float rawv ~pos:(4 * !i + 2) x1 ; 
				Raw.set_float rawv ~pos:(4 * !i + 3) y1 ;
				x0 := x1 ; 
				y0 := y1 ; 
				incr i; 
			) (List.tl m_poly) ; 
			m_rawv <- rawv :: m_rawv ; 
		); 
	)
	method updateLayers () = (
		m_g#updateLayer m_layer;
		m_Z <- m_g#getZ(); 
	)
	method empty () = (
		m_tris <- [] ; 
		m_poly <- [] ; (* don't delete the corners *)
		self#update () ; 
	)
	method fill (tracks : Track.pcb_track list) (mods : Mod.pcb_module list) = (
		m_tris <- [] ; 
		m_poly <- [] ; 
		(* generate a bounding box *)
		let corners = (List.map (fun (a,b,_) -> (a,b)) (Array.to_list m_corners.(0))) in
		let (minx,miny) = 
			List.fold_left (fun (mx,my) (x,y)  -> (min mx x),(min my y))
			(List.hd corners) corners  in
		let (maxx,maxy) = 
			List.fold_left (fun (mx,my) (x,y)  -> (max mx x),(max my y))
			(List.hd corners) corners in
		let bbx = (minx,miny,maxx,maxy) in
		(* first filter by layer - remove all tracks & modules 
		that do not have our layer and are not on our net *)
		let tracks = List.filter (fun t -> 
			((t#getLayer ()) = m_layer || (t#getType() = Track_Via))
			&& bbxIntersect (t#getDrcBBX()) bbx
			&& (t#getNet()) != m_net
		) tracks in
		let mods = List.filter (fun m-> m#hasLayer m_layer) mods in
		(* keep the pads associated with their module, for position *)
		
		(* unfortunately grfx.ml works with quads, not polygons - 
		and we need polys, or a closed sequence of vertices, for meshing 
		hence routines need to be duplicated. *)
		let makeCircle (x,y) w = 
			let n = 16 in
			let v = ref [] in
			let t = ref 0.0 in
			let dt = 2.0 *. pi /. foi(n) in
			let cos_ t = cos(t) *. w *. 0.5 +. x in
			let sin_ t = sin(t) *. w *. 0.5 +. y in
			for i = 1 to n do (
				v := (cos_ !t , sin_ !t) :: !v ; 
				t := !t +. dt ;
			) done ; 
			!v (* this does not include the repeated last point *)
		in
		let makeTrack s e width = 
			let dx,dy = Pts2.sub e s in
			let len = sqrt(dx *. dx +. dy *. dy) /. (0.5 *. width) in
			let (nx, ny) = ( dx /. len, dy /. len) in (*line between them normalized to width.*)
			let (mx, my) = ((-1.0) *. ny , nx) in (*rotate pi/2 ccw *)
			let pnt t x y = ( x +. cos(t)*.nx +. sin(t)*.mx, y +. cos(t)*.ny +. sin(t)*.my ) in
			let t = ref (pi /. 2.0) in
			let dt = -2.0 *. pi /. 16.0 in
			let v = ref [] in
			let endcap (x,y) = 
				for i = 0 to 8 do (
					v := (pnt !t x y) :: !v ;
					t := !t +. dt ;
				) done ; 
			in
			endcap e ; 
			t := pi /. -2.0 ; 
			endcap s ; 
			(* List.iter (fun (x,y) -> printf "track pt (%f , %f)\n%!" x y ; ) !v ; *)
			!v (* again no repeats *)
		in
		let makeOval o w h = 
			(* not so tricky - just use maketrack *)
			if w > h then (
				let cw = 0.5 *. (w -. h) in
				makeTrack (Pts2.add o (-1.0 *. cw,0.0)) (Pts2.add o (cw,0.0)) h
			) else (
				let ch = 0.5 *. (h -. w) in
				makeTrack (Pts2.add o (0.0,-1.0 *. ch)) (Pts2.add o (0.0,ch)) w
			)
		in
		let makeRect (x,y) w h = 
			let f = 0.5 in
			 [ (x -. f *. w , y -. f *. h ) ; (x -. f *. w , y +. f *. h ) ; 
			(x +. f *. w , y +. f *. h ) ; (x +. f *. w , y -. f *. h ) ]
		in
		let segments s = 
			(* convert a series of vertices to segments, and close the loop. *)
			let q = List.rev s in
			let k = List.hd q in
			List.rev_map2 (fun a b -> (a,b)) s (k :: List.rev (List.tl q))
		in
		
		let segs = ref [] in
		let pts = ref [] in 
		(* make corners a loop so we don't miss any segments *)
		let corners_poly = (List.hd corners) :: (List.rev corners) in
		
		let addSegs v = 
			(* make sure at least one point is inside the list of corners *)
			if List.exists (fun p -> Poly.inside corners_poly p) v then (
				(* add the track points to the growing list, of course *)
				List.iter (fun e -> pts := e :: !pts) v ; 
				(* need to iterate over all segments, adding any intersections 
				to the list of points to add to the mesh *)
				let segv = segments v in
				List.iter (fun (a,b) -> 
					List.iter (fun (c,d) -> 
						let hit,e = Pts2.intersectBool a b c d in
						if hit then (
							pts := e :: !pts ; 
						)
					) !segs ; 
				) segv ;
				(* keep a running list of segments *)
				List.iter (fun e -> segs := e :: !segs) segv ; 
			) ; 
		in
		
		(* iterate over the tracks *)
		List.iter (fun t -> 
			let v = 
			match t#getType () with
				| Track_Track -> 
					makeTrack (t#getStart()) (t#getEnd()) 
						(fudge *. t#getWidth() +. 2.0 *. m_clearance +. m_minthick) ; 
				| Track_Via -> 
					makeCircle (t#getStart()) 
						(fudge *. t#getWidth() +. 2.0 *. m_clearance +. m_minthick) ; 
			in
			addSegs v ; 
		) tracks ; 
		
		(* iterate over the modules *)
		printf "Note: pads are not connected to zone; manual thermal releif required (for now)\n%!"; 
		List.iter (fun m -> 
			List.iter (fun p -> 
				if p#hasLayer m_layer (*&& p#getNet() != m_net*) then (
					let bbx = p#getBBX() in
					let pp = bbxCenter bbx in (* may have rotation applied *)
					let w,h = bbxWH bbx in
					let v = 
					match p#getShape() with 
						| Pad_Circle -> 
							makeCircle pp
								(fudge *. w +. 2.0 *. m_clearance +. m_minthick) ; 
						| Pad_Rect -> 
							makeRect pp
								(fudge *. w +. 2.0 *. m_clearance +. m_minthick)
								(fudge *. h +. 2.0 *. m_clearance +. m_minthick) ; 
						| Pad_Oval -> 
							makeOval pp 
								(fudge *. w +. 2.0 *. m_clearance +. m_minthick)
								(fudge *. h +. 2.0 *. m_clearance +. m_minthick) ;
						| _ -> printf "don't know pad shape!\n%!"; 
							[] ;
					in
					addSegs v ; 
				) ; 
			) (m#getPads()); 
		) mods ; 
		
		(* add the corners in last so they are added first *)
		pts := List.rev_append corners !pts ; 
		segs := List.rev_append (segments corners) !segs ;
		(* make the filter function *)
		let filter (a,b,c) =
			let d = Pts2.scl (Pts2.add a (Pts2.add b c)) 0.33333 in
			let good = ref (Poly.inside corners_poly d) in 
			List.iter (fun t -> 
				let e = 
				match t#getType() with
					| Track_Track ->
						( Pts2.closestpointonline (t#getStart()) (t#getEnd()) d true )
					| Track_Via -> 
						( (t#getStart()) )
				in
				if Pts2.distance d e < (fudge *. (t#getWidth()) *. 0.5 
					+. m_clearance +. 0.5 *. m_minthick) 
				then good := false
			) tracks ; 
			(* do the same for the modules *)
			List.iter (fun m -> 
				List.iter (fun p -> 
					if p#hasLayer m_layer (* && p#getNet() != m_net *) then (
						let bbx = p#getBBX() in
						let x,y = bbxCenter bbx in (* may have rotation applied *)
						let w,h = bbxWH bbx in
						let sx = (fudge *. w +. 2.0 *. m_clearance +. m_minthick) *. 0.5 in
						let sy = (fudge *. h +. 2.0 *. m_clearance +. m_minthick) *. 0.5 in
						let bad = 
						match p#getShape() with 
							| Pad_Circle -> 
								Pts2.distance (x,y) d < sx 
							| Pad_Rect -> 
								(fst d) > x -. sx && (fst d) < x +. sx &&
								(snd d) > y -. sy && (snd d) < y +. sy
							| Pad_Oval ->  
								let e,t = if sx > sy then (
									let cw = (sx -. sy) in
									Pts2.closestpointonline 
										(Pts2.add (x,y) (-1.0 *. cw, 0.0))
										(Pts2.add (x,y) (1.0 *. cw, 0.0))
										d true , sy
								) else (
									let ch = (sy -. sx) in
									Pts2.closestpointonline 
										(Pts2.add (x,y) (0.0,-1.0 *. ch))
										(Pts2.add (x,y) (0.0, 1.0 *. ch))
										d true , sx
								) in
								Pts2.distance d e < t
							| _ -> false;
						in
						(* if bad then ( --debug code.
							match p#getShape() with
								| Pad_Circle -> 
									printf "triangle %f,%f intersect circle @ %f,%f , %f \n%!"
									(fst d) (snd d) x y sx ; 
								| Pad_Rect -> 
									printf "triangle %f,%f intersect rect @ %f,%f , %f x %f\n%!"
									(fst d) (snd d) x y sx sy; 
								| Pad_Oval -> 
									printf "triangle %f,%f intersect oval @ %f,%f , %f x %f\n%!"
									(fst d) (snd d) x y sx sy; 
								| _ -> (); 
						) ;  *)
						good := !good && not bad ; 
					) ; 
				) (m#getPads()); 
			) mods ; 
			!good
		in
		(* attempt to limit really tiny triangles by spanning the space with extra points *)
		(* assume that we want 20 along the larger dim *)
		let dx,dy = maxx -. minx,maxy -. miny in
		let nx,ny = if dx > dy then (
			20, iof (floor 20.0 *. dy /. dx)
		)else(
			iof (floor 20.0 *. dx /. dy) , 20
		)in
		let sx,sy = dx /. (foi (nx-1)), dy /.(foi (ny-1)) in
		for kx = 0 to nx do (
			for ky = 0 to ny do (
				pts := ((minx -. (sx /. 2.0) +. sx *. (foi kx)),(miny  -. (sy /. 2.0) +. sy *. (foi ky))) :: !pts ; 
			) done ; 
		) done ; 
		m_tris <- Mesh.mesh !pts !segs filter ; 
		self#update() ; (* refill the Raw buffers *)
	)
	method hitclear () = m_hit <- false
	method hit (px,py) netnum hitsize hitz hitclear = (
		(* see if p is next to any of our corners *)
		if not m_moving then (
			m_hit <- false ; 
			if glayerEn.(m_layer) then (
				let found = ref false in
				let n = ref 0 in
				let r = 0.01 /. 2.0 in
				let len = Array.length m_corners.(0) in
				for i = 0 to len-1 do (
					let (cx,cy,_) = (m_corners.(0)).(i) in
					if fabs(px-.cx) < r && fabs(py-.cy) < r then (
						found := true; 
						n := i; 
					)
				) done ; 
				(* also look at the lines between corners *)
				let hitEdge k n s = 
					let j = (k+1) mod len in
					let ax,ay,_ = (m_corners.(0)).(k) in
					let bx,by,_ = (m_corners.(0)).(j) in
					let siz = (Pts2.distance (ax,ay) (bx,by)) *. r in
					if Pts2.tracktouch (ax,ay) (bx,by) (px,py) (r/. 2.0) then k,siz else n,s
				in
				if !found then (
					m_hitN <- !n ; 
					m_hitEdge <- -1 ; 
					let ms = 3.1415926 *. r *. r in
					if m_Z > hitz || (m_Z = hitz && ms < hitsize) then (
						m_hit <- true ; 
						List.iter (fun f -> f ()) hitclear; 
						(m_net,ms,(m_Z-. 0.01),[self#hitclear])
					) else (netnum,hitsize,hitz,hitclear)
				) else (
					(* see if we hit a segment *)
					let k = ref (-1) in
					let hitedge,edgesize = Array.fold_left (fun (n,s) _ -> 
						incr k; 
						hitEdge !k n s
					) ((-1),hitsize) (m_corners.(0)) in
(* 					if hitedge >= 0 then printf "hit an edge in zone!\n%!" ;  *)
					if hitedge >= 0 && (m_Z > hitz || (m_Z = hitz && edgesize < hitsize)) then (
						m_hit <- true; 
						m_hitEdge <- hitedge; 
						m_hitN <- -1 ; 
						List.iter (fun f -> f ()) hitclear; 
						(m_net,hitsize,m_Z,[self#hitclear])
					) else (netnum,hitsize,hitz,hitclear)
				)
			) else (netnum,hitsize,hitz,hitclear)
		) else (netnum,hitsize,hitz,hitclear)
	)
	method add (x,y) = (
		(* add a point to the given selected edge *)
		let corners = (m_corners.(0)) in
		let clen = Array.length corners in
		if m_hit && m_hitEdge >= 0 && m_hitEdge < clen then (
			let j = ref 0 in
			let c2 = Array.make (clen+1) (0.0,0.0,0) in
			for i = 0 to clen-1 do (
				c2.(!j) <- corners.(i); 
				incr j; 
				if i = m_hitEdge then (
					c2.(!j) <- x,y,0 ; 
					incr j;
				)
			) done ; 
			self#cleanCorners c2; 
			self#empty (); 
			self#update (); 
		)
	)
	method delete () = (
		(* remove a corner *)
		let corners = (m_corners.(0)) in
		let clen = Array.length corners in
		if m_hit && m_hitN >= 0 && m_hitN < clen && clen > 2 then (
			let c2 = Array.make (clen-1) (0.0,0.0,0) in
			let j = ref 0 in
			for i = 0 to clen-1 do (
				if i <> m_hitN then (
					c2.(!j) <- corners.(i); 
					incr j;
				)
			) done ;
			self#cleanCorners c2; 
			self#empty (); 
			self#update (); 
		)
	)
	method cleanCorners corners = (
		let clen = Array.length corners in
		let c3 = Array.map (fun (x,y,_) -> x,y,0) corners in
		let x,y,_ = c3.(clen-1) in
		c3.(clen-1) <- x,y,1 ; 
		m_corners.(0) <- c3; 
	)
	method move (x,y) = (
		let len = Array.length m_corners.(0) in
		if m_moving && m_hitN >= 0 && m_hitN < len then (
			let _,_,z = (m_corners.(0)).(m_hitN) in
			(m_corners.(0)).(m_hitN) <- (x,y,z); 
			self#updateCorners (); 
		) ; 
	)
	method polyToTris () = (
		(* try to convert the polygons in the input to triangles *)
		(* for now only works if kicadocaml saved the file previously *)
		m_tris <- [] ; 
		let c = ref (0.0, 0.0, 0, 0) in
		let b = ref (0.0, 0.0, 0, 0) in
		List.iter (fun a -> 
			let mark (_,_,d,_) = d = 1 in
			let loc (e,f,_,_) = (e,f) in
			if (mark a) && not (mark !b) && not (mark !c) then (
				(* this is a legit triangle, add it to the list *)
				m_tris <- (loc !c, loc !b, loc a) :: m_tris ; 
			); 
			c := !b ; 
			b := a ; 
		) m_poly; 
		m_tris <- List.rev m_tris ; 
		if List.length m_tris = (List.length m_poly) / 3 then (
			printf "converted %d poly segments to %d triangles\n%!" 
				(List.length m_poly) (List.length m_tris) ; 
			self#update(); 
		) else ( m_tris <- [] ; ); 
	) 
	method read_poly ic = (
		printf "reading in zone poly corners @ %d \n%!" !linenum; 
		let endpat = "\$endPOLYSCORNERS" in
		let line = ref (input_line2 ic) in
		let rex = Pcre.regexp ~flags:[`CASELESS] endpat in
		m_poly <- []; 
		while not (Pcre.pmatch ~rex !line) do (
			let sp = Pcre.extract ~pat:"([\d-]+) ([\d-]+) ([\d-]+) ([\d-]+)" !line in
			let conv n = fois (ios (sp.(n))) in
			m_poly <- ( (conv 1),(conv 2),(ios (sp.(3))),(ios (sp.(4))) ) :: m_poly ; 
			line := input_line2 ic ; 
		) done ; 
		m_poly <- List.rev m_poly ;
		self#polyToTris (); 
	)
	method read ic = (
		printf "reading in zone @ %d \n%!" !linenum ; 
		let endpat = "\$endCZONE_OUTLINE" in
		let line = ref (input_line2 ic) in
		let rex = Pcre.regexp ~flags:[`CASELESS] endpat in
		let corners = ref [] in (* temporary list; the other is a list of lists *)
		let conv n = fois (ios n) in
		while not (Pcre.pmatch ~rex !line) do (
			(* printf "%s\n%!" !line ;  *)
			let tp = try Some (Pcre.extract ~pat:"^([\w\$]+)" !line).(1) with _ -> None in
			(
			match tp with 
				| Some tpp -> (
					match tpp with 
					| "ZInfo" -> (
						let sp = Pcre.extract ~pat:"^\w+ (\w+) (\d+) \"([^\"]+)\"" !line in
						m_timestamp <- sp.(1) ; 
						m_net <- ios (sp.(2)) ; 
						m_netname <- sp.(3) ; 
						)
					| "ZLayer" -> (
						let sp = Pcre.extract ~pat:"^\w+ (\d+)" !line in
						m_layer <- ios (sp.(1)); 
						) 
					| "ZAux" -> ( m_aux <- !line ; )
					| "ZClearance" -> (
						let sp = Pcre.extract ~pat:"^\w+ (\d+)" !line in
						m_clearance <- (conv (sp.(1)))  ; 
						)
					| "ZMinThickness" -> (
						let sp = Pcre.extract ~pat:"^\w+ (\d+)" !line in
						m_minthick <- conv (sp.(1)) ; 
						)
					| "ZOptions" -> ( m_options <- !line ; )
					| "ZCorner" -> (
						let sp = Pcre.extract ~pat:"^\w+ ([\d-]+) ([\d-]+) (\d+)" !line in
						corners := ( (conv (sp.(1))),(conv (sp.(2))),(ios (sp.(3))) ) :: !corners ;
						)
					| "$POLYSCORNERS" -> (self#read_poly ic)
					| _ -> () 
				) 
				| None -> ()
			) ; 
			line := input_line2 ic ; 
		) done ;
		(* need to clean up the corners - break into a series of polygons; 
		the first is the primary, the rest are cutouts *)
		m_corners <- [||] ; 
		let acorners = ref [] in
		let dcorners = ref [] in 
		List.iter (fun (a,b,c) -> 
			dcorners := (a,b,c) :: !dcorners ; 
			if c > 0 then (
				acorners := (!dcorners) :: !acorners ; 
				dcorners := [] ; (* empty so can be filled again *)
			)
		) (List.rev !corners); 
		(* now convert this to an array of arrays *)
		m_corners <- Array.map (fun lst -> 
			Array.of_list (List.rev lst); 
		) (Array.of_list (List.rev !acorners)); 
	)
	method draw bbox = (
		let alpha = if !gcurnet = m_net then 0.78 else 0.5 in
		m_g#setAlpha alpha ; 
		if m_g#draw bbox ~hit:m_hit then ( (* there shouldn't be anything outside the corners, right? *)
			(* if we hit an edge, highlight that *)
			let clen = Array.length (m_corners.(0)) in
			if m_hit && m_hitEdge >= 0 && m_hitEdge < clen then (
				let (ax,ay,_) = (m_corners.(0)).(m_hitEdge) in
				let (bx,by,_) = (m_corners.(0)).((m_hitEdge+1) mod clen) in
				GlDraw.begins `lines ; 
				GlDraw.color ~alpha (1.,1.,1.) ; (* white *)
				GlDraw.vertex2 (ax,ay) ; 
				GlDraw.vertex2 (bx,by) ; 
				GlDraw.ends () ;
			) ; 
			let color = m_g#getColor() in
			GlDraw.color ~alpha color ;
			List.iter (fun rv -> 
				if (Raw.length rv) > 2 then (
					let len = (Raw.length rv)/2 in
					GlArray.vertex `two rv;
					GlArray.draw_arrays `lines 0 len ; 
				) ;
			) m_rawv ; 
			if (Raw.length m_rawv_tri) > 2 then (
				GlDraw.color ~alpha:(0.3+. alpha/. 2.5) color ;
				GlArray.vertex `two m_rawv_tri;
				GlArray.draw_arrays `lines 0 ((Raw.length m_rawv_tri)/2) ; 
			); 
			if (Raw.length m_rawv_fill) >= 6 then (
				GlDraw.color ~alpha:(0.2+. alpha/. 2.5) color ;
				GlArray.vertex `two m_rawv_fill;
				GlArray.draw_arrays `triangles 0 ((Raw.length m_rawv_fill)/2) ; 
			);
		); 
	)
	method save oc = (
		fprintf oc "$CZONE_OUTLINE\n" ; 
		fprintf oc "ZInfo %s %d \"%s\"\n"  m_timestamp m_net m_netname ; 
		fprintf oc "ZLayer %d\n" m_layer ; 
		fprintf oc "%s\n" m_aux ; 
		fprintf oc "ZClearance %d T\n"  (iofs m_clearance) ; 
		fprintf oc "ZMinThickness %d\n" (iofs m_minthick) ; 
		fprintf oc "%s\n%!" m_options ; 
		Array.iter (fun corn -> 
			Array.iter (fun (x,y,z) -> 
				fprintf oc "ZCorner %d %d %d\n" 
				(iofs x) (iofs y) z ; 
			) corn ; 
		) m_corners ;
		(* if there are triangle, save them - otherwise, try to save the poly *)
		if List.length m_tris > 0 then (
			fprintf oc "$POLYSCORNERS\n" ; 
			List.iter (fun (a,b,c) -> 
				fprintf oc "%d %d 0 0\n" 
					(iofs (fst a)) (iofs (snd a)) ; 
				fprintf oc "%d %d 0 0\n" 
					(iofs (fst b)) (iofs (snd b)) ; 
				fprintf oc "%d %d 1 0\n"  (* this marks the end of our (small) polygon? *)
					(iofs (fst c)) (iofs (snd c)) ; 
			) m_tris ; 
			fprintf oc "$endPOLYSCORNERS\n%!" ; 
		)else if (List.length m_poly) > 0 then (
			fprintf oc "$POLYSCORNERS\n" ; 
			List.iter (fun (x,y,z,w) -> 
				fprintf oc "%d %d %d %d\n"
					(iofs x) (iofs y) z w ; 
			) m_poly ; 
			fprintf oc "$endPOLYSCORNERS\n%!" ; 
		) ; 
		fprintf oc "$endCZONE_OUTLINE\n" ; 
	)
end
