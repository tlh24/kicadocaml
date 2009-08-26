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
	(* minimum thickness of the zone.  kicad draws the edges of each polygon
	using a stroke of this width.  hence, when meshing our zones, we add this to the offset.
	therefore the rendering and meshing of zones here will have larger holes than in kicad or 
	in the gerbers that kicad produces. *)
	val mutable m_corners = [] (* list of lists of coordinates, float*float*int  *)
	val mutable m_poly = [] (* list of poly coordinates, float*float*int*int *)
	val mutable m_tris = [] (* list of triangles, ((float*float)*(float*float)*(float*float)) *)
	val mutable m_rawv = [] (* list of raw arrays for the polys *)
	val mutable m_rawv_tri = Raw.create_static `float 1 
	val mutable m_rawv_fill = Raw.create_static `float 1 
	val mutable m_g = new grfx (* used to store/compute color and Z-pos *)
	
	val mutable m_timestamp = ""
	val mutable m_aux = "" 
	val mutable m_options = ""
	
	method getLayer () = m_layer

	method set_corners pts = (
		m_corners <- [] ; (* list of lists, remember! *)
		m_corners <- (List.map (fun (x,y) -> (x,y,0)) pts) :: m_corners ; 
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
	method update () = (
		self#free () ; 
		let i = ref 0 in
		(* printf "updating poly lines length %d\n%!" len ; *)
		let x0 = ref 0.0 in
		let y0 = ref 0.0 in
		List.iter (fun corners -> 
			if List.length corners > 2 then (
				let (x2,y2,_) = (List.hd corners) in
				x0 := x2 ; y0 := y2 ;
				let len = (List.length corners) -1 in
				let rawv = Raw.create_static `float (4 * len) in
				i := 0 ; 
				List.iter (fun (x1,y1,_) -> 
					Raw.set_float rawv ~pos:(4 * !i + 0) !x0 ; 
					Raw.set_float rawv ~pos:(4 * !i + 1) !y0 ; 
					Raw.set_float rawv ~pos:(4 * !i + 2) x1 ; 
					Raw.set_float rawv ~pos:(4 * !i + 3) y1 ;
					x0 := x1 ; 
					y0 := y1 ; 
					incr i; 
				) (List.tl corners) ; 
				m_rawv <- rawv :: m_rawv ; 
			);
		) m_corners ; 
		if List.length m_tris > 2 then (
			let len = (List.length m_tris * 3) in (* number of lines *)
			Raw.free_static m_rawv_tri ; 
			m_rawv_tri <- Raw.create_static `float (len*4) ; 
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
			x0 := x2 ; y0 := y2 ;
			let len = (List.length m_poly) -1 in
			i := 0 ; 
			let rawv = Raw.create `float (4 * len) in
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
		(* now need to manage the color ; use grfx to compute this *)
		m_g#updateLayer false m_layer; 
	)
	method fill (tracks : Track.pcb_track list) (mods : Mod.pcb_module list) = (
		m_tris <- [] ; 
		m_poly <- [] ; 
		(* generate a bounding box *)
		let corners = (List.map (fun (a,b,_) -> (a,b)) (List.hd m_corners)) in
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
		and we need polys, or a closed sequence of vertices, for meshing *)
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
		List.iter (fun m -> 
			List.iter (fun p -> 
				if p#hasLayer m_layer && p#getNet() != m_net then (
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
					if p#hasLayer m_layer && p#getNet() != m_net then (
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
						if bad then (
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
						) ;  
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
		let sx,sy = dx /. (foi (nx+1)), dy /.(foi (ny+1)) in
		for kx = 1 to nx do (
			for ky = 1 to ny do (
				pts := ((minx +. sx *. (foi kx)),(miny +. sy *. (foi ky))) :: !pts ; 
			) done ; 
		) done ; 
		m_tris <- Mesh.mesh !pts !segs filter ; 
		self#update() ; (* refill the Raw buffers *)
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
				m_tris <- (loc a, loc !b, loc !c) :: m_tris ; 
			); 
			c := !b ; 
			b := a ; 
		) m_poly; 
		
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
		m_corners <- [] ; 
		let dcorners = ref [] in 
		List.iter (fun (a,b,c) -> 
			dcorners := (a,b,c) :: !dcorners ; 
			if c > 0 then (
				m_corners <- (List.rev !dcorners) :: m_corners ; 
				dcorners := [] ; (* empty so can be filled again *)
			)
		) (List.rev !corners); 
		m_corners <- List.rev m_corners ; 
	)
	
	method draw () =  (* really should accept a screenbox here for culling *)
		GlDraw.color ~alpha:0.5 (m_g#getColor()) ;
		List.iter (fun rawv -> 
			if (Raw.length rawv) > 2 then (
				let len = (Raw.length rawv)/2 in
				GlArray.vertex `two rawv;
				GlArray.draw_arrays `lines 0 len ; 
			) ;
		) m_rawv ; 
		if (Raw.length m_rawv_tri) > 2 then (
			GlDraw.color ~alpha:0.4 (m_g#getColor()) ;
			GlArray.vertex `two m_rawv_tri;
			GlArray.draw_arrays `lines 0 ((Raw.length m_rawv_tri)/2) ; 
		); 
		if (Raw.length m_rawv_fill) >= 6 then (
			GlDraw.color ~alpha:0.3 (m_g#getColor()) ;
			GlArray.vertex `two m_rawv_fill;
			GlArray.draw_arrays `triangles 0 ((Raw.length m_rawv_fill)/2) ; 
		); 

	method save oc = (
		fprintf oc "$CZONE_OUTLINE\n" ; 
		fprintf oc "ZInfo %s %d \"%s\"\n"  m_timestamp m_net m_netname ; 
		fprintf oc "ZLayer %d\n" m_layer ; 
		fprintf oc "%s\n" m_aux ; 
		fprintf oc "ZClearance %d T\n"  (iofs m_clearance) ; 
		fprintf oc "ZMinThickness %d\n" (iofs m_minthick) ; 
		fprintf oc "%s\n%!" m_options ; 
		List.iter (fun corn -> 
			List.iter (fun (x,y,z) -> 
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
