open Printf
open Comm
open Tk
open Glwindow
(* module for computing a triangularization of a space given a set of 
	points in a plane *)

(* rather than dealing with pointers, we store everything in an array, 
	then index this array with numbers internal to the triangle structure *)
type t_triangle = {
	mutable a: (float*float) ; 
	mutable b: (float*float) ; 
	mutable c: (float*float) ; 
	mutable abn: int ; (* triangle off edge ab *)
	mutable bcn: int ; (* triangle off edge bc *)
	mutable can: int ; (* triangle off edge ca *)
} ;;

let triangleCenter a b c = 
	(* finds the center (centroid, really) of a triangle *)
	let ac2 = Pts2.scl (Pts2.add a c) 0.5 in
	let bc2 = Pts2.scl (Pts2.add b c) 0.5 in
	Pts2.intersectPt a bc2 b ac2
	;;
let triCenter t = triangleCenter t.a t.b t.c 
let triMean t = Pts2.scl (Pts2.add t.a (Pts2.add t.b t.c)) 0.33333 
;;

let insidetri2 (x1,y1) (x2,y2) (x3,y3) (x,y) = 
	(* use barycentric coordinates *)
	let a = x1 -. x3 in
	let b = x2 -. x3 in
	let c = y1 -. y3 in
	let d = y2 -. y3 in
	let det = (a*.d -. b*.c) in
	if abs_float det > 0.0001 then (
		let e = x -. x3 in
		let f = y -. y3 in
		let b1,b2 = (d*.e -. b*.f) /. det , (a*.f -. c*.e) /. det in
		b1 > 0.0 && b1 < 1.0 && b2 > 0.0 && b2 < 1.0
		(* the equality is for when the vertex is on an edge of the triangle. *)
	) else false
;;
let insidetri3 a b c (x,y) = 
	(* determine if the point (x,y) is within the triangle *)
	(* basically, if it is left of only one of the segments, then it is *)
	let inside (x0,y0) (x1,y1) = 
		if abs_float (y1 -. y0) > 0.00001 then (
			let u = (y -. y0) /. (y1 -. y0) in
			if u >= 0.0 && u < 1.0 then ( 
				let xx = x0 +. (x1 -. x0) *. u in
				if xx >= x then 1 else 0
			) else 0
		) else 0
	in
	if (inside a b) + (inside b c) + (inside c a) = 1 then true else false
	;;
let add (ax,ay) (bx,by) = 
	ax +. bx, ay +. by
	;; 
let sub (ax,ay) (bx,by) = 
	ax -. bx, ay -. by
	;; 
let scl a b = 
	(fst a) *. b ,  (snd a) *. b
	;;
let cross (ax,ay) (bx,by) = 
	(ax *. by) -. (ay *. bx)
	;;
let insidetri a b c d = 
	cross (sub b a) (sub d a) > 0.0 &&
	cross (sub c b) (sub d b) > 0.0 &&
	cross (sub a c) (sub d c) > 0.0 
	;;
let ptonline a b c = 
	let d = (Pts2.sub b a) in
	let e = (Pts2.sub c a) in
	let f = abs_float (Pts2.crossnorm d e )in
	if f < 0.00001 then (
		let g = add (scl d ((Pts2.length e)/.(Pts2.length d))) a in
		if Pts2.length d >= Pts2.length e then true,g else false,g
	) else false, (0.0,0.0)
	;;
let ccw r = 
		List.fold_left (fun ss (a,b,c) -> 
			min ss (Pts2.cross (Pts2.sub a b) (Pts2.sub b c))
		) 2.0 [(r.a,r.b,r.c);(r.b,r.c,r.a);(r.c,r.a,r.b)] 
	;;
let printtri t = 
	let w = ccw t in
	let s = if w < 0.0 then "CW" else "CCW" in
	printf " a=(%f,%f) b=(%f,%f) c=(%f,%f) abn=%d bcn=%d can=%d %s\n%!"
		(fst (t.a)) (snd (t.a)) (fst (t.b)) (snd (t.b)) (fst (t.c)) (snd (t.c))
		t.abn t.bcn t.can s; 
	;;
	
let debugTris t d e = 
	(* format for use with the matlab helper function, for display and examination *)
	printf "debugTris(["; 
	let printpoint p = printf "%f,%f;" (fst p) (snd p) in
	List.iter printpoint [t.a;t.b;t.c;d;e]; 
	printf "]);\n%!"
	;;
	
let gtris = ref [| |] 
let gannopts = ref [] 
let gannosegs = ref [] 
let ghotseg = ref ((0.0, 0.0),(1.0, 1.0))
let gcenter = ref (0.0, 0.0)
let gzoom = ref 1.0 
let gselected = ref 0
let gselecthot = ref false
let renderfunc = ref (fun () -> ())
let delayfunc = ref (fun () -> ())
let glwind = new glwindow
let tcnt = ref 0 (* number of triangles *)
let grun = ref 0
	
let render togl = 
	Togl.make_current togl ; 
	GlMat.push (); 
	GlMat.scale ~x:1.0 ~y:(-1.0 ) ~z:1.0 () ; 
	(* GlMat.translate ~x:(-1.0 *. (fst !gcenter)) ~y:(-1.0 *. (snd !gcenter)) ~z:0.0 (); *)
	GlDraw.begins `triangles ; 
	GlDraw.color ~alpha:0.5 (0.1,0.2, 0.6); 
	let renderTriangle t = 
		let a = t.a in
		let b = t.b in
		let c = t.c in
		GlDraw.vertex3 (fst a, snd a, 0.5); 
		GlDraw.vertex3 (fst b, snd b, 0.5); 
		GlDraw.vertex3 (fst c, snd c, 0.5); 
	in
	for k = 0 to !tcnt - 1 do (
		renderTriangle (!gtris.(k))
	)  done ; 
	if !gselecthot then (
		GlDraw.color ~alpha:0.65 (1.0,0.45, 1.0); 
	) else (
		GlDraw.color ~alpha:0.65 (0.35,0.65, 1.0); 
	) ; 
	if !gselected >= 0 && !gselected < Array.length !gtris && false then (
		renderTriangle (!gtris).(!gselected); 
		(* zoom in on this triangle *)
		let t = (!gtris).(!gselected) in
		let minx = min (min (fst t.a) (fst t.b)) (fst t.c) in
		let miny = min (min (snd t.a) (snd t.b)) (snd t.c) in
		let maxx = max (max (fst t.a) (fst t.b)) (fst t.c) in
		let maxy = max (max (snd t.a) (snd t.b)) (snd t.c) in
		let tpan = ((minx +. maxx) *. -0.5), ((miny +. maxy) *. 0.5) in
		let tz = 1.5 /. (max (maxx -. minx) (maxy -. miny)) in
		let pan = glwind#getPan() in
		let z,_,_ = glwind#getZoom() in
		glwind#setPan (add (scl pan 0.999) (scl tpan 0.001)) ; 
		let nz = (0.999 *. z +. 0.001 *. tz) in
		glwind#setZoom (nz,nz,1.0); 
	) ; 
	GlDraw.ends () ; 
	
	GlDraw.begins `lines ; 
	GlDraw.color ~alpha:0.5 (1.0,1.0, 1.0); 
	for k = 0 to !tcnt - 1 do (
		let a = (!gtris.(k)).a in
		let b = (!gtris.(k)).b in
		let c = (!gtris.(k)).c in
		GlDraw.vertex3 (fst a, snd a, 0.6); 
		GlDraw.vertex3 (fst b, snd b, 0.6); 
		GlDraw.vertex3 (fst a, snd a, 0.6); 
		GlDraw.vertex3 (fst c, snd c, 0.6); 
		GlDraw.vertex3 (fst c, snd c, 0.6); 
		GlDraw.vertex3 (fst b, snd b, 0.6); 
	)  done ; 
	(* render little crosses for the points *)
	GlDraw.color ~alpha:0.5 (1.0,0.5, 0.0); 
	let h = 0.01 /. !gzoom in
	List.iter (fun (x,y) -> 
		GlDraw.vertex3 (x -. h, y, 0.4) ; 
		GlDraw.vertex3 (x +. h, y, 0.4) ; 
		GlDraw.vertex3 (x, y -. h, 0.4) ; 
		GlDraw.vertex3 (x, y +. h, 0.4) ; 
	) !gannopts ; 
	(* reder segments in red *)
	GlDraw.color ~alpha:0.5 (1.0,0.0, 0.0); 
	List.iter (fun (a,b) -> 
		GlDraw.vertex3((fst a),(snd a),0.5); 
		GlDraw.vertex3((fst b),(snd b),0.5); 
	) !gannosegs ;
	GlDraw.color ~alpha:0.8 (0.0,1.0, 1.0); 
	let (a,b) = !ghotseg in
	GlDraw.vertex3((fst a),(snd a),0.8); 
	GlDraw.vertex3((fst b),(snd b),0.8); 
	GlDraw.ends () ;
	GlMat.pop (); 
	Gl.flush ();
	Togl.swap_buffers togl ; 
	!delayfunc (); 
	gselected := -1 ; 
	;;
		
let makewindow top = 
	let wind = Toplevel.create top  in
	Wm.title_set wind "mesh view";
	let togl = Togl.create ~width:1000 ~height:1000 
		~rgba:true ~double:true ~depth:true wind in
	glwind#bind wind togl ; 
	glwind#reshape togl ; 
	glwind#setRenderCB (fun () -> render togl );
	renderfunc := (fun () -> glwind#render togl) ; 
	;;
	
let mesh pts segs filter = 
	(* if we have n points, then we need at most 2n (or so) triangles - 
	the actual formula is 2n - 2 - k, where k is the number of points on the 
	convex hull *)
	(* do not worry about duplicates until we insert the points *)
	let npts = List.length pts in
	let ntri = npts * 4 in
	let origin = (0.0 , 0.0) in
	let tt = Array.make ntri 
		{a=origin; b=origin; c=origin; abn=(-1); bcn=(-1); can=(-1)} in
	gtris := tt ; 
	let tlast = ref 0 in (* the last insertion point *)
	
	let rotate tn d = 
		(* given triangle index tn, rotate by d sides, this used for
		aligning triangles for the flip-test operation mantain CCW orientation *)
		let t = tt.(tn) in (* make a copy *)
		match d with 
			| 1 -> ( (* bc -> ab etc *)
				{a=t.b; b=t.c; c=t.a; abn=t.bcn; bcn=t.can; can=t.abn})
			| 2 -> ( (* ca -> ab etc *)
				{a=t.c; b=t.a; c=t.b; abn=t.can; bcn=t.abn; can=t.bcn})
			| _ -> (t)
	in
	
	let connect ts old nu = 
		if ts >= 0 && ts < ntri then (
			let s = tt.(ts) in
			if s.abn = old then (tt.(ts)).abn <- nu ; 
			if s.bcn = old then (tt.(ts)).bcn <- nu ; 
			if s.can = old then (tt.(ts)).can <- nu ; 
		)
	in
	
	let angle a b c = abs_float (Pts2.crossnorm 
		(Pts2.sub b a) 
		(Pts2.sub c b) ) in
	let minangle2 j k = (* minimum angle of two triangles *)
		List.fold_left min 2.0 
			[(angle j.a j.b j.c);(angle j.b j.c j.a);(angle j.c j.a j.b);
			(angle k.a k.b k.c);(angle k.b k.c k.a);(angle k.c k.a k.b)]
	in
	let orient fn frot = 
		(* common edge is supplied in com wrt f : 0=ab 1=bc 2=ca *)
		let f = rotate fn frot in (* rotate f so that edge AB is facing triangle g *)
		let gn = f.abn in
		let g0 = tt.(gn) in
		let grot = if fn = g0.abn then 0 
			else if fn = g0.bcn then 1 
			else if fn = g0.can then 2
			else 0
		in
		let g = rotate gn grot in 
		f, g
	in
	let flip fn gn frot = 
		if fn >= 0 && fn < ntri && gn >= 0 && gn < ntri then (
			(* given two triangles f and g whch share a common 
			edge, flip this edge if it would make the smallest angle in both 
			larger.  (I could do the delaunay criteria, but...lazy). *)
			let f,g = orient fn frot in
			(* see if there are any angles > 180 - if so, cannot flip
			since the corresponding quadralateral is concave *)
			(* printf "trying flip f, g :\n%!"; printtri f ; printtri g; *)
			(* generate two suggestion triangles *)
			let fp = {a=f.a; b=g.c; c=f.c; abn=g.bcn; bcn=gn; can=f.can} in
			let gp = {a=g.a; b=f.c; c=g.c; abn=f.bcn; bcn=fn; can=g.can} in
			(* printf "-- to fp, gp : \n%!"; printtri fp ; printtri gp; *)
			(* test to see if the two proposed triangles remain CCW *)
			(* if not, then the quad perimeter has changed *)
			if ccw fp > 0.0 && ccw gp > 0.0 then ( 
			(* 
			let angtest (a,b,c) = (Pts2.crossnorm (Pts2.sub a b) (Pts2.sub b c)) < 0.0 in
			if not (List.exists angtest 
					[(f.c,f.a,g.c);(f.a,g.c,f.b);(g.c,f.b,f.c);(f.b,f.c,f.a)]) then (
					*)
				(* measure the mininum internal angle *)
				(* abs so it is independent of the order of vertices 
				(even though we always create them in a ccw way *)
				let fpgpa = minangle2 fp gp in
				let fga = minangle2 f g in
				if fpgpa > fga then (
					tt.(fn) <- fp ; 
					tt.(gn) <- gp ; 
					(* connect the exterior triangles correctly then *)
					connect f.bcn fn gn ; 
					connect g.bcn gn fn ; 
					gselected := fn ; 
					gselecthot := true ; 
					if !tcnt mod 20 = 0 then (!renderfunc) (); 
					(*gselected := gn ; 
					(!renderfunc) (); *)
					(* printf "flipping!\n%!"; *)
				) else ( (*printf "failed minimum angle test before %f after %f \n%!" fga fpgpa;*))
			) else ( (* printf "failed cross prod angle test\n%!";*) )
		)
	in
	let distt a d =
		let x,y = Pts2.abs (Pts2.sub a d) in
		if x > 0.00002 || y > 0.00002 then true 
		else false
	in
	let distok t d = 
		(distt t.a d) && (distt t.b d) && (distt t.c d)
	in
	let rec subdivide tn d = 
		(* given point d in triangle tn (index#), subdivide, and update the edge-links *)
		(* have to introduce three new triangles here*)
		(* first test to make sure it is not close to the vertices *)
		let t = tt.(tn) in (* make a copy *)
		if distok t d then (
			let abdn = tn in (* reuse this spot *)
			let bcdn = !tcnt in
			incr tcnt ; 
			let cadn = !tcnt in
			incr tcnt ; 
			tt.(abdn) <- {a=(t.a); b=(t.b); c=d; abn=(t.abn); bcn=bcdn; can=cadn} ; 
			tt.(bcdn) <- {a=(t.b); b=(t.c); c=d; abn=(t.bcn); bcn=cadn; can=abdn} ; 
			tt.(cadn) <- {a=(t.c); b=(t.a); c=d; abn=(t.can); bcn=abdn; can=bcdn} ; 
			(* connect back in *)
			connect (t.abn) tn abdn ; 
			connect (t.bcn) tn bcdn ; 
			connect (t.can) tn cadn ; 
			(* now need to flip / test for flip each of these *)
			flip abdn t.abn 0 ; 
			flip bcdn t.bcn 0 ; 
			flip cadn t.can 0 ; (* all these have edge AB aligned with the edges of the former large triangle *)
		) (* else (
			printf "subdivide point (%f , %f) rejected, too close to existing point\n%!" (fst d) (snd d) ;
		) *)
	in

	let findcontaining d = 
		(* given a point, find the index of the triangle contains it *)
		(* if it is not found, return -1 *)
		(* this is for inserting points, not edges *)
		let n = ref (!tlast) in
		let g = ref d in (* snapped point *)
		let found = ref false in
		let prev = ref (-1) in
		let prev2 = ref (-1) in
		(* printf "findcontaining with (%f,%f)\n%!" (fst d) (snd d) ; *)
		while !n >= 0 && !n < ntri && not !found do (
			let t = tt.(!n) in
			(* printf "(%f,%f) looking at " (fst d) (snd d); printtri t ;
			gselected := !n ; 
			gselecthot := false; 
			(!renderfunc) ();  *)
			found := insidetri t.a t.b t.c d ; 
			if not !found then (
				if distok t d then (
					(* find a point within this triangle *)
					let e = Pts2.scl (Pts2.add (Pts2.add t.a t.b) t.c) 0.333333333 in
					(* see which side this intersects with, and move to that triangle *)
					if fst (Pts2.intersectBool t.a t.b d e) then( n := t.abn ; )
					else if fst (Pts2.intersectBool t.b t.c d e) then( n := t.bcn ; )
					else if fst (Pts2.intersectBool t.c t.a d e) then( n := t.can ; )
					else(
						if distok t d then (
							let testseg a b = 
								(* this snaps to an edge, to prevent CW triangles *)
								let fnd,h = ptonline a b d in
								if fnd then g := h ; 
								fnd
							in
							if testseg t.a t.b || testseg t.b t.c || testseg t.c t.a then (
								(* it is on an edge *)
								found := true ; 
							) else (
								printf "does not intersect and point not on an edge\n%!" ; 
								n := -1 ;
							)
						) else ( (* this point is already in the mesh *) 
							(* printf "find point (%f , %f) rejected, too close to existing point\n%!" 
								(fst d) (snd d) ; *)
							n := -1 ; 
						);
					)
				) else ( (* this point is already in the mesh *) 
					(* printf "find point (%f , %f) rejected, too close to existing point\n%!" 
						(fst d) (snd d) ; *)
					n := -1 ; 
				); 
			); 
			(* see if we have a loop - 
			this means we are oscillating, and either triangle is good*)
			if !n = !prev2 then found := true ; 
			prev2 := !prev;
			prev := !n
		) done ; 
		(* update for next time *)
		if !n >= 0 && !n < ntri then tlast := !n ;
		!n, !g
	in
	
	let rec edgefind d e = (
		(* look for intersections between triangle edges and segment d e *)
		(* first find the triangle that d is in *)
		let n = ref (!tlast) in
		let addpoint = ref false in
		let addseg = ref false in
		let found = ref false in
		let g =  ref (0.0, 0.0) in
		(* original algorithm: use the links between the triangles to move toward 
		the best triangle ; 
		present more brute-force algorithm: just choose the triangle that has a vertex
		closest to d, and return that  *)
		let mindist = ref 1e60 in
		let d2 = ref d in
		let dist (x,y) (w,z) = (x -. w)*.(x -. w)+.(y -. z)*.(y -. z) in (* squared distance *)
		for i = 0 to (Array.length tt)-1 do (
			let dista = dist (tt.(i)).a d in
			let distb = dist (tt.(i)).b d in
			let distc = dist (tt.(i)).c d in
			if dista < distb && dista < distc && dista < !mindist then (
				n := i; 
				d2 := (tt.(i)).a; 
				mindist := dista; 
			)else if distb < dista && distb < distc && distb < !mindist then (
				n := i; 
				d2 := (tt.(i)).b; 
				mindist := distb; 
			)else if distc < dista && distc < distb && distc < !mindist then (
				n := i; 
				d2 := (tt.(i)).c; 
				mindist := distc; 
			)
		) done; 
		(* while !n >= 0 && !n < ntri && not !found do (
			(* this search will be different from above - the point must be 
			a vertex of the triangle *)
			let t = tt.(!n) in
			gselected := !n ; 
			(!renderfunc) (); 
			printf "%% looking for a triangle which contains this point as a vertex\n%!"; 
			debugTris t d e;
			debugTris tt.(t.abn) d e;
			debugTris tt.(t.bcn) d e; 
			debugTris tt.(t.can) d e; 
			if not (distok t d) then (
				found := true ; (* a vertex!*)
			) else (
				(* start from center of triangle, project, move. *)
				let f = scl (add (add t.a t.b) t.c) 0.33333333 in
				if fst (Pts2.intersectBool t.a t.b f d) then( n := t.abn ; )
				else if fst (Pts2.intersectBool t.b t.c f d) then( n := t.bcn ; )
				else if fst (Pts2.intersectBool t.c t.a f d) then( n := t.can ; )
				else( (* must be close to one of the vertices *)
					let da = Pts2.distance t.a f in
					let db = Pts2.distance t.b f in
					let dc = Pts2.distance t.c f in
					if da < db && da < dc then n := t.abn ; 
					if db < da && db < dc then n := t.bcn ; 
					if dc < da && dc < db then n := t.can ; 
				) ; 
			) ; 
		) done ; *)
		(* update for next time *)
		if !n >= 0 && !n < ntri then tlast := !n ;
		found := false ;
		let f = !d2 in
		ghotseg := (d,e) ; 
		while !n >= 0 && !n < ntri && not !found do (
			let v = tt.(!n) in
			gselected := !n ; 
			(* if e is already in this triangle, then it is in the list of segments *)
			(* (it is an edge of the triangle) *)
			if not (distok v e) then ( 
				found := true ; 
				addpoint := false ; 
				addseg := false ; 
			) else (
				let u = if not (distt v.a f) then v
					else if not (distt v.b f) then rotate !n 1 
					else rotate !n 2 
				in
				(* printf "looking for intersection with edges of triangle\n%!";
				debugTris u f e; *)
				let ab = Pts2.norm (sub u.b u.a) in
				let ac = Pts2.norm (sub u.c u.a) in
				let ae = Pts2.norm (sub e u.a) in
				let x = Pts2.cross ab ae in
				let xd = Pts2.dot ab ae in
				let y = Pts2.cross ac ae in
				let yd = Pts2.dot ac ae in
				if abs_float x < 0.00001 then (
					(* segments ab and de are parallel - check the dot product. *)
					if xd > 0.0 then (
						(* parallel. we already know b != e so shorten segment to be *)
						g := u.b ; 
						found := true ; 
						addpoint := false ; 
						addseg := true; 
					) else (
						(* antiparallel. this is not the right triangle - move to the one off segment ac *)
						n := u.can ; 
					); 
				) else if abs_float y < 0.00001 then (
					(* segments ac and de are parallel - check the dot product. *)
					if yd > 0.0 then (
						(* parallel. we already know c != e so shorten segment to ce *)
						g := u.c ; 
						found := true ; 
						addpoint := false ; 
						addseg := true;
					) else (
						(* antiparallel. move to the triangle off segment ab. *)
						n := u.abn ; 
					); 
				) else ( 
					if x > 0.0 then ( (* CCW angle *)
						if y < 0.0 then ( (* CW angle *)
							(* must intersect segment BC *)
							let sect,h = Pts2.intersectBool u.b u.c f e in
							if not sect then(
								(* did not hit, but close to an endpoint, so just use that *)
								found := true ; 
								addpoint := false ; 
								addseg := true ;
								if abs_float x < abs_float y then g := u.b else g := u.c ;
								(* printf "error in edgefind: segments should intersect but do not\n%!" ; 
								debugTris u f e; 
								(!renderfunc) (); 
								I've checked a few of these instances, and the code seems to do the right thing
								(fingers crossed!*)
								gselected := !n ; 
							) else (
								found := true ; 
								addpoint := true ; 
								addseg := true ; 
								g := h ; 
							)
						) else (
							(* the angle is larger - move to triangle off AC *)
							n := u.can ; 
						) ; 
					) else (
						(* the angle is less - move to triangle off AB *)
						n := u.abn ; 
					) ;
				) ; 
			) ; 
		) done ; 
		if !n >= 0 && !n < ntri then tlast := !n ;
		!addpoint, !addseg, !g , e (* never update the far endpoint - only the closer *)
	) in
	
	let insert d = 
		(* printf "inserting (%f,%f)\n%!" (fst d) (snd d) ; *)
		let tn,e = findcontaining d in
		if tn >= 0 && tn < ntri then subdivide tn e
	in
	
	(* ok, we start with two triangles which are slightly larger than the bounding box
	and subdivide them *)
	let (minx,miny) = 
		List.fold_left (fun (mx,my) (x,y)  -> (min mx x),(min my y)) 
		(List.hd pts) pts  in
	let (maxx,maxy) = 
		List.fold_left (fun (mx,my) (x,y)  -> (max mx x),(max my y))
		(List.hd pts) pts in
	let (ex,ey) = Pts2.scl (Pts2.sub (maxx,maxy) (minx,miny)) 0.25 in
	let ba = (minx -. ex, miny -. ey) in
	let bb = (minx -. ex, maxy +. ey) in
	let bc = (maxx +. ex, maxy +. ey) in
	let bd = (maxx +. ex, miny -. ey) in
	gcenter := Pts2.scl (Pts2.add ba bc) 0.5 ; 
	gzoom := 1.9 /. (max (maxx -. minx) (maxy -. miny)); 
	glwind#setZoom (!gzoom,!gzoom,!gzoom) ; 
	glwind#setPan (((fst !gcenter) *. -1.0), (snd !gcenter)) ;
	tcnt := 0; 
	tt.(0) <- {a=bb; b=ba; c=bc; abn=(-1); bcn=1; can=(-1)}; 
	incr tcnt; 
	tt.(1) <- {a=bd; b=bc; c=ba; abn=(-1); bcn=0; can=(-1)};
	incr tcnt; 
	gannosegs := segs ; 
	List.iter insert pts ; 
	
	let opt () = 
		for k = 0 to !tcnt -1 do (
			let t = tt.(k) in
			flip k t.abn 0 ; 
		) done ; 
		for k = 0 to !tcnt -1 do (
			let t = tt.(k) in
			flip k t.bcn 1 ; 
		) done ; 
		for k = 0 to !tcnt -1 do (
			let t = tt.(k) in
			flip k t.can 2 ; 
		) done ; 
		gselected := 0 ; 
	in
	(* ok, need to make sure no triangles span segments *)
	(* if they do, we simply insert a new point until no segments are left w/ intersections *)
	let rec eliminate ss w dopt chthr = 
		(* ss = list of segments; w= recursion counter *)
		if List.length ss > 0 && w < 1000 then (
			(* only optimize if we've eliminated at least 5 segments *)
			let dopt2 = if chthr - (List.length ss) > 5 then true else false in
			if dopt && dopt2 then( (!renderfunc) (); opt () ; ); 
			(* printf "eliminate %d segments\n%!" (List.length ss) ; *)
			let morpts = ref [] in
			let morsegs = ref [] in
			List.iter (fun (a,b) -> 
				let addpoint,addseg,g,e = edgefind a b in
				if addpoint then ( morpts := g :: (!morpts); ); 
				if addseg then ( morsegs := (e,g) :: (!morsegs); ); 
			) ss ; 
			printf "eliminate segments:  %d points\n%!" (List.length !morpts) ;
			gannopts := !morpts ; 
			gannosegs := !morsegs ; 
			List.iter insert !morpts ; 
			!renderfunc ();
			let newthr = if dopt2 then (List.length !morsegs) else chthr in
			eliminate !morsegs (w+1) dopt newthr; 
		) ; 
	in
	
	eliminate segs 0 true (List.length segs + 10); 
	(* second pass to make sure all segments are there  - 
	optimization may have caused a segment to be crossed after the 
	segment was shortened during iteration *)
	opt () ;
	eliminate segs 0 false (List.length segs + 10);  
	
	gannosegs := segs ; 
	let tl = Array.to_list ( Array.map 
		(fun t -> (t.a, t.b, t.c)) 
		(Array.sub tt 0 (!tcnt)) ) in
	let tl2 = List.filter filter tl in
	let k = ref 0 in
	List.iter (fun (d,e,f) -> 
		tt.(!k) <- {a=d;b=e;c=f;abn=0;bcn=0;can=0};
		incr k; 
	) tl2 ; 
	tcnt := List.length tl2 ; 
	!renderfunc () ; 
	tl2
	;;