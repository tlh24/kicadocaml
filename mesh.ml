(* Copyright 2008-2010, Timothy L Hanson *)
(* This file is part of Kicadocaml.

    Kicadocaml is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Kicadocaml is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Kicadocaml.  If not, see <http://www.gnu.org/licenses/>.
*)
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
let dot (ax,ay) (bx,by) = 
	ax *. bx +. ay *. by
	;;
let norm (x,y) = 
	let sz = sqrt(x *. x +. y *. y ) in
	x /. sz , y /. sz
	;;
let length (x,y) = 
	sqrt(x *. x +. y *. y )
	;;
let distance2 a b = 
	let x,y = sub a b in
	x *. x +. y *. y
	;;
let crossnorm a b = 
	cross (norm a) (norm b)
	;;
let insidetri a b c d = (* seems to be the most robust and efficient method *)
	cross (sub b a) (sub d a) > 0.0 &&
	cross (sub c b) (sub d b) > 0.0 &&
	cross (sub a c) (sub d c) > 0.0 
	;; 
let ccw r = 
	let ba = sub r.b r.a in
	let cb = sub r.c r.b in
	let ac = sub r.a r.c in
	cross ba cb > 0.0 && cross cb ac > 0.0 && cross ac ba > 0.0
	;;
let angle a b c = abs_float (crossnorm (sub b a) (sub c b) ) 
	;;
let minangle j = 
	let e = angle j.a j.b j.c in
	let f = angle j.b j.c j.a in
	let g = angle j.c j.a j.b in
	min e (min f g)
	;;
let minangle2 j k = min (minangle j) (minangle k)
	;;
let ptonline a b c = 
	let d = sub b a in
	let e = sub c a in
	let f = abs_float (crossnorm d e )in
	if f < 0.00001 then (
		let g = add (scl d ((length e)/.(length d))) a in
		if length d >= length e then true,g else false,g
	) else false, (0.0,0.0)
	;;
	
let printtri t = 
	let s = if ccw t then "CCW" else "CW" in
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
	
let render togl = (
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
	);;
		
let makewindow top = (
	let wind = Toplevel.create top  in
	Wm.title_set wind "mesh view";
	let togl = Togl.create ~width:1000 ~height:1000 
		~rgba:true ~double:true ~depth:true wind in
	glwind#bind wind togl ; 
	glwind#reshape togl ; 
	glwind#setRenderCB (fun () -> render togl );
	renderfunc := (fun () -> glwind#render togl) ; 
	);;
	
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
		aligning triangles for the flip-test operation. mantains CCW orientation *)
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
			(* generate two suggestion triangles *)
			let fp = {a=f.a; b=g.c; c=f.c; abn=g.bcn; bcn=gn; can=f.can} in
			let gp = {a=g.a; b=f.c; c=g.c; abn=f.bcn; bcn=fn; can=g.can} in
			(* printf "-- to fp, gp : \n%!"; printtri fp ; printtri gp; *)
			(* test to see if the two proposed triangles remain CCW *)
			(* if not, then the quad perimeter has changed *)
			if ccw fp && ccw gp then ( 
				(* measure the mininum internal angle *)
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
					if !tcnt mod 45 = 0 then (!renderfunc) (); 
					(*gselected := gn ; 
					(!renderfunc) (); *)
					(* printf "flipping!\n%!"; *)
				) else ( (*printf "failed minimum angle test before %f after %f \n%!" fga fpgpa;*))
			) else ( (* printf "failed cross prod angle test\n%!";*) )
		)
	in
	let distt a d =
		let x,y = (sub a d) in
		abs_float x > 0.00002 || abs_float y > 0.00002
	in
	let distok t d = 
		(distt t.a d) && (distt t.b d) && (distt t.c d)
	in
	let subdivide tn d = 
		(* given point d in triangle tn (index#), subdivide, and update the edge-links *)
		(* have to introduce three new triangles here*)
		(* first test to make sure it is not close to the vertices *)
(* 		printf "subdivide (%f,%f)\n" (fst d) (snd d);  *)
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
		)
	in
	let findcontaining2 d vertsOK = (
		(* search for the triangle which contains point d  *)
		(* this is used for insertion *)
		let n = ref (!tlast) in
		let found = ref false in
		let prev = ref (-1) in
		let prev2 = ref (-1) in
		while !n >= 0 && !n < ntri && not !found do (
			let t = tt.(!n) in
(* 			printf "(%f,%f) looking at " (fst d) (snd d); printtri t ; 
			gselected := !n ; 
			gselecthot := false; 
			(!renderfunc) (); *)
			if distok t d then (
				let e = cross (sub t.b t.a) (sub d t.a) in
				let f = cross (sub t.c t.b) (sub d t.b) in
				let g = cross (sub t.a t.c) (sub d t.c) in
				if e >= 0.0 && f >= 0.0 && g >= 0.0 then (found := true)
				else if e < 0.0 then n := t.abn (* off segment ba *)
				else if f < 0.0 then n := t.bcn
				else if g < 0.0 then n := t.can
				else (n := !n+1; n := !n mod ntri;)
			) else (
				if vertsOK then (found := true)
				else n := -1 ; 
			); 
			(* see if we have a loop - this means we are oscillating, and either triangle is good*)
			if !n = !prev2 then (found := true);
			prev2 := !prev; prev := !n
		) done ; 
		(* update for next time *)
		if !n >= 0 && !n < ntri then tlast := !n ;
		!n
	) in
	let insert d = 
(* 		printf "inserting (%f,%f)\n%!" (fst d) (snd d) ; *)
		let tn = findcontaining2 d false in
		if tn >= 0 && tn < ntri then subdivide tn d
	in
	let rec edgefind d e = (
		(* look for intersections between triangle edges and segment d e *)
		(* first find the triangle that d is in *)
		let n = ref (!tlast) in
		let addpoint = ref false in
		let addseg = ref false in
		let found = ref false in
		let g =  ref (0.0, 0.0) in
		
		n := findcontaining2 d true;
		let t = tt.(!n) in
		(* snap point d to a vertex *)
		let da = distance2 t.a d in
		let db = distance2 t.b d in
		let dc = distance2 t.c d in
		let f = if da < db && da < dc then t.a
			else if db < da && db < dc then t.b
			else t.c in
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
				let ab = norm (sub u.b u.a) in
				let ac = norm (sub u.c u.a) in
				let ae = norm (sub e u.a) in
				let x = cross ab ae in
				let xd = dot ab ae in
				let y = cross ac ae in
				let yd = dot ac ae in
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
	
	(* we start with two triangles which are slightly larger than the bounding box
	and subdivide them *)
	let (minx,miny) = 
		List.fold_left (fun (mx,my) (x,y)  -> (min mx x),(min my y)) 
		(List.hd pts) pts  in
	let (maxx,maxy) = 
		List.fold_left (fun (mx,my) (x,y)  -> (max mx x),(max my y))
		(List.hd pts) pts in
	let (ex,ey) = scl (sub (maxx,maxy) (minx,miny)) 0.25 in
	let ba = (minx -. ex, miny -. ey) in
	let bb = (minx -. ex, maxy +. ey) in
	let bc = (maxx +. ex, maxy +. ey) in
	let bd = (maxx +. ex, miny -. ey) in
	gcenter := scl (add ba bc) 0.5 ; 
	gzoom := 1.9 /. (max (maxx -. minx) (maxy -. miny)); 
	glwind#setZoom (!gzoom,!gzoom,!gzoom) ; 
	glwind#setPan (((fst !gcenter) *. -1.0), (snd !gcenter)) ;
	tcnt := 0; 
	tt.(0) <- {a=bb; b=ba; c=bc; abn=(-1); bcn=1; can=(-1)}; 
	incr tcnt; 
	tt.(1) <- {a=bd; b=bc; c=ba; abn=(-1); bcn=0; can=(-1)};
	incr tcnt; 
	gannosegs := segs ; 
	printf "inserting all points into mesh..\n%!"; 
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
	(* need to make sure no triangles span segments *)
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
(* 			printf "eliminate segments:  %d points\n%!" (List.length !morpts) ; *)
			gannopts := !morpts ; 
			gannosegs := !morsegs ; 
			List.iter insert !morpts ; 
			!renderfunc ();
			let newthr = if dopt2 then (List.length !morsegs) else chthr in
			eliminate !morsegs (w+1) dopt newthr; 
		) ; 
	in
	printf "checking that all segments are in mesh..\n%!"; 
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