open Printf
open Comm
open Mesh

let printpoly poly = 
	List.iter (fun (x,y) -> printf "%f , %f \n" x y) poly; 
	;;

let inside poly (x,y) = 
		(* determines if a given point is within the polygon defined by the vertices m_poly*)
		(* the list of verticies MUST LOOP - that is, 
		the first must be the same as the last for the alg to work properly. *)
		let cnt =ref 0 in
		let x0 = ref 0.0 in
		let y0 = ref 0.0 in
		if List.length poly > 2 then (
			let (x2,y2) = (List.hd poly) in
			x0 := x2 ; y0 := y2 ;
			List.iter (fun (x1,y1) -> 
				(* printf "x0 y0 x1 y1 %f %f %f %f \n%!" !x0 !y0 x1 y1 ; *)
				if abs_float (y1 -. !y0) > 0.00001 then (
					let u = (y -. !y0) /. (y1 -. !y0) in
					(* printf "u %f\n%!" u ; *)
					if u >= 0.0 && u < 1.0 then (
						let xx = !x0 +. (x1 -. !x0) *. u in
						(* printf "xx %f\n%!" xx ; *)
						if xx >= x then(
							(*printf " point %f,%f left of/inside segment %f,%f | %f,%f \n%!"
								x y !x0 !y0 x1 y1 ;*)
							incr cnt ; 
						)
					);
				) ; 
				x0 := x1 ; 
				y0 := y1 ;
			) (List.tl poly) ; 
		); 
		if (!cnt mod 2) = 0 || !cnt = 0 then 
			((* printf "outside\n%!";*) false)
		else ((* printf "inside\n%!";*) true)
	;;
	
let ptsinsidetri a b c pts = 
	(* are any of the points inside triangle a b c *)
	(* points are float*float*bool - bool must be true.*)
	let found = ref false in
	Array.iter ( fun (x,y,z) -> 
		if z && (not !found) then (
			found := insidetri a b c (x,y) ;  
		) ; 
	) pts ; 
	!found
	;;
	
let rec maketri verts tris = 
	printf "calling maketri with \n%!" ; 
	Array.iter (fun (x,y,_) -> printf "%f , %f \n" x y) verts; 
	(* make tris a reference to a list for efficiency *)
	let len = Array.length verts in
	if len < 3 then (
		()
	)else if len = 3 then (
		let (ax,ay,_) = verts.(0) in
		let (bx,by,_) = verts.(1) in
		let (cx,cy,_) = verts.(2) in
		if not ((ax = bx) && (ax = cx)) && not ((ay = by) && (ay = cy)) then (
			tris := ((ax,ay),(bx,by),(cx,cy)) :: !tris ; 
		); 
	) else (
		let k = ref 0 in
		while !k < len do (
			let d = mod2 (!k-1) len in
			let e = !k in
			let f = (!k+1) mod len in
			let (ax,ay,az) = verts.( d ) in (* regular mod does not work here *)
			let (bx,by,bz) = verts.( e ) in
			let (cx,cy,cz) = verts.( f ) in
			if az && bz && cz then (
				let crs = Pts2.crossnorm (Pts2.sub (bx,by) (ax,ay)) (Pts2.sub (cx,cy) (bx,by)) in
				if crs > 0.0 then (
					(* printf "cross positive (%f,%f) (%f,%f) (%f,%f)\n"
						ax ay bx by cx cy ; *)
					verts.( d ) <- (ax,ay,false); (* mark them as ignore for the ptsinside alg *)
					verts.( e ) <- (bx,by,false); 
					verts.( f ) <- (cx,cy,false); 
					let inside = ptsinsidetri (ax,ay) (bx,by) (cx,cy) verts in
					(* if inside then printf "inside\n%!" else printf "outside\n%!" ; *)
					if not inside && not ((ax = bx) && (ax = cx)) && not ((ay = by) && (ay = cy)) then (
						(* add the triangle to the list *)
						tris := ((ax,ay),(bx,by),(cx,cy)) :: !tris ; 
						incr k ; 
						incr k ; (* twice so we get to the next potential triangle *)
					) else ( 
						verts.( e ) <- (bx,by,true); (* otherwise keep it in the queue *)
						incr k ; 
					); 
					verts.( d ) <- (ax,ay,true); (* reset markers *)
					verts.( f ) <- (cx,cy,true); 
				) else ( incr k ) ;
			) else ( incr k );  
		) done ; 
		(* now filter out the used points, and recurse *)
		let goodpts = Array.fold_left (fun n (_,_,z) -> if z then n+1 else n) 0 verts in
		let verts2 = Array.make goodpts (0.0,0.0,true) in
		let d = ref 0 in
		Array.iter (fun (x,y,z) -> 
			if z then (
				verts2.(!d) <- (x,y,z); 
				incr d; 
			); 
		) verts ; 
		maketri verts2 tris ; (* true tail-recursion? *)
	)
	;;

let maketri2 poly = 
	(* wrapper for the actual recursive algorithm *)
	(* I think arrays will be easier to work with here *)
	(* make an array of float*float*bool - bool for seeing if point is valid. *)
	let verts = Array.of_list (List.map (fun (a,b) -> (a,b,true)) poly) in
	let len = (Array.length verts) in
	let angle = ref 0.0 in
	for i = 0 to len-1 do (
		let (ax,ay,_) = verts.(mod2 (i-1) len) in (* regular mod does not work here *)
		let (bx,by,_) = verts.( i ) in
		let (cx,cy,_) = verts.((i+1) mod len) in
		angle := !angle +. Pts2.crossangle 
			(Pts2.sub (bx,by) (ax,ay))
			(Pts2.sub (cx,cy) (bx,by)) ; 
	) done ; 
	let revarray ar = 
		let l = (Array.length ar) - 1 in
		Array.mapi (fun i _ -> ar.(l - i)) ar
	in
	let verts2 = if !angle < 0.0 then revarray verts else verts in
	let tris = ref [] in
	maketri verts2 tris ; 
	!tris (* return our list of triangles *)
	;;