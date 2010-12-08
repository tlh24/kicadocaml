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
let sub a b = 
	(fst a) -. (fst b) , (snd a) -. (snd b) 
	;; 
let add a b = 
	(fst a) +. (fst b) , (snd a) +. (snd b) 
	;; 
let scl a b = 
	(fst a) *. b ,  (snd a) *. b
	;;
let div a b = 
	(fst a) /. (fst b) , (snd a) /. (snd b) 
	;;
let abs (x,y) = (abs_float x, abs_float y)
	;;
let norm (x,y) = 
	let sz = sqrt(x *. x +. y *. y ) in
	x /. sz , y /. sz
	;;
let dot a b = 
	(fst a) *. (fst b) +. (snd a) *. (snd b) 
	;;
let dotnorm a b = 
	dot (norm a) (norm b)
	;;
let dotangle a b = 
	acos (dotnorm a b)
	;;
let cross (ax,ay) (bx,by) = 
	(ax *. by) -. (ay *. bx)
	;;
let crossnorm a b = 
	cross (norm a) (norm b)
	;;
let crossangle a b = 
	asin (crossnorm a b)
	;;
let fois a b = 
	(float_of_int a) /. 10000. , (float_of_int b) /. 10000. 
	;;
let foist a = 
	(float_of_int (fst a)) /. 10000. , (float_of_int (snd a)) /. 10000. 
	;;
let iofs a = 
	(int_of_float ((fst a) *. 10000.)), (int_of_float ((snd a) *. 10000.))
	;;
let length a = 
	let x, y = a in
	let sq w = w*.w in
	sqrt( (sq x) +. (sq y) )
	;;
let length2 a = 
	let x, y = a in
	let sq w = w*.w in
	(sq x) +. (sq y)
	;;
let distance a b = 
	let c = sub a b in
	length c 
	;;
let distance2 a b =  (*the squared distance*)
	let x, y = sub a b in
	x *. x +. y *. y
	;;
let rotateMove (ax,ay) =  
(* returns the *vector*, which when added to a, rotates it 90 deg CW *)
	( ay -. ax , (-1.0) *. ax -. ay)
	;;
let rotate (x,y) r = (* rotate a vector about z *)
	(* I honestly don't know the convention; this is CCW in normal coords*)
	(cos(r) *. x +. sin(r) *. y),(cos(r) *. y -. sin(r) *. x)
	;;
let closestuonline a b c clamp = 
	let (ax, ay) = a in 
	let (bx, by) = b in 
	let (cx, cy) = c in
	let ab = sub b a in
	let lab = length ab in
	let u = 
		if (abs_float (bx -. ax)) < 0.0001 then ( (* vertical *)
			if (abs_float (by -. ay)) < 0.0001 then (
				0. 
			)else(
				(* print_endline "vertical in closestuonline"; *)
				(cy -. ay) /. (by -. ay) )
		) else if (abs_float (by -. ay)) < 0.0001 then ( (* horizontal *)
			(* print_endline "horizontal in closestuonline"; *)
			(cx -. ax) /. (bx -. ax) 
		) else (
			((cx -. ax)*.(bx -. ax) +. (cy -. ay)*.(by -. ay )) /. (lab *. lab) 
		)
	in
	if clamp then (
		if u < 0. then 0.
		else if u > 1. then 1.
		else u
	) else  u 
	;;
let closestpointonline a b c clamp = 
	let u = closestuonline a b c clamp in
	let ab = sub b a in
	add a (scl ab u)
	;;
	(*
let closespointdistance a b c = 
	let d = closestpointonline a b c true in
	distance c d
	;;
	*)
let closestpointdistance a b c reverse = 
	(* output : distance  and point on line ab and c *)
	(* if reverse, then the order of the output arguments is inverted *)
	(* this is for consistency with DRC, when the order of the input arguments is reversed*)
	let d = closestpointonline a b c true in
	if reverse then (
		(distance c d),c,d
	) else (
		(distance c d),d,c
	)
	;;
let relativelen a b = 
	(* returns length of b relative to a *)
	(* assumes two vectors are parallel *)
	(* if they are the same length, result = 1 *)
	(dot a b) /. (length2 a)
	
let clamp1 a b c = 
	if a < b then b 
	else (
		if a > c then c
		else a
	)
	;;
	
let parallel a b c d = 
	(* see if two lines are parallel *)
	if distance2 a b < 0.0001 || distance2 c d < 0.0001 then( false )
	else (
		let ab = sub b a in
		(* a prime is the origin *)
		let xx = norm ab in
		let yy = (-1.) *. (snd xx) , (fst xx) in
		let project e = 
			(dot (sub e a) xx) , (dot (sub e a) yy)
		in
		let cp = project c in
		let dp = project d in
		let cd = sub dp cp in
		if abs_float (snd cd) < 0.001 then true
		else false
	)
	;;
let intersectPt a b c d = 
	(* find the point at which two lines intersect *)
	(* assumes that the lines are valid & non-parallel *)
	let ab = sub b a in
	(* a prime is the origin *)
	let bp = length ab in
	let xx = norm ab in
	let yy = (-1.) *. (snd xx) , (fst xx) in
	let project e = 
		(dot (sub e a) xx) , (dot (sub e a) yy)
	in
	let cp = project c in
	let dp = project d in
	let cd = sub dp cp in
	let m = (fst cd) /. (snd cd) in
	let o = (fst cp) -. m *. (snd cp) in
	(* o is the x-intersect *)
	add (scl ab (o /. bp)) a 
	;;
let intersectBool a b c d = 
	(* see if two line segments intersect *)
	let ab = sub b a in
	(* a prime is the origin *)
	let bp = length ab in
	let xx = norm ab in
	let yy = (-1.) *. (snd xx) , (fst xx) in
	let project e = 
		(dot (sub e a) xx) , (dot (sub e a) yy)
	in
	let cp = project c in
	let dp = project d in
	let cd = sub dp cp in
	let m = (fst cd) /. (snd cd) in
	let o = (fst cp) -. m *. (snd cp) in
	let e = add (scl ab (o /. bp)) a in
	(* cp and dp must span the x-axis *)
	if ((snd cp) <= 0. && (snd dp) >= 0.) || ((snd cp) >= 0. && (snd dp) <= 0.) then (
		if o >= 0. && o <= bp then ( true, e )
		else ( false, e )
	) else ( false, e )
	;;
let intersect a b c d =
	(* see if two line-segments intersect *)
	(* rather than dealing with weird end-cases, we just 
	rotate & translate the coordinate frame. *)
	(* return a squared distance, not a bool *)
	(* also return two points of intersection (sorta, they are recomendations)) *)
	if distance2 a b = 0. || distance2 c d = 0. then (
		1e24, a, c
	) else (
		let ab = sub b a in
		(* a prime is the origin *)
		let bp = length ab in
		let xx = norm ab in
		let yy = (-1.) *. (snd xx) , (fst xx) in
		let project e = 
			(dot (sub e a) xx) , (dot (sub e a) yy)
		in
		let cp = project c in
		let dp = project d in
		let cd = sub dp cp in
		if (snd cd) = 0. then ( (*lines are parallel *)
			(distance a c), a, c
		) else if (fst cd) = 0. then ( (* lines are perpendicular *)
			(* finding the point of intersection is simple *)
			let intr = fst cp in
			let u = intr /. bp in
			let v = (-1. *. (snd cp))/.(snd cd) in
			(* if they intersect, scale back v & return closest point there *)
			if u >= -0.001 && u <= 1.001 && v >= -0.001 && v <= 1.001 then (
				(* print_endline "in intersect : perpendicular hit!"; *)
				(* they intersect *)
				let e = add (scl ab (0.99 *. u)) a in
				let _,f,_ = closestpointdistance c d e false in
				-0.1, e, f 
			(* the -0.1 is to prevent closestpointdistance from 'winning' 
			and setting the suggested values *)
			) else ( 
				1e24, a, c
			)
		) else (
			(* they are at an angle to eachother *)
			let m = (fst cd) /. (snd cd) in
			let o = (fst cp) -. m *. (snd cp) in
			(* o is the x-intersection. *)
			let co = sub (o, 0.) cp in
			let v = relativelen cd co in
			let u = o /. bp in
			if u >= -0.001 && u <= 1.001 && v >= -0.001 && v <= 1.001 then (
				(* they intersect!! *)
				(* print_endline "in intersect : angled hit!"; *)
				let e = add (scl ab (0.99 *. u)) a in
				let _,f,_ = closestpointdistance c d e false in
				-0.1, e, f
			) else (
				1e24, a, c
			)
		)
	)
	;;
	
let linedistance a b c d = 
	let ar = [| 
		(closestpointdistance a b c false);  (* last bool to indicate that the points should be reversed in order *)
		(closestpointdistance a b d false); 
		(closestpointdistance c d a true ); 
		(closestpointdistance c d b true ); 
		(intersect a b c d) |] in
	let min = ref 1e31 in
	let ee = ref (1. , 0.) in
	let ff = ref (0. , 1.) in
	Array.iter (fun (l,e,f) -> 
		if l <= !min then (
			min := l; 
			ee := e; 
			ff := f; 
		); 
	) ar; 
	!min, !ee, !ff
	;;

let tracktouch a b c width = 
	(* see if a point is touching a track *)
	let d = closestpointonline a b c true in
	distance2 c d < width *. width
	;;