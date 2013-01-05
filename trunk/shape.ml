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
open Grfx

type pcb_shape_type = Shape_Segment | Shape_Circle | Shape_Arc | Shape_Polygon

class pcb_shape = 
object
	val mutable m_stx = 0.
	val mutable m_sty = 0.
	val mutable m_enx = 0.
	val mutable m_eny = 0.
	val mutable m_width = 0.
	val mutable m_layer = 0
	val mutable m_angle = 0
	val mutable m_polycount = 0
	val mutable m_polyX : 'float list = [] 
	val mutable m_polyY : 'float list = [] 
	val mutable m_type = Shape_Segment
	val mutable m_g = new grfx
	method update rot x y = (
		m_g#updateLayer m_layer ; (* this also sets z, needed for vertices *)
		m_g#empty () ;
		(
		match m_type with
			| Shape_Polygon -> ( (*doesn't look like I have any, but write some sort of handler here ..*)
				m_g#empty () ; (*clear the list, all Grfx functions add to it.. *)
				m_g#makeTrack (m_stx,m_sty) (m_enx,m_eny) m_width ; 
				let p = List.combine m_polyX m_polyY in
				let rec drawpoly plist sx sy = 
					match plist with
					| [] -> () ; 
					| h::t -> (
						let (hx, hy) = h in
						m_g#makeTrack (sx,sy) (hx,hy) m_width;
						drawpoly t hx hy 
						); 
				in
				drawpoly p m_enx m_eny ; 
			)
			| _ -> m_g#makeTrack (m_stx,m_sty) (m_enx,m_eny) m_width ; 
		); 
		m_g#rotateTranslate rot x y ; 
	)
	method updateLayers () = m_g#updateLayer m_layer ; 
	method getZ () = m_g#getZ (); 
	method copy () = (
		m_g <- new grfx ; 
	)
	method flip () = (
		m_sty <- m_sty *. (-1.) ;
		m_eny <- m_eny *. (-1.) ; 
		m_layer <- flip_layer m_layer ; 
		(* callee must call update *)
	)
	method draw bbox = (
		ignore(m_g#draw ~hit:false bbox);
	)
	method getBBX () = (
		let w = m_width /. 2.0 in
		let xl = m_enx :: m_polyX in
		let yl = m_eny :: m_polyY in
		(List.fold_left (fun a b -> min a (b-.w)) (m_stx-.w) xl),
		(List.fold_left (fun a b -> min a (b-.w)) (m_sty-.w) yl),
		(List.fold_left (fun a b -> max a (b+.w)) (m_stx+.w) xl),
		(List.fold_left (fun a b -> max a (b+.w)) (m_sty+.w) yl)
	)
	method read ic line shapetype = (
		let parse_startend = 
			let sp = Pcre.extract ~pat:"\w+ ([\.\d-]+) ([\.\d-]+) ([\.\d-]+) ([\.\d-]+)" line in
			m_stx <- foss sp.(1) ; 
			m_sty <- foss sp.(2) ; 
			m_enx <- foss sp.(3) ; 
			m_eny <- foss sp.(4) ; 
		in
		let parse_widthlayer = 
			let sp = Pcre.extract ~pat:"\w+ [\.\d-]+ [\.\d-]+ [\.\d-]+ [\.\d-]+ ([\.\d-]+) (\d+)" line in
			m_width <- foss sp.(1) ; 
			m_layer <- ios sp.(2) ; 
		in
		(
			match shapetype with
				| Shape_Segment -> ( 
					parse_startend ; 
					parse_widthlayer ;
				)
				| Shape_Circle -> (
					parse_startend ; 
					parse_widthlayer ; 
				)
				| Shape_Arc -> (
					parse_startend ; 
					let sp = Pcre.extract ~pat:"\w+ \d+ \d+ \d+ \d+ (\d+) ([\.\d-]+) (\d+)" line in
					m_angle <- ios sp.(1) ; 
					m_width <- foss sp.(2) ; 
					m_layer <- ios sp.(3) ; 
				)
				| Shape_Polygon -> (
					parse_startend ;
					let sp = Pcre.extract ~pat:"\w+ \d+ \d+ \d+ \d+ (\d+) ([\.\d-]+) (\d+)" line in
					m_polycount <- ios sp.(1) ; 
					m_width <- foss sp.(2) ; 
					m_layer <- ios sp.(3) ; 
					for i = 1 to m_polycount do
						let ll = input_line2 ic in
						let spp = Pcre.extract ~pat:"Dl ([\.\d-]+) ([\.\d-]+)" ll in
						m_polyX <- (( foss spp.(1) ) :: m_polyX ) ; 
						m_polyY <- (( foss spp.(2) ) :: m_polyY ) ; 
					done
				)
		) ; 
		m_type <- shapetype ; 
	)
	method save oc = (
		let save_startend oc typ = 
			fprintf oc "%s %s %s %s %s" typ 
			(sofs m_stx) (sofs m_sty) (sofs m_enx) (sofs m_eny) ; 
		in
		let save_widthlayer oc = 
			fprintf oc " %s %d\n" (sofs m_width) m_layer ; 
		in
		match m_type with
			| Shape_Segment -> (
				save_startend oc "DS" ; 
				save_widthlayer oc ; 
			)
			| Shape_Circle -> (
				save_startend oc "DC" ; 
				save_widthlayer oc ; 
			)
			| Shape_Arc -> (
				save_startend oc "DA"; 
				fprintf oc " %d" m_angle; 
				save_widthlayer oc; 
			)
			| Shape_Polygon -> (
				save_startend oc "DP"; 
				fprintf oc " %d" m_polycount ; 
				save_widthlayer oc ; 
				List.iter2 (fun x y -> 
					fprintf oc "Dl %s %s\n" (sofs x) (sofs y);
				) m_polyX m_polyY ; 
			)
	)
end ;;