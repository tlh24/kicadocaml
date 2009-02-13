open Printf
open Comm
open Grfx

type pcb_shape_type = Shape_Segment | Shape_Circle | Shape_Arc | Shape_Polygon

class pcb_shape = 
object
	val mutable m_stx = 0
	val mutable m_sty = 0
	val mutable m_enx = 0
	val mutable m_eny = 0
	val mutable m_width = 0
	val mutable m_layer = 0
	val mutable m_angle = 0
	val mutable m_polycount = 0
	val mutable m_polyX : 'int list = [] 
	val mutable m_polyY : 'int list = [] 
	val mutable m_type = Shape_Segment
	val mutable m_g = new grfx
	method update rot x y = (
		m_g#updateLayer false m_layer ; (* this also sets z, needed for vertices *)
		m_g#empty () ;
		(
		match m_type with
			| Shape_Polygon -> ( (*doesn't look like I have any, but write some sort of handler here ..*)
				m_g#empty () ; (*clear the list, all Grfx functions add to it.. *)
				m_g#makeTrackInt m_stx m_sty m_enx m_eny m_width ; 
				let p = List.combine m_polyX m_polyY in
				let rec drawpoly plist sx sy = 
					match plist with
					| [] -> () ; 
					| h::t -> (
						let (hx, hy) = h in
						m_g#makeTrackInt sx sy hx hy m_width;
						drawpoly t hx hy 
						); 
				in
				drawpoly p m_enx m_eny ; 
			)
			| _ -> m_g#makeTrackInt m_stx m_sty m_enx m_eny m_width ; 
		); 
		m_g#rotateTranslate rot x y ; 
	)
	method updateLayers () = m_g#updateLayer false m_layer ; 
	method draw bbox = (
		m_g#draw ~hit:false bbox
	)
	method read ic line shapetype = (
		let parse_startend = 
			let sp = Pcre.extract ~pat:"\w+ ([\d-]+) ([\d-]+) ([\d-]+) ([\d-]+)" line in
			m_stx <- ios sp.(1) ; 
			m_sty <- ios sp.(2) ; 
			m_enx <- ios sp.(3) ; 
			m_eny <- ios sp.(4) ; 
		in
		let parse_widthlayer = 
			let sp = Pcre.extract ~pat:"\w+ [\d-]+ [\d-]+ [\d-]+ [\d-]+ (\d+) (\d+)" line in
			m_width <- ios sp.(1) ; 
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
					let sp = Pcre.extract ~pat:"\w+ \d+ \d+ \d+ \d+ (\d+) (\d+) (\d+)" line in
					m_angle <- ios sp.(1) ; 
					m_width <- ios sp.(2) ; 
					m_layer <- ios sp.(3) ; 
				)
				| Shape_Polygon -> (
					parse_startend ;
					let sp = Pcre.extract ~pat:"\w+ \d+ \d+ \d+ \d+ (\d+) (\d+) (\d+)" line in
					m_polycount <- ios sp.(1) ; 
					m_width <- ios sp.(2) ; 
					m_layer <- ios sp.(3) ; 
					for i = 1 to m_polycount do
						let ll = input_line2 ic in
						let spp = Pcre.extract ~pat:"Dl (\d+) (\d+)" ll in
						m_polyX <- ( (ios spp.(1) ) :: m_polyX ) ; 
						m_polyY <- ( (ios spp.(2) ) :: m_polyY ) ; 
					done
				)
		) ; 
		m_type <- shapetype ; 
	)
	method save oc = (
		let save_startend oc typ = 
			fprintf oc "%s %d %d %d %d" typ m_stx m_sty m_enx m_eny ; 
		in
		let save_widthlayer oc = 
			fprintf oc " %d %d\n" m_width m_layer ; 
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
				List.iter2 (fun x y -> fprintf oc "Dl %d %d\n" x y;) m_polyX m_polyY ; 
			)
	)
end ;;