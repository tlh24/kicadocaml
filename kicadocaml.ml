(* Copyright 2008-2017, Timothy L Hanson *)
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
open Pcre
open Tk
open Comm
open Grfx
open Modtext
open Shape
open Pad
open Mod
open Track
open Zone 
open Ratnest
open Propagate
open Drc
open Schematic
open Doarray
open Netlist
open Cell

let nulfun _ = false ;;

module FDSet =
  Set.Make(struct type t = Unix.file_descr let compare = compare end)
  
class pcb_generic_section = 
(*generic storage of sections that we don't recognize, but want to keep in the file *)
object 
	val mutable m_type = ""
	val mutable m_lines : 'string list = [] 
	method getType() = m_type 
	method read ic line = 
	(
		m_type <- (Pcre.extract ~pat:"\$(\w+)" line).(1) ; 
		let line = ref (input_line2 ic) in
		let endpat = "\$End" ^ m_type in
(* 		print_endline ("pcb_generic section " ^ m_type);  *)
		(* note!  assumes generic sections do not have sub-sections ! *)
		(* some of the sections end with 'End', others with 'end', hence this must be 
		case-insensitive *)
		(* pcbnew occasionally seems to mess up the layer list - 
		just set it to 6 layers by default. *)
		glayerPresent := [0; 1; 2; 3; 4; 15]; 
		while not ( Pcre.pmatch ~rex:(Pcre.regexp ~flags:[`CASELESS] endpat) !line ) do
			m_lines <- ( !line :: m_lines ) ; 
			line := input_line2 ic ; 
			(* might as well try to understand some of these 'generic' sections.. *)
			(* right now, disable layers not in the file *)
			(* add more later... *)
			let tp,found = try (Pcre.extract ~pat:"^(\w+)" !line).(1), true
				with Not_found -> "", false
			in
			if found then (
				(* 
				if tp = "Layer" then (
					let l = (Pcre.extract ~pat:"(\d+)" !line).(1) in
					glayerPresent := ((ios l) :: !glayerPresent); 
					print_endline( "layer " ^ l ^ " present"); 
				); 
				*)
				if tp = "TrackClearence" || tp = "TrackClearance" then (
					let l = (Pcre.extract ~pat:"([\.\d]+)" !line).(1) in
					gclearance := foss l ; 
					print_endline ( "global track clearance = " ^ sof !gclearance ); 
				); 
				if tp = "Units" then (
					let l = (Pcre.extract ~pat:"^(\w+) (\w+)" !line).(2) in
					if l = "mm" then (
						printf "Internal units: mm\n%!"; 
						gscl := 25.4;
						gunit := "mm";
					)
				)
			); 
		done ; 
	)
	method save oc = (
		if (List.length m_lines) > 0 then (
			fprintf oc "$%s\n" m_type ; 
			List.iter (fun l -> fprintf oc "%s\n" l ; ) (List.rev m_lines); 
			fprintf oc "$End%s\n\n" m_type ; 
			(*these sections seem to be followed by two newlines.. *)
			flush oc ; 
		)
	)
end;;

let gnets = ref [] (*this must be a reference, as we are updating! *)
let gtracks = ref []
let gmodules = ref []
let gzones = ref []
let gcells = ref []
let ggeneric = ref []
let gzoom = ref 1.0
let gdrag = ref (0.0, 0.0)
let goldpan = ref (0.0, 0.0)
let ghithold = ref false (* don't update 'hit' when shift is down -- just use box *)
let genabledepth = ref true 
let file_header  = ref "" 
let gratsnest = new ratsNest
let gclosesock = ref (fun () -> print_endline "nothing" ; )
let gbutton3pressed = ref false
let gbutton1pressed = ref false
let gtrackwidth = ref 0.01 
let gtrackwidthlist = ref [] 
let gviasizelist = ref [] 
let ggridsizelist = ref []
let ggridorigin = ref (0.0, 0.0)
let gviapad = ref 0.047
let gviadrill = ref 0.015
let gfname = ref "" 
let groute135 = ref false
let gtrackKeepSlope = ref true
let gfilelist = ref [] 
let gdrawtracks = ref true
let gdrawmods = ref true
let gdrawzones = ref true
let gdrawratsnest = ref true
let gpushrouting = ref false
let gtrackDRC = ref true
let gcrossProbeTX = ref true
let gcrossProbeRX = ref true
let gTrackAdd = ref (fun _ -> ())
let gViaAdd = ref (fun _ _ -> ())
let gCellMenuRefresh = ref (fun _ -> ())
let gpulling = ref true 
let gcurrentcell = ref None

let readlines ic =
	(*remove the old modules *)
	gnets := [] ; 
	gmodules := [] ; 
	gtracks := [] ; 
	gcells := [] ;
	gzones := [] ; 
	ggeneric := [] ; 
	let gitline ic = 
		try
			input_line2 ic, false 
		with
			| End_of_file -> "", true
	in
	let readtracks ic = 
		let line = ref (input_line2 ic) in
		while not (Pcre.pmatch ~pat:"\$EndTRACK" !line) do 
		(
			let t = new pcb_track in
			t#read ic !line ; 
			gtracks := (t :: !gtracks) ; 
			line := input_line2 ic ; 
		)
		done ;
	in
	let readcells ic = 
		let line = ref (input_line2 ic) in
		while not (Pcre.pmatch ~pat:"\$EndCELL" !line) do 
		(
			let c = new pcb_cell in
			c#read ic !line ; 
			gcells := (c :: !gcells) ; 
			line := input_line2 ic ; 
		)
		done ;
	in
	(* read the header *)
	file_header := input_line2 ic ; 
	gfver := ios (try (Pcre.extract ~pat:"Version (\d+)" !file_header).(1)
		with _ -> "1"); 
	if !gfver = 2 then gscl := 1.0 ; 
	(* and the blank line after that *)
	let eof = ref false in
	while not !eof do (
		let line, eoff = gitline ic in
		let s, g = 
			try (Pcre.extract ~pat:"(\$\w+)" line).(1), true
			with Not_found -> "", false
		in
		if g then (
			match s with
				| "$EQUIPOT" -> ( 
					let net = new pcb_net in
					net#read ic; 
					gnets := (net :: !gnets) ; 
				)
				| "$MODULE" -> (
					let m = new pcb_module in
					m#read ic; 
					gmodules := m :: !gmodules; 
				)
				| "$DRAWSEGMENT" -> (
					let t = new pcb_track in
					t#read_drawsegment ic; 
					gtracks := (t :: !gtracks) ; 
				)
				| "$TRACK" -> (
					readtracks ic ; 
				)
				| "$CELL" -> (
					readcells ic ; 
				)
				| "$CZONE_OUTLINE" -> (
					let zon = new zone in
					zon#read ic ;
					gzones := zon ::!gzones;
				)
				| _ -> (
					let generic = new pcb_generic_section in
					try
						generic#read ic line ; 
						ggeneric := (generic :: !ggeneric); 
					with
						End_of_file -> ()
				)
		) ; 
		eof := eoff ; 
	)
	done; 
	close_in_noerr ic ; 
	print_endline ( "file version:" ^ (soi !gfver) ^ " scale:" ^ (string_of_float !gscl) ); 
	print_endline ( "number of nets:" ^ string_of_int(List.length !gnets) ) ; 
	print_endline ( "number of modules:" ^ string_of_int(List.length !gmodules) ) ; 
	print_endline ( "number of tracks:" ^ string_of_int(
		List.length (List.filter (fun t-> not(t#is_drawsegment())) !gtracks) )) ; 
	print_endline ( "number of drawsegments:" ^ string_of_int(
		List.length (List.filter (fun t-> t#is_drawsegment()) !gtracks) )) ; 
;;

let addToFilelist fil schem = 
	(* remove  any old entries with the same board name *)
	gfilelist := List.filter (fun a -> (fst a) <> fil) !gfilelist ; 
	gfilelist := ((fil,schem) :: !gfilelist); 
	;;

let saveall filename = 
	print_endline ("saving " ^ filename );
	let oc = open_out filename in
	fprintf oc "%s\n\n" !file_header ; 
	let sv q = q#save oc ; in
	let zones,others = List.partition (fun q -> Pcre.pmatch ~pat:"ZONE" (q#getType()))
		(List.rev !ggeneric) in 
	List.iter sv others;(*reverse b/c they are read in backwards. *)
	List.iter sv (List.rev !gnets ); 
	List.iter sv (List.rev !gmodules ); 
	let segments, tracks = List.partition (fun t-> t#is_drawsegment()) !gtracks in
	List.iter sv (List.rev segments ); 
	fprintf oc "$TRACK\n" ;
	List.iter sv (List.rev  tracks ); 
	fprintf oc "$EndTRACK\n" ;
	List.iter sv zones;
	List.iter sv (List.rev !gzones ); 
	fprintf oc "$EndBOARD\n" ;
	flush oc ; (* this is necessary!!! *)
	(* addToFilelist filename ; *)
	close_out_noerr oc ; 
	;;
	
let exportCIF filename = (* this is a somewhat experimental feature! *)
	print_endline ("saving " ^ filename );
	let oc = open_out filename in
	fprintf oc "DS 1 20 2;\n"; (* scale by 20/2 -- so we keep kicad's native resolution *)
	(* units: 1 mil = 1 micron. CIF works in centimicrons (10nm = 10-8m) *)
	let trans x = iof (x *. 10000.0) in (* CHECK THIS *)
	(* not all CIF programs support rounded wires -- for these, use polygons. *)
	let polywire layer = 
		List.iter (fun t -> 
		if t#getLayer () = layer then (
			let sx,sy = t#getStart () in
			let ex,ey = t#getEnd () in
			let width = t#getWidth () in
			(* this copied from grfx.ml -- we want something similar, but simpler as only need outline*)
			let dx = ex -. sx in
			let dy = ey -. sy in
			let n = 15 in
			let t = ref (pi /. -2.0) in
			let dt = pi /. foi(n) in
			if dx *. dx +. dy *. dy > 0.0 then (
				let len = sqrt(dx *. dx +. dy *. dy) /. (0.5 *. width) in
				let (nx, ny) = ( dx /. len, dy /. len) in (*line between them normalized to width.*)
				let (mx, my) = (-1. *. ny , nx) in (*rotate pi/2 ccw *)
				let pnt t x y = 
					( x -. cos(t)*.nx +. sin(t)*.mx, y -. cos(t)*.ny +. sin(t)*.my) in
				let endcap x y = 
					for i = 1 to n do (
						let vx,vy = pnt !t x y in
						fprintf oc " %d %d" (trans vx) (trans vy); 
						t := !t +. dt ; 
					)done; 
				in
				fprintf oc "P"; 
				endcap sx sy; 
				endcap ex ey;
				fprintf oc ";\n"; 
			) else (
				(* draw a circle then.*)
				let pnt t = 
					(sx +. 0.5*.width*.cos(t), sy +. 0.5*.width*.sin(t)) in
				fprintf oc "P"; 
				for i = 1 to 2*n do (
					let vx,vy = pnt !t in
					fprintf oc " %d %d" (trans vx) (trans vy); 
					t := !t +. dt ; 
				)done; 
				fprintf oc ";\n"; 
			)
		)
	) !gtracks; 	
	in
	(* first layer: , parylene cut-through.*)
	fprintf oc "L Via1;\n"; 
	(* iterate through the drawings layer, plotting each *)
	polywire 24; 
	(* next the metal layer *)
	fprintf oc "L Metal;\n"; 
	polywire 15; 
	(* iterate through the module(s) too -- represent the pads as blocks. *)
	List.iter (fun m ->
		List.iter (fun p ->
			if p#getShape () = Pad_Rect then (
				let lx,ly,ux,uy = p#getBBX () in
				let cx = 0.5 *. (lx +. ux) in
				let cy = 0.5 *. (ly +. uy) in
				let w = ux -. lx in
				let h = uy -. ly in
				fprintf oc "B %i %i %i %i;\n"
					(trans w) (trans h) (trans cx) (trans cy) ; 
			)
		) (m#getPads ())
	) !gmodules; 
	(* now the upper via layer -- cut through the top parylene *)
	fprintf oc "L Via2;\n"; 
	polywire 24; 
	polywire 23; (* solder mask_top *)
	fprintf oc "DF;\n";
	fprintf oc "C 1;\n";
	fprintf oc "End\n"; 
	flush oc ; (* this is necessary!!! *)
	(* addToFilelist filename ; *)
	close_out_noerr oc ; 
	;;
	
let exportGerber filename = (* testing, testing. *)
	(* make an aperture list, w/ pads. *)
	let apertures = Hashtbl.create 100 in
	let cnt = ref 10 in
	List.iter (fun t->
		let width = t#getWidth () in
		if not (Hashtbl.mem apertures (width, 0.0)) then (
			Hashtbl.add apertures (width, 0.0) !cnt; 
			cnt := !cnt + 1; 
		)
	) !gtracks; 
	(* iterate over pads, too *)
	List.iter (fun m->
		List.iter (fun p->
			let width = p#getSx() in
			let height = p#getSy() in
			if p#getShape() = Pad_Rect then (
				if not (Hashtbl.mem apertures (width, height)) then (
					Hashtbl.add apertures (width, height) !cnt; 
					cnt := !cnt + 1; 
				)
			);
			if p#getShape() = Pad_Circle || p#getShape() = Pad_Oval then (
				let l = if width > height then width else height in
				if not (Hashtbl.mem apertures (l, 0.0)) then (
					Hashtbl.add apertures (l, 0.0) !cnt; 
					cnt := !cnt + 1; 
				)
			)
		) (m#getPads())
	) !gmodules; 
	
	let saveGerberFile fnm layers nopanellay pann affinefun = 
		print_endline ("saving " ^ fnm );
		let oc = open_out fnm in
		let pc = "%" in
		fprintf oc "G04 (created by kicadocaml v 112)*\n"; 
		fprintf oc "G01*\nG70*\nG90*\n" ; 
		fprintf oc "%sMOIN*%s\n" pc pc;
		fprintf oc "G04 Gerber Fmt 3.4, Leading zero omitted, Abs format*\n";
		fprintf oc "%sFSLAX34Y34*%s\n" pc pc; 
		(* these won't be in order .. meh. *)
		fprintf oc "G04 APERTURE LIST*\n"; 
		Hashtbl.iter (fun k v -> 
			let (w,h) = k in
			if (h = 0.0) then (
				fprintf oc "%sADD%dC,%1.6f*%s\n" pc v w pc
			) else (
				fprintf oc "%sADD%dR,%1.6fX%1.6f*%s\n" pc v w h pc
			)
		) apertures; 
		(* now iterate over the tracks / modules, flashing the apertures. *)
		let cnvt d = round (d *. 10000.0) in
		let gerbPrint x y flashcode = 
			let six = if x < 0.0 then "-" else "" in
			let siy = if y > 0.0 then "-" else "" in (*flip vertical axis .. not sure why.*)
			fprintf oc "X%s%06dY%s%06dD%s*\n" 
				six (cnvt (fabs x)) siy (cnvt (fabs y)) flashcode;
		in
		let gerbTrack (sx,sy) (ex,ey) = 
			gerbPrint sx sy "02"; 
			gerbPrint ex ey "01"; 
		in
		let gerbTrackPanel start fin = 
			(* this also pannelizes! *)
			for i=0 to pann-1 do (
				gerbTrack (affinefun i start) (affinefun i fin)
			)done
		in
		let gerbPrintPanel x y flashcode = 
			for i=0 to pann-1 do (
				let ox,oy = affinefun i (x,y) in
				gerbPrint ox oy flashcode
			)done
		in
		Hashtbl.iter (fun k v -> 
			let (w,h) = k in
			fprintf oc "G54D%d*\n" v ; 
			if h = 0.0 then (
				List.iter (fun t->
					if (List.mem (t#getLayer()) layers) && t#getWidth() = w then ( 
						gerbTrackPanel (t#getStart()) (t#getEnd()); 
					) ;
					if (List.mem (t#getLayer()) nopanellay) && t#getWidth() = w then (
						gerbTrack (t#getStart()) (t#getEnd()); 
					) ; 
				) !gtracks; 
			) ; 
			List.iter (fun m ->
				List.iter (fun p -> 
					if List.exists (fun l -> p#hasLayer l) layers then(
						let width = p#getSx() in
						let height = p#getSy() in
						if p#getShape() = Pad_Rect then (
							if width = w && height = h then (
								let sx,sy = p#getCenter() in
								gerbPrintPanel sx sy "03";
							)
						);
						if p#getShape() = Pad_Circle then (
							if width = w && h = 0.0 then (
								let sx,sy = p#getCenter() in
								gerbPrintPanel sx sy "02";
							)
						);
						if p#getShape() = Pad_Oval then (
							if width = w && height = h then (
								let sx,sy = p#getCenter() in
								if width > height then (
									let w2 = (w -. h) *. 0.5 in
									gerbTrackPanel ((sx -. w2), sy) ((sx +. w2), sy)
								) else (
									let h2 = (h -. w) *. 0.5 in
									gerbTrackPanel (sx, (sy -. h2)) (sx, (sy -. h2)) 
								)
							)
						)
					)
				) (m#getPads())
			) !gmodules 
		) apertures ; 
		fprintf oc "M02*\n";
		flush oc ; (* this is necessary!!! *)
		(* addToFilelist filename ; *)
		close_out_noerr oc ; 
	in
	(* only layers that have tracks on them *)
	let rootname = if Pcre.pmatch ~pat:"[^\.]+\.gbr" filename then 
		(Pcre.extract ~pat:"([^\.]+)\.gbr" filename).(1) 
		else filename in
	(*for lay = 0 to 24 do (
		if List.exists (fun t-> t#getLayer() = lay) !gtracks then (
			let fnm = (rootname ^ "_" ^ (layer_to_string lay) ^ ".gbr") in
			let fnm2 = (rootname ^ "_array_" ^ (layer_to_string lay) ^ ".gbr") in
			(* saveGerberFile fnm2 lay 6.75 29.45 10 2 ;*)
			saveGerberFile fnm lay 0.0 0.0 1 1
		)
	) done*)
	(* custom setup.. *)
	let pannelizeFun pann (x,y) = 
		let n = foi pann in
		let rotated yoffs nn = 
			if nn >= 0.0 && nn < 2.0 then (
				(* rotate 90, move *)
				let ox =  1.0 *. y in
				let oy = -1.0 *. x -. nn *. 10.4 +. yoffs in
				(ox,oy)
			)
			else if nn >= 2.0 && nn < 4.0 then (
				(* rotate 270, move *)
				let ox = -1.0 *. y  in
				let oy =  1.0 *. x -. (nn -. 1.0)*. 10.4 -. 0.2 +. yoffs in
				(ox,oy)
			) else (-100.0, -100.0)
		in
		if pann < 10 then (
			let ox = x +. n *. 10.4 -. (10.4 *. 5.0) in
			let oy = y in
			(ox,oy)
		)
		else if pann >= 10 && pann < 20 then (
			(* rotate 180, move *)
			let ox =  -1.0 *. x +. (n -. 10.0)*. 10.4 -. (10.4 *. 4.0) +. 0.2 in
			let oy = -1.0 *. y in
			(ox,oy)
		)
		else if pann >= 20 && pann < 24 then (
			rotated (-37.5) (n -. 20.0) )
		else if pann >= 24 && pann < 28 then ( 
			rotated (37.5 +. 10.4 *. 2.0 +. 0.2) (n -. 24.0 ) )
		else (-100.0, -100.0)
	in
	let npan = 28 in
	saveGerberFile (rootname ^ "_metal_INVERT.gbr") [15] [23] npan pannelizeFun;
	saveGerberFile (rootname ^ "_outline.gbr") [24] [] npan pannelizeFun; 
	saveGerberFile (rootname ^ "_TiEtch.gbr") [1] [23] npan pannelizeFun;
	saveGerberFile (rootname ^ "_copper.gbr") [22] [23] npan pannelizeFun;
	saveGerberFile (rootname ^ "_protect.gbr") [4] [] npan pannelizeFun;
	saveGerberFile (rootname ^ "_parylene_INVERT.gbr") [0] [23] npan pannelizeFun;
	saveGerberFile (rootname ^ "_wafer.gbr") [3] [23] 1 (fun _ x -> x); 
	;;
	
(* UI stuff *)
let abouttext =
"Kicad PCB editor\n" ^
"written in Ocaml (yay) \n" ^
"by Tim Hanson, sideskate@gmail.com \n" ^
"for use with the Kicad suite of programs.\n" ^
"   (not meant to work wholly by itself -- lib load incomplete) \n\n" ^
"Relevant hotkeys / commands: \n" ^ 
" right mouse button --- pan \n" ^ 
" scroll wheel --- zoom \n" ^
" middle mouse button --- \n" ^
"    * in move module & move text mode, rotate; \n" ^
"    * in add or move track mode, select layer and update width\n"^
"       (based on the track currently under the cursor).\n"^
" .... \n" ^
" a - add track mode\n" ^
" Ctrl-T - select track width \n" ^ 
" b - break track under cursor \n" ^
" e - edit (only text edit mplemented srry)\n" ^
" f - In track editing/adding mode:\n\tfuse (join) tracks - two tracks must be highlighted \n" ^
"     In module moving mode:\n\tflip a module from the top to the bottom of the board, and vice-versa\n"^
" h - hide / unhide selected text in move text mode\n" ^
" L - Switch (shift-selected) tracks to current layer\n" ^ 
" m - move module \n" ^
" o - set origin of grid to current snap. \n" ^
" O - reset origin of grid to 0,0. \n" ^
" <space> - toggle between add and move track modes\n" ^
" t - move track mode\n" ^
" v - insert a via (in add tracks mode) \n" ^
" v - edit module value (in move module mode)\n"^
" Ctrl-V - select via size \n" ^
" w - change width of highlited wire\n" ^
" X - mirror along x axis\n" ^
" Y - mirror along y axis\n" ^
" .... \n" ^
" page up - select copper layer \n" ^
" page down - select component layer \n" ^
" F5 - select inner layer 1\n" ^
" F6 - select inner layer 2\n" ^
" F7 - select inner layer 3\n" ^
" F8 - select inner layer 4 (but you should use the middle mouse button usually)\n"  ^
" .... \n" ^
" Shift - select multiple modules for moving \n" ^
"         (somewhat incomplete feature) \n" ^
" Enter - cross probe transmit to eeschema\n" ^
" Backspace - remove highlighted track\n" ^
" Delete - remove highlighted track & all connected tracks\n" ^
" Escape - Unstick the mouse (workaround for Tk error)\n" ^
" Ctrl-F - find \n" ^
" F3 - find next \n"

let helpbox name text top () =
	let helptop = Toplevel.create ~name top in
	Wm.title_set helptop name;
	let mesg = Message.create ~text helptop in
	pack [mesg]

let render togl cb =
	Togl.make_current togl ; 
	let w, h = !gwindowsize in
	let ar = (foi w)/.(foi h) in 
	GlClear.color (0.0, 0.0, 0.0) ~alpha:0.0;
	GlClear.clear [`color; `depth];
	if !genabledepth then ( 
		Gl.enable `depth_test ;
	) else (
		Gl.disable `depth_test ;
	) ; 
	Gl.enable `blend ; 
	GlMat.push() ; 
	GlMat.scale ~x:!gzoom ~y:(-1. *. !gzoom) ~z:(1.0) () ;  
	GlMat.translate ~x:(fst !gpan) ~y:(snd !gpan) ~z:(0.0) (); 
	let sx,sy = Pts2.scl !gpan (-1.)in
	let w = ar /. !gzoom in
	let h = 1. /. !gzoom in
	let screenbbx = (sx -. w, sy -. h, sx +. w, sy +. h) in
	
	let alpha = ref 0.2 in
	if !ggridDraw then (
		Array.iteri (fun i g -> 
			GlMat.push() ; 
			GlMat.translate ~x:(0.0) ~y:(0.0) ~z:((foi i) /. 500.0) (); 
			Grid2.draw screenbbx g !ggridorigin !alpha; 
			if !genabledepth then alpha := !alpha +. 0.04; 
			GlMat.pop() ; 
		) ggrid ;
		(* if the depth buffer is disabled we don't need to change the alpha level. *)
	) ; 
	!ginfodisp( "" ) ;
	let more = cb () in
	if not more then (
		(* draw the tracks back to front so that the alpha-blending makes sense *)
		(* this requires slightly more iteration but.. eh well, it is the correct way to do it*)
		let layerZlist = List.filter (fun a -> glayerEn.(a)) (List.rev !glayerZlist) in
		let lastLayer = try List.hd (List.rev layerZlist) with _ -> 0 in
		List.iter ( fun lay -> 
			let lastiter = (lay = lastLayer) in
			let z = glayerZ.(lay) in
			GlMat.push () ; 
			GlMat.translate ~x:0. ~y:0. ~z () ; 
			(* draw the modules - they handle z-sorting internally *)
			if !gdrawmods then (
				List.iter (fun m -> 
					m#draw screenbbx z
				) !gmodules ; 
			);
			(* draw the tracks.. *)
			if !gdrawtracks then (
				List.iter (fun m -> 
					if m#isVia() then (
						if lastiter then m#draw screenbbx; 
					) else if m#getLayer() = lay then (
						m#draw screenbbx 
					); 
				) !gtracks ; 
			); 
			(* do the same on the zones. *)
			if !gdrawzones then (
				List.iter (fun zon -> 
					if zon#getLayer() = lay then (
						zon#draw screenbbx ; 
					); 
				) !gzones ;
			) ; 
			GlMat.pop () ; 
		) layerZlist ; 
		
		if !gdrawratsnest then gratsnest#draw ();
		(* it is useful to see where the cursor is .. *)
		let h = foi(snd !gwindowsize) in
		let s = (2. /. h) /. !gzoom in
		
		let vertex3 raw c (x,y,z) = 
			Raw.set_float raw ~pos:(!c*3 + 0) x ; 
			Raw.set_float raw ~pos:(!c*3 + 1) y ; 
			Raw.set_float raw ~pos:(!c*3 + 2) z ;
			incr c ; 
		in
		let drawCrosshairs (x, y) = 
			let c = ref 0 in
			let raw = Raw.create_static `float 24 in 
			GlArray.vertex `three raw ; 
			let ss = s /. 1.0 in
			let gz = 4.0 /. !gzoom in
			let z = 1. in
			vertex3 raw c ( x       , y +. gz, z) ; 
			vertex3 raw c ( x +. ss , y +. gz, z) ; 
			vertex3 raw c ( x +. ss , y -. gz , z) ; 
			vertex3 raw c ( x       , y -. gz , z) ; 
			vertex3 raw c ( x -. gz , y +. ss, z) ; 
			vertex3 raw c ( x +. gz , y +. ss, z) ; 
			vertex3 raw c ( x +. gz , y      , z) ; 
			vertex3 raw c ( x -. gz , y      , z) ; 
			GlArray.draw_arrays `quads 0 8 ; 
			Raw.free_static raw ; 
		in
		(*let drawRing (x,y) radius1 radius2 = 
			let c = ref 0 in
			let n = 8 in
			let raw = Raw.create_static `float (n*2*4*3) in 
			GlArray.vertex `three raw ; 
			let (fid, fod) = radius1 /. !gzoom, radius2 /. !gzoom in
			let t = ref 0. in
			let dt = pi /. (foi n) in
			let pnt d a = 
				vertex3 raw c ((x -. d *. cos(a)), (y +. d *. sin(a)), 0.99) in
			for i = 1 to 2*n do (
				pnt fid !t;  
				pnt fid (!t +. dt);  
				pnt fod (!t +. dt);  
				pnt fod !t;
				t := !t +. dt ; 
			) done ;
			GlArray.draw_arrays `quads 0 (n*2*4) ; 
			Raw.free_static raw ; 
		in *)
		let drawCircle (x,y) radius = 
			(* for convienence, will make this drawable with quads not triangles. *)
			let fw,fh = radius/.2.0, radius/.2.0 in
			let n = 10 in
			let c = ref 0 in
			let raw = Raw.create_static `float (n*4*3) in 
			GlArray.vertex `three raw ; 
			let t = ref (pi /. foi((n+1) * 2)) in
			let dt = pi /. foi(n+1) in
			let cos_ t = cos(t) *. fw in
			let sin_ t = sin(t) *. fh in
			let pnt a = 
				vertex3 raw c a in
			let z = 0.99 in
			for i = 1 to n do (
				pnt (x -. cos_(!t), y -. sin_(!t), z) ; 
				pnt (x -. cos_(!t+.dt), y -. sin_(!t+.dt), z) ; 
				pnt (x -. cos_(!t+.dt), y +. sin_(!t+.dt), z) ; 
				pnt (x -. cos_(!t), y +. sin_(!t), z) ; 
				t := !t +. dt ; 
			) done; 
			GlArray.draw_arrays `quads 0 (n*4) ; 
			Raw.free_static raw ; 
		in
		let drawRect (xl,yl,xh,yh) = 
			let count = ref 0 in
			let raw = Raw.create_static `float 12 in
			GlArray.vertex `three raw ; 
			let vertex3 (x,y,z) = 
				Raw.set_float raw ~pos:(!count*3 + 0) x ; 
				Raw.set_float raw ~pos:(!count*3 + 1) y ; 
				Raw.set_float raw ~pos:(!count*3 + 2) z ;
				incr count ; 
			in
			vertex3 ( xl , yl, 0.5) ; 
			vertex3 ( xl , yh, 0.5) ; 
			vertex3 ( xh , yh, 0.5) ; 
			vertex3 ( xh , yl, 0.5) ; 
			GlArray.draw_arrays `quads 0 4 ; 
			Raw.free_static raw ; 
		in
		if !gmode = Mode_AddTrack then (
			(* draw the layer we'll add a track on, scaled appropriately *)
			GlDraw.color ~alpha: 0.25 (layer_to_color !glayer); 
			drawCircle !gcurspos !gtrackwidth; 
		); 
		GlDraw.color ~alpha:0.7 (match !gmode with
			| Mode_AddTrack -> (1.,0.80,0.22) (* orange *)
			| Mode_MoveTrack -> (1.,0., 0.2) (* red *)
			| Mode_MoveModule -> (0.55,0.16,1.) (* purple-blue *)
			| Mode_MoveText -> (0.22,0.77,1.) (* aqua *)
		); 
		drawCrosshairs !gcurspos ; 
		GlDraw.color ~alpha:0.5 (0.4 , 1., 0.8 ); 
		drawCrosshairs !gsnapped ; 
		
		(* draw the selection box *)
		if bbxIntersect !gselectRect screenbbx then (
			GlDraw.color ~alpha:0.2 (0.8 , 0.4, 1.0 ); 
			drawRect !gselectRect ;
		) ; 
	);

	GlMat.pop() ; 
	Gl.flush ();
	Togl.swap_buffers togl ; 
(*  	Printexc.print_backtrace stdout (* ocaml 3.11 *)  *)
  ;;

let redoRatNest () =
	print_endline "redoing ratsnest" ; 
	gratsnest#make (List.filter (fun m-> m#getVisible ()) !gmodules) !gtracks ;
	;; 
	
let testfile file = 
	try 
		let ic = open_in file in 
		close_in  ic; 
		true
	with Sys_error _ -> false
	;;

let selectSch top fname = 
	(* select the schematic file associated with this board *)
	(* first see if we need to pop open a file selection dialog. *)
	(* printf "selectSch called with:%s\\s\n%!" fname ; 
	List.iter (fun (fname, sname) -> 
		printf "fname:%s\\s sname:%s\\s\n%!" fname sname ; 
	) !gfilelist; *)
	let selectSchFile () = 
		(* pop open a dialog suggesting what needs to happen *)
		let dlog = Toplevel.create top in
		Wm.title_set dlog "Specify assoc. schem. ";
		let str = 	"Please specify the associated schematic file " ^
				"(if the schematics are in a heirarchy, select the root). " ^
				"This is needed to determine the schematic hierarchy, and where " ^ 
				"each of the components are in case you should want to duplicate " ^ 
				"a segment of the layout \n" ^
				"If you don't have a schematic, just click 'cancel' in the file selector." in
		let msg = Message.create ~text:str  dlog in
		let but = Button.create ~text:"Ok" ~command:(fun () -> Tk.destroy dlog) dlog in
		Tk.pack ~side:`Top ~fill:`Y ~expand:true ~padx:3 ~pady:3 
			[Tk.coe msg; Tk.coe but]; 
		let filetypsch = [ {typename="schematics";extensions=[".sch"];mactypes=[]} ] in
		let schfile = Tk.getOpenFile ~defaultextension:".sch" 
			~filetypes:filetypsch ~title:"open schematic" () in
		printf "schematic file: %s\n%!" schfile ; 
		addToFilelist fname schfile; 
		schfile
	in
	
	try 
		let schfile = snd (List.find (fun (k,_) -> k = fname) !gfilelist) in
		let ok = testfile schfile in
		if not ok then selectSchFile() else schfile 
	with Not_found -> selectSchFile ()
	;;

let updateLayers layer b = 
	if glayerEn.(layer) != b  || b then (
		glayerEn.(layer) <- b ; 
		(* need to recreate the via color *)
		let copperlayers = List.filter ((>) 16) !glayerPresent in 
		(* the > seems backwards to me .. ehwell. *)
		gviaColor := layers_to_color copperlayers glayerEn ; 
		List.iter (fun m -> m#updateLayers() ) !gmodules ;
		List.iter (fun m -> m#updateLayers() ) !gzones ; 
		List.iter (fun m -> 
			m#setHit false; 
			if m#isVia() then m#updateColor () ; 
		) !gtracks ;
		(* render togl nulfun; *)
	) ;;
	
let getPrefsFile () = try "/home/" ^ (Unix.getlogin ()) ^ "/.kicadocaml" 
	with _ ->  (Unix.getcwd()) ^ "/.kicadocaml" 
	;;
	

(* open a file *)
let openFile top fname = 
	let ic = open_in fname in
	glayerPresent := [] ; (* clear the old layer list, this file may have different layers present *)
	readlines ic ; 
	(* also add the other known layers -- see kicad!*)
	glayerPresent := [20;21;22;23;24] @ !glayerPresent; 
	(* update the enabled list ... *)
	for i = 0 to 31 do glayerEn.(i) <- false done;
	List.iter (fun u -> glayerEn.(u) <- true) !glayerPresent;  
	gfname := fname ; (* if we get this far, then we can assign it *)
	(* addToFilelist fname ; *)
	Wm.title_set top ("Kicad Ocaml " ^ fname );
	List.iter (fun m -> m#update()) !gmodules ; 
	List.iter (fun m -> m#update()) !gtracks ; 
	List.iter (fun z -> z#update ()) !gzones ; 
	updateLayers 15 true ; (* default to component layer *)
	(* if the board was saved in pcbnew, then we need to propagate the netcodes to all tracks. *)
	(* otherwise, adding tracks to existing ones becomes impossible ... *)
	let trackzero = List.length (List.filter (fun t -> (t#getNet ()) <= 0) !gtracks) in
	let tracklen = List.length !gtracks in
	printf "number of read tracks: %d; number of tracks with net of zero %d \n%!" tracklen trackzero ; 
	
	if (foi trackzero) /. (foi tracklen) > 0.4 then (
		printf "It seems that this board was previously saved in PCBnew...\n%!"; 
		propagateNetcodes2 gmodules gtracks true false top (fun () -> ()); 
	); 
	gratsnest#clearAll (); 
	gratsnest#make !gmodules !gtracks; 
	(* figure out the (approximate) center of the board *)
	if (List.length !gmodules) > 0 then (
		let center = bbxCenter (List.fold_left 
			(fun b m -> bbxMerge b (m#getBBX false )) 
			((List.hd !gmodules)#getBBX false) !gmodules) in
		gpan := Pts2.scl center (-1.0) ; 
	) ; 
	(* make a set of all the track widths in this board *)
	let trackWidth = ref (SI.empty) in
	let round x = int_of_float (floor (x +. 0.5)) in
	List.iter (fun t-> 
		if t#getType () = Track_Track then (
			trackWidth := SI.add (round((t#getWidth())*.1000.0)) !trackWidth ; 
		)
	) !gtracks; 
	(* convert them to floating-point *)
	SI.iter (fun tw -> !gTrackAdd ((foi tw) /. 1000.0) ) !trackWidth; 

	(* do the same for the vias. *)
	let viaSize = ref (SI2.empty) in
	List.iter (fun t-> 
		if t#getType () = Track_Via then (
			viaSize := SI2.add (
				(round((t#getWidth())*.1000.0)),
				(round((t#getDrill())*.1000.0))
				) !viaSize ; 
		)
	) !gtracks; 
	SI2.iter (fun tw -> !gViaAdd ((foi (fst tw)) /. 1000.0) ((foi (snd tw)) /. 1000.0) ) !viaSize; 
	
	(* and do the same for the cells. *)
	!gCellMenuRefresh (); 
	
	linenum := 0 ; 
	let schfil = selectSch top fname in
	if testfile schfil then (
		printf "reading schematic ... %!"; 
		gschema#openFile schfil "00000000" "root" (ref []); 
		gschema#collapseAr ""; 
		printf " done.\n%!"; 
	) ;
	;;

let makemenu top togl filelist = 
	(* create menu bar *)
	let menubar = Frame.create ~borderwidth:0 ~relief:`Raised top in
	let fileb = Menubutton.create ~text:"File" menubar
	and optionb = Menubutton.create ~text:"Options" menubar
	and viab = Menubutton.create ~text:"Via" menubar 
	and trackb = Menubutton.create ~text:"Track" menubar 
	and gridb = Menubutton.create ~text:"Grid" menubar 
	and cellb = Menubutton.create ~text:"Cells" menubar in
	let filemenu = Menu.create ~tearoff:false fileb
	and optionmenu = Menu.create ~tearoff:false optionb
	and viamenu = Menu.create ~tearoff:false viab 
	and trackmenu = Menu.create ~tearoff:false trackb
	and gridmenu = Menu.create ~tearoff:false gridb 
	and cellmenu = Menu.create ~tearoff:false cellb in
	let infodisp = Text.create ~width:45 ~height:2 menubar in
	let cursorbox = Text.create ~width:17 ~height:2 menubar in
	let snapbox = Text.create ~width:17 ~height:2 menubar in
	let originbox = Text.create ~width:17 ~height:2 menubar in
	(* information display callback *)
	ginfodisp := ( fun s ->  
		Text.delete ~start:(`Linechar (1, 1) , [`Linestart]) ~stop:(`End, []) (infodisp) ; 
		Text.insert  ~index:(`End, []) ~text:s infodisp;
	 ); 
	 ginfodispappend := ( fun s ->  
		Text.insert  ~index:(`End, []) ~text:s infodisp;
	 ); 
	let coorddisp txtbx str x y =
		let s = Printf.sprintf "%s x %2.4f\n%s y %2.4f" str x str y in
		Text.delete ~start:(`Linechar (1, 1) , [`Linestart]) ~stop:(`End, []) (txtbx) ; 
		Text.insert  ~index:(`End, []) ~text:s txtbx; 
	in
	gcursordisp := (fun str x y -> coorddisp cursorbox str x y; 
		coorddisp snapbox "snap" (fst !gsnapped) (snd !gsnapped); 
		coorddisp originbox "origin" (fst !ggridorigin) (snd !ggridorigin)); 
		
	(*array callback ... *)
	let arrayFun () = 
		let dlog = Toplevel.create top in
		Wm.title_set dlog "Array components and tracks";
		let str = 	"Enter the sheet names involved in array, " ^ 
						"with the following template: sheet{1..1..8}. \n" ^ 
						"sheet1, sheet2 etc. are the sheet names in eeshema" ^
						"with {x..y..z} where x = start sheet, y = increment, z = last sheet" in
		let msg = Message.create ~text:str  dlog in
		let sheets = Entry.create ~width:20 dlog in
		Entry.insert ~index:(`Num 0) ~text:"stim{1..1..8}" sheets ; 
		let msg2 = Message.create  ~text:"Enter the sheet that has already been layed out, e.g. sheet1" dlog in
		let template = Entry.create ~width:20 dlog in
		Entry.insert ~index:(`Num 0) ~text:"stim1" template ;
		let msg3 = Message.create ~text:"Enter array x spacing, inches" dlog in
		let xentry = Entry.create dlog in
		Entry.insert ~index:(`Num 0) ~text:"0" xentry ;
		let msg4 = Message.create ~text:"Enter array y spacing, inches" dlog in
		let yentry = Entry.create dlog in
		Entry.insert ~index:(`Num 0) ~text:"1" yentry ;
		let arrayCmd () = 
			doArray (Entry.get sheets) (Entry.get template) 
				(fos(Entry.get xentry)) (fos(Entry.get yentry)) 
				gmodules gtracks (fun () -> render togl nulfun) redoRatNest top
		in
		let but = Button.create ~text:"Array!" ~command:arrayCmd dlog in
		Tk.pack ~side:`Top ~fill:`Y ~expand:true ~padx:3 ~pady:3 
			[Tk.coe msg; Tk.coe sheets; Tk.coe msg2; Tk.coe template ; 
			Tk.coe msg3; Tk.coe xentry; Tk.coe msg4; Tk.coe yentry; Tk.coe but]; 
	in
		
	(* adjust via drill sizes *)
	let viaDrillAdjust () = 
		let dlog = Toplevel.create top in
		Wm.title_set dlog "Change via drill size";
		let msg = Message.create ~text:"vias with a diameter >" ~width:100 dlog in
		let min = Entry.create ~width:12 dlog in
		let msg2 = Message.create ~text:"and < "  dlog in
		let max = Entry.create ~width:12 dlog in
		let msg3 = Message.create ~text:" set to drill size "  dlog in
		let drill = Entry.create ~width:12 dlog in
		let button = Button.create ~text:"Apply!"
		~command:(fun () ->
			let minw = fos (Entry.get min) in
			let maxw = fos (Entry.get max) in
			let drillw = fos (Entry.get drill) in
			List.iter (fun t-> 
				if t#isVia() then (
					let w = t#getWidth() in
					if minw < w && w < maxw then (
						t#setDrill drillw ; 
						t#update () ; 
					)
				); 
			) !gtracks; 
			render togl nulfun; (*show the user the results!*)
		)dlog in
		Tk.pack  ~fill:`X ~side:`Left 
			[Tk.coe msg; Tk.coe min; Tk.coe msg2; Tk.coe max; Tk.coe msg3; Tk.coe drill; Tk.coe button]; 
	in

	let entryframe dlog (txt,default) = 
		let frame1 = Frame.create dlog in
		let msg = Message.create ~text:txt ~width:110 frame1 in
		let ntry = Entry.create ~width:12 frame1 in
		Entry.insert ~index:(`Num 0) ~text:default ntry ; 
		Tk.pack ~fill:`X ~expand:true ~side:`Left [Tk.coe msg; Tk.coe ntry]; 
		(frame1, fun () -> Entry.get ntry )
	in
	let msgframe dlog text = 
		let frame1 = Frame.create dlog in
		let msg = Message.create ~text frame1 in
		Tk.pack ~fill:`Both ~expand:true [msg] ; 
		frame1 
	in
	(* adjust text position / rotations on modules following a template *)
	let textPositionAdjust () = 
		let dlog = Toplevel.create top in
		Wm.title_set dlog "Change text position";
		let frame1  = msgframe dlog 
			"Change text position, size, and orientation based on a template module" in
		let exemplarFrm, exemplarCb = entryframe dlog ("Template module (e.g. Q1)","") in
		let frame2 = Frame.create dlog in
		let button = Button.create ~text:"set!" 
		~command:(fun () -> 
			let exemplar = exemplarCb () in
			(* find the exemplar *)
			let emod , fnd= try
				(List.find (fun m -> m#getRef() = exemplar) !gmodules), true
			with Not_found ->(List.hd !gmodules), false in
			if fnd then (
				let libref = emod#getLibRef () in
				let others = List.filter (fun m -> 
					m#getRef() != exemplar && 
					m#getLibRef() = libref
				) !gmodules in
				List.iter (fun etxt -> (* iterate over all exemplar texts *)
					let typ = etxt#getType () in
					List.iter (fun m -> (* iter over all similar modules *)
						let txts = List.filter (fun t -> 
							t#getType() = typ
						) (m#getTexts ()) in
						List.iter (fun t -> 
							t#setSize (etxt#getSize ()) ; 
							t#setPos (etxt#getPos ()) ; 
							t#setRot (etxt#getRot ()) ; 
							t#setShow (etxt#getShow ()) ; 
							t#setWidth (etxt#getWidth ()) ; 
							(* everything but the actual content :-) *)
							t#update2 () ; 
							render togl nulfun ; 
						) txts ; 
					) others ; 
				) (emod#getTexts ()) ; 
			) else (
				printf "module with reference %s not found!\n%!" exemplar ; 
			)
		) frame2 in
		Tk.pack ~side:`Left~fill:`X [button] ; 
		let all = [frame1; frame2 ; exemplarFrm] in
		Tk.pack ~fill:`Both ~expand:true all ; 
	in
	(* adjust text sizes on specific modules *)
	let textSizeAdjust () = 
		let dlog = Toplevel.create top in
		Wm.title_set dlog "Change text sizes on select modules";
		let frame1  = msgframe dlog "Change text sizes on select modules" in
		let msgs = [|"module name (footprint)";"text width (x)";"text height (y)";
			"text line thickness";"Limit to modules on the same sheet & sub-sheets as:"^
			"(eg. R21; leave blank for all modules)"|] in
		let msgs2 = Array.map (fun a -> (a, "")) msgs in
		let framelist = Array.map (entryframe dlog) msgs2 in
		let frames = Array.to_list (Array.map fst framelist) in
		let cbs = Array.map snd framelist in
		(* radiobuttons for show/hide *)
		let showframe = Frame.create dlog in
		let showvar = Textvariable.create ~on:showframe () in
		Textvariable.set showvar "show";
		let show = ref true in
		let showcallback () = 
			show := ((Textvariable.get showvar)="show") ; 
		in
		let mkradio label = 
			Radiobutton.create ~indicatoron:true ~text:label 
			~value:label ~variable:showvar 
			~command:showcallback 
			showframe
		in
		let showb = mkradio "show" in
		let hideb = mkradio "hide" in
		Tk.pack ~side:`Left~fill:`X [showb ; hideb] ; 
		(* the apply button *)
		let frame2 = Frame.create dlog in
		let button = Button.create ~text:"set!" 
		~command:(fun () -> 
			let modname = (cbs.(0))() in
			let width = fos ((cbs.(1))()) in
			let height = fos ((cbs.(2))()) in
			let thickness = fos ((cbs.(3))()) in
			let onsheet = (cbs.(4))() in
			let ar = if not (onsheet = "") then ( (* the alternate reference - aka path *)
				let mo = List.find (fun m -> m#textHas onsheet) !gmodules in
				mo#pathSheet ()
			) else "" in
			let cnt = ref 0 in
			List.iter (fun m -> 
				if m#getLibRef() = modname && m#pathHas ar then (
					List.iter (fun t -> 
						t#setSize (width, height); 
						t#setWidth thickness ; 
						t#setShow !show ; 
						t#update2 ();
						incr cnt; 
					) (m#getTexts()) ;
				) ;
			) !gmodules ; 
			printf "Updated %d texts\n%!" !cnt ; 
			render togl nulfun; 
		) frame2 in
		Tk.pack ~side:`Left~fill:`X [button] ; 
		let all = frame1 :: frame2 :: showframe :: frames in
		Tk.pack ~fill:`Both ~expand:true all ; 
	in
	let minTextSizeAdjust () = 
		let dlog = Toplevel.create top in
			Wm.title_set dlog "minimum text size";
		let frame1  = msgframe dlog "Change the minimum text dimensions for *all* modules.  (Initial setting: the present minimum)" in
		(* maybe should get the minimum sizes here .. ? *)
		let minwidth = ref 10.0 in
		let minheight = ref 10.0 in
		let minthick = ref 10.0 in
		List.iter (fun m -> 
			List.iter (fun t -> 
				let width,height = t#getSize() in
				let thick = t#getWidth () in
				minwidth := min width !minwidth ; 
				minheight := min height !minheight ; 
				minthick := min thick !minthick ; 
			) (m#getTexts ())
		) !gmodules ; 
		let msgs = [| ("text height",(sof !minheight));
			("text width",(sof !minwidth));
			("text line thickness",(sof !minthick)) |] in
		let framelist = Array.map (entryframe dlog) msgs in
		let frames = Array.to_list (Array.map fst framelist) in
		let cbs = Array.map snd framelist in
		let frame2 = Frame.create dlog in
		let button = Button.create ~text:"set!" 
		~command:(fun () -> 
			let width = fos ((cbs.(0))()) in
			let height = fos ((cbs.(1))()) in
			let thickness = fos ((cbs.(2))()) in
			let cnt = ref 0 in
			List.iter (fun m -> 
				List.iter (fun t -> 
					let w,h = t#getSize () in
					t#setSize ((max w width),(max h height)); 
					let tk = t#getWidth () in
					t#setWidth (max tk thickness) ;
					t#update2 (); 
					incr cnt; 
					render togl nulfun; (* interactive! *)
				) (m#getTexts()) ;
			) !gmodules ; 
			printf "Updated %d texts\n%!" !cnt ; 
		) frame2 in
		Tk.pack ~side:`Left~fill:`Both [button] ; 
		let all = frame1 :: frame2  :: frames in
		Tk.pack ~fill:`Both ~expand:true all ; 
	in
	let padSolderMaskAdjust () = 
		(* see if there are any pads with non-zero drill that are not on the solder mask layers *)
		(* through-hole pads should generally have soldermask openings on both sides of the board *)
		(* soldermask component = 23 *)
		(* soldermask copper = 22 *)
		let count = ref 0 in
		let total = ref 0 in
		List.iter (fun m-> 
			List.iter (fun p -> 
				if p#getDrill() > 0. && (not (p#hasLayer 22) || not (p#hasLayer 23) ) then (
					incr count; 
				);
				incr total ; 
			) (m#getPads())
		) !gmodules ; 
		if !count > 0 then (
			(* open up a Tk dialog to ask user if error should be corrected *)
			let resp = Dialog.create ~parent:top ~title:"Ajust soldermask on pads" 
				~message:((soi !count) ^ " through-hole pads (pads with a non-zero drill size)"
					^ " (out of " ^ (soi !total)
					^ " total) were found to not have soldermask opening on both top and bottom. \n"
					^ " Fix this? " )
				~buttons:["Yes"; "No"] ()
			in
			if resp = 0 then (
				List.iter (fun m-> 
					List.iter (fun p -> 
						if p#getDrill() > 0. then (
							if not (p#hasLayer 22) then (
								p#addLayer 22 
							) ;
							if not (p#hasLayer 23) then (
								p#addLayer 23 
							) ;
						);
					) (m#getPads())
				) !gmodules ; 
			) ; 
		) else (
			ignore( Dialog.create ~parent:top ~title:"Ajust soldermask on pads" 
				~message:"All pads with drill have soldermask openings on both sides of board"
				~buttons:["Ok"] () ); 
		) ; 
	in
	let filterModulesBySheet () = 
		let dlog = Toplevel.create top in
		Wm.title_set dlog "Filter Modules By Sheet";
		let msg = Message.create ~text:"Show any parts whose netname matches" ~width:155 dlog in
		let exp = Entry.create ~width:10 dlog in
		let button1 = Button.create ~text:"Go"
			~command:(fun () -> 
				let sn = Entry.get exp in
				List.iter (fun m -> 
					if m#sheetNameHas sn then m#setVisible true
					else m#setVisible false
				) !gmodules ;
				gratsnest#clearAll (); 
				redoRatNest (); 
				render togl nulfun; (*show the user the results!*)
			) dlog  in
		let msg2 = Message.create ~text:"Show all parts on same sheet as (including sub-sheets)" ~width:130 dlog in
		let exp2 = Entry.create ~width:10 dlog in
		let button2 = Button.create ~text:"Go"
			~command:(fun () -> 
				let sn = Entry.get exp2 in
				let mo = List.find (fun m -> m#textHas sn) !gmodules in
				let ar = mo#pathSheet () in
				List.iter (fun m -> 
					if m#pathHas ar then m#setVisible true
					else m#setVisible false
				) !gmodules ;
				(* let's do the same with the tracks - 
				hide those not associated with nets of visible modules. *)
				let netset = List.fold_right (fun m s ->
					if m#getVisible () then (
						List.fold_right SI.add (m#getPadNets()) s
					) else s
				) !gmodules (SI.empty) 
				in
				List.iter (fun t ->
					if SI.exists ((=) (t#getNet())) netset then (
						t#setVisible true 
					) else (
						t#setVisible false 
					); 
				) !gtracks ; 
				gratsnest#clearAll (); 
				redoRatNest (); 
				render togl nulfun; (*show the user the results!*)
			) dlog  in
		let button3 = Button.create ~text:"Show All!"
			~command:(fun () -> 
				List.iter (fun m -> m#setVisible true) !gmodules ;
				List.iter (fun m -> m#setVisible true) !gtracks ;
				gratsnest#clearAll (); 
				redoRatNest (); 
				render togl nulfun; (*show the user the results!*)
			) dlog in
		Tk.pack ~fill:`Y ~side:`Left 
			[Tk.coe msg; Tk.coe exp; Tk.coe button1;  
			Tk.coe msg2; Tk.coe exp2; Tk.coe button2; Tk.coe button3]; 
	in
	
	let makeBOM () = 
		(* make a bill of materials : 
		group multiple components with the same value and the same footprint together *)
		(* use a hash table indexed by string*string   *)
		let hash = Hashtbl.create 1000 in
		List.iter (fun m->
			Hashtbl.add hash ((m#getValue()),(m#getLibRef())) (m#getRef()) ; 
		) !gmodules; 
		(* select a file to save *)
		let filetyp = [ {typename="comma seperated text";extensions=[".csv"];mactypes=[]} ] in
		let filename = (getSaveFile ~defaultextension:".csv" ~filetypes:filetyp ~title:"save BOM" ()) in
		let oc = open_out filename in
		(* now iterate over the keys, only processing each key once *)
		fprintf oc ("value,footprint,count,references\n"); 
		let used = Hashtbl.create 1000 in
		Hashtbl.iter (fun key _ -> 
			if not (Hashtbl.mem used key) then (
				Hashtbl.add used key true ; 
				let value,footprint = key in (* value e.g. "10K" *)
				let refs = Hashtbl.find_all hash key in
				fprintf oc "%s,%s,%d," value footprint (List.length refs);
				List.iter (fun s-> fprintf oc "%s "s ) refs ; 
				fprintf oc "\n"; 
				flush oc; 
			) ; 
		) hash ; 
		close_out_noerr oc; 
	in

	let save_lua () = (
		(* iterate over all modules, creating a lua file that duplicates the structure *)
		(* select a file to save *)
		let filetyp = [ {typename="lua code";extensions=[".lua"];mactypes=[]} ] in
		let filename = (getSaveFile ~defaultextension:".lua" ~filetypes:filetyp 
			~title:"save lua module description" ()) in
		let oc = open_out filename in
		fprintf oc "-- I have no idea if this is the most efficient \n"; 
		fprintf oc "-- way to do it.  well, gotta start somewhere. \n";
		fprintf oc "l = {}\n"; 
		let i = ref 1 in
		List.iter(fun m-> m#save_lua oc i) !gmodules; 
		fprintf oc "return l\n"; 
		close_out_noerr oc; 
	) in
	
	let make_xyr () = 
		(* make a xyr file, which contains the x, y, and rotation for each component, 
		along with pin 1 absolute location. 
		positions are in inches, decimal *)
		let filetyp = [ {typename="x y rotation";extensions=[".xyr"];mactypes=[]} ] in
		let savexyr layer = 
			let title = if layer = 15 then "Top XYR" else "Bottom XYR" in
			(* assume we flip along the y-axis *)
			let xscale = if layer = 15 then 1.0 else -1.0 in
			(* flipping also incurrs a 180deg rotation *)
			let radd = if layer=15 then 0.0 else 180.0 in
			let filename = (getSaveFile ~defaultextension:".xyr" ~filetypes:filetyp ~title ()) in
			let oc = open_out filename in
			fprintf oc "#PCB parts location file %s\n" title; 
			fprintf oc "#refdes	value	foot	x	y	r	p1x	p1y\n"; 
			List.iter ( fun m->
				m#update(); (* just in case .. *)
				let x,y = m#getCenter false in
				let r1 = xscale *. ((foi (m#getRot())) /. 10.0) +. radd in (* xscale to do the flip..*)
				let r2 = if r1 >= 360.0 then r1 -. 360.0 else (if r1 < 0.0 then 360.0 +. r1 else r1 ) in
				(* resistors and capacitors: just output 0 or 90 for rotation. *)
				(* this improves robot acccuracy *)
				let rd = m#getRefChar () in
				let r = if rd = "C" || rd = "R" || rd = "L" then (
						if r2 >= 180.0 then r2 -. 180.0 else r2
					) else r2 in
				(* the pad1 stuff is used for validation *)
				let p1x,p1y = try 
					let pad1 = List.find( fun p -> p#getPadName() = "1" )
					(m#getPads()) in
					pad1#getCenter()
				with Not_found -> 0.0, 0.0 in
				fprintf oc "%s\t%s\t%s\t%f\t%f\t%f\t%f\t%f\n"
					(m#getRef())
					(m#getValue())
					(m#getLibRef())
					x y r p1x p1y ; 
			) (List.filter (fun m -> m#getLayer() = layer) !gmodules ); 
			(* also we should add in through-hole pads for alignment -- 
			this because the surface mount pads will be covered in paste when actually pnping.. *)
			let fidnum = ref 0 in
			List.iter (fun m -> 
				List.iter (fun p -> 
					if p#getShape () = Pad_Circle then (
						let x1,y = p#getCenter () in
						let x = x1 *. xscale in
						if p#hasLayer (string_to_layer "Bot") && 
						p#hasLayer (string_to_layer "Top") then (
							let p1x,p1y = (p#getSx()),(p#getSy()) in
							fprintf oc "%s\t%s\t%s\t%f\t%f\t%f\t%f\t%f\n"
								("F"^(soi !fidnum))
								"TH-Fiducial"
								"F"
								x y 0.0 p1x p1y ; 
							incr fidnum; 
						) else (
							(* otherwise, see if it has a drill hole..*)
							let size = p#getDrill () in
							if size > 0.0 then (
								fprintf oc "%s\t%s\t%s\t%f\t%f\t%f\t%f\t%f\n"
									("F"^(soi !fidnum))
									"Drill-Fidcl"
									"F"
									x y 0.0 size size ; 
								incr fidnum; 
							)
						)
					) 
				) (m#getPads ())
			) (List.filter (fun m -> m#getLayer() = 15) !gmodules );
			close_out_noerr oc; 
		in
		savexyr 15 ; 
		savexyr 0 ;
	in
	let filetyp = [ {typename="boards";extensions=[".brd"];mactypes=[]} ] in
	(*add file menu entries *)
	let openCommand () = 
		let fname = Tk.getOpenFile ~defaultextension:".brd" 
			~filetypes:filetyp ~title:"open board" () in
		openFile top fname;
	in
	Menu.add_command filemenu ~label:"Open (Ctrl-O)" ~command:openCommand ; 
	let openNetlistCommand () = 
		let filetyp2 = [ {typename="netlist";extensions=[".net"];mactypes=[]} ] in
		let fname = Tk.getOpenFile ~defaultextension:".net" 
			~filetypes:filetyp2 ~title:"open netlist" () in
		let mods,nets = read_netlist fname gmodules in
		gmodules := mods; 
		gnets := nets; 
		(* need to update the zone nets *)
		List.iter (fun z -> 
			let name = z#getNetName() in
			let nn = try Some (List.find (fun n -> n#getName() = name) !gnets) 
				with _ -> ignore( Dialog.create ~parent:top ~title:"Zone netname changed" 
				~message:("Zone netname "^ name ^" not found!\n"^"Layer "^(layer_to_string (z#getLayer())) )
				~buttons:["Ok"] () ); None in
			(match nn with 
				| Some n2 -> z#setNetNum (n2#getNet ()); 
				| None -> ()
			)
		) !gzones ; 
		(* redo the rat's nest. *)
		propagateNetcodes2 gmodules gtracks true false top 
			(fun () -> render togl nulfun) ; 
		redoRatNest() ; 
	in
	Menu.add_command filemenu ~label:"Load netlist" ~command:openNetlistCommand ; 
	(* 
	let filetypsch = [ {typename="schematics";extensions=[".sch"];mactypes=[]} ] in
	Menu.add_command filemenu ~label:"Open schematic" ~command:
		(fun () -> 
			let fname = Tk.getOpenFile ~defaultextension:".sch" 
				~filetypes:filetypsch ~title:"open schematic" () in
			let schema = new schematic in
			linenum := 0 ; 
			schema#openFile fname "00000000" "root" ; 
			schema#print "" ; 
		); 
	*)
	Menu.add_command filemenu ~label:"Array" ~command:arrayFun; 
	Menu.add_command filemenu ~label:"Save As" ~command:
		(fun () -> 
			let fname2 = (getSaveFile ~defaultextension:".brd" 
				~filetypes:filetyp ~title:"save board" ()) in
			saveall fname2; 
			Wm.title_set top ("Kicad Ocaml " ^ fname2 );
			(* update gfname last in case the user clicks cancel and 
			an exception is thrown *)
			gfname := fname2; 
		) ; 
	Menu.add_command filemenu ~label:"Save (Ctrl-S)" ~command:
		(fun () -> saveall !gfname; ) ; 
	Menu.add_command filemenu ~label:"Quit" ~command:
		(fun () -> 
			(!gclosesock) () ; 
			let geo = Wm.geometry_get top in
			closeTk () ; 
			(* save the file list (perhaps this will include more preferences, later *)
			let prefs = getPrefsFile () in 
			let oc = open_out prefs in
			print_endline( "writing preferences " ^ prefs ) ; 
			List.iter (fun (board, schematic) -> 
				fprintf oc "%s %s\n" board schematic;
			) !gfilelist ; 
			fprintf oc "Geometry %s\n" geo; 
			(* write the persistent variables *)
			let writebool name v = 
				let tv = if !v then "true" else "false" in
				fprintf oc "%s %s\n" name tv; 
			in
			let writefloat name v = 
				fprintf oc "%s %f\n" name v 
			in
			writebool "genabledepth" genabledepth;
			writebool "groute135" groute135;
			writebool "gtrackKeepSlope" gtrackKeepSlope;
			writebool "gdrawtracks" gdrawtracks; 
			writebool "gdrawmods" gdrawmods; 
			writebool "gdrawzones" gdrawzones; 
			writebool "gdrawratsnest" gdrawratsnest;
			writebool "gdrawratsnest" gdrawratsnest;
			writebool "gshowPadNumbers" gshowPadNumbers; 
			writebool "gshowHiddenText" gshowHiddenText; 
			writebool "ggridDraw" ggridDraw; 
			writebool "ggridSnap" ggridSnap; 
			writebool "gsnapTracksToGrid" gsnapTracksToGrid; 
			writebool "gdrawText" gdrawText;
			writebool "gdosnap" gdosnap; 
			writebool "gTrackEndpointOnly" gTrackEndpointOnly;
			writebool "gTrackDragConnected" gTrackDragConnected; 
			writebool "gtrackDRC" gtrackDRC; 
			writefloat "ggrid0" ggrid.(0);
			writefloat "ggrid1" ggrid.(1);
			writefloat "ggrid2" ggrid.(2);
			writefloat "ggrid3" ggrid.(3);
			writefloat "ggrid4" ggrid.(4);
			flush oc ; 
			close_out_noerr oc; 
		); 
	List.iter (fun (fil, _) -> 
		Menu.add_command filemenu ~label:fil ~command:
			(fun () -> openFile top fil) ; 
		) filelist; 
		
	(* add the layer buttons *)
	let makeLayerFrame choicelist layerlist radiocallback checkcallback container =
		let layer_to_tkcolor layer = 
			let (r,g,b) = layer_to_color (string_to_layer layer) in
			let ftohex f = 
				let c = iof (((f*. 0.5) +. 0.5) *. 255.) in
				Printf.sprintf "%2.2x" c
			in
			"#" ^ (ftohex r) ^ (ftohex g) ^ (ftohex b) 
		in
		let frame0 = Frame.create container in
		let bframe = Frame.create frame0 in
		let var = Textvariable.create ~on:bframe () in
		Textvariable.set var (List.hd choicelist);
		let blist = List.map (fun choice ->
				  Radiobutton.create ~indicatoron:true ~text:choice ~value:choice
					~background:(`Color (layer_to_tkcolor choice))
					~variable:var ~command:(fun () -> radiocallback choice) bframe)
				choicelist in
		Tk.pack ~side:`Left ~fill:`X blist;
		let cframe = Frame.create frame0 in
		let clist = List.map (fun choice -> 
			let v = Textvariable.create ~on:cframe () in
			Textvariable.set v "On" ; 
			Checkbutton.create cframe ~text:choice ~indicatoron:true
			~variable:v
			~background:(`Color (layer_to_tkcolor choice))
			~offvalue:"Off" ~onvalue:"On"
			~command:(fun () -> checkcallback choice ((Textvariable.get v)="On")  )
			) layerlist in
		Tk.pack ~side:`Left ~fill:`X clist;
		let curlayer = Message.create ~width:70 ~text:"Current L." frame0 in
		let enlayer = Message.create ~width:70 ~text:"Enabled L." frame0 in
		Tk.pack ~side:`Left ~fill:`X 
			[Tk.coe curlayer; Tk.coe bframe; Tk.coe enlayer; Tk.coe cframe];
		let updateradio choice =
		List.iter2 (fun c b -> 
			if c = choice then (
				Radiobutton.select b ; 
				radiocallback choice ; 
			) else ())
		  choicelist blist
		in
		(frame0, updateradio)
	in
	let raiseLayer lay = 
		if List.exists ((=) lay) !glayerPresent then (
			glayer := lay ; 
			glayerZlist := List.filter (fun l -> not (l = lay)) !glayerZlist ; (* remove it *)
			glayerZlist := (lay :: !glayerZlist); (* put layer at head *)
			update_layer_z () ; 
			updateLayers lay true; 
			render togl nulfun
		) else ( print_endline "layer not present in board"; ) ; 
	in
	let laylist = ["SS_Top";"Top";"L1";"L2";"L3";"L4";"Bot";"SS_Bot";"Drawings";"SM_Top";"SM_Bot"] in
	let (layerframe,changelayercallback) = makeLayerFrame 
		laylist
		laylist
		(fun s -> raiseLayer (string_to_layer s) )
		(fun s b ->
			let lay = string_to_layer s in
			if List.exists ((=) lay) !glayerPresent then (
				updateLayers lay b; 
				render togl nulfun
			) else ( print_endline "layer not present in board"; ) ; 
		)
		top in
	let snapgrid cor = 
		(snap (fst cor) ggrid.(0) (fst !ggridorigin)), 
		(snap (snd cor) ggrid.(0) (snd !ggridorigin)) in
	(* mouse wheel corresponds to buttons 4 & 5 *)
	let calcCursPos ?(worknet=0) ?(onlyworknet=false) ev pan dosnap = 
		(*first map to screen, then translate and scale *)
		let w, h = !gwindowsize in
		let ar = (foi w)/.(foi h) in 
		let w2, h2 = (foi w) /. 2. , (foi h) /. 2. in
		let x = ((foi ev.ev_MouseX) -. w2) /. w2 *. ar in
		let y = ((foi ev.ev_MouseY) -. h2) /. h2 /. 1. in
		let out = (x /. !gzoom -. (fst pan)) , (y /. !gzoom -. (snd pan)) in
		
		(* see how many modules are selected for movement; 
		if there are any, don't hit any *)
		(* this prevents confusing cross-probing while moving modules *)
		let nonemoving = not ( List.exists (fun m -> m#getMoving () ) !gmodules ) in
		(* also should update the hit & snap *)
		gratsnest#clearSel (); 
		gsnapped := out ; 
		if !ggridSnap then ( (* either you grid snap or you track snap. *)
			gsnapped := snapgrid out
		); 
		if dosnap && nonemoving && (not !ghithold) then (
			(* set the hit flags& get a netnum *)
			let (nn,hitsize2,hitz2,hitclear2) = 
				if !gdrawmods then (
					List.fold_left (fun (netnum,hitsize,hitz,clearhit) m -> 
						m#hit out onlyworknet netnum hitsize hitz clearhit
					) (worknet, 1e24, -2e2, (fun() -> ()) ) !gmodules 
				) else (worknet, 1e24, -2e2, (fun() -> ()) )
			in
			(* printf "after mod: hitsize %f hitz %f\n%!" hitsize2 hitz2;*)
			let nn3,hitsize3,hitz3,hitclear3 = 
				if !gdrawtracks && !gmode <> Mode_MoveText then (
					List.fold_left (fun (netn1,hitsize,hitz,hitclear) track -> 
						track#hit (out, onlyworknet, hitsize, hitz, hitclear, netn1)
					) (nn,hitsize2,hitz2,[hitclear2]) !gtracks 
				) else nn,hitsize2,hitz2,[]
			in
			(* and do the zones .. *)
			let netn2,_,_,_ = 
				if !gdrawzones && !gmode <> Mode_MoveText then (
					List.fold_left (fun (netnum,hitsize,hitz,hitclear) zon -> 
						zon#hit out netnum hitsize hitz hitclear
					) (nn3,hitsize3,hitz3,hitclear3) !gzones  
				) else nn3,hitsize3,hitz3,hitclear3
			in
			gcurnet := netn2 ; 
		); 
		out
	in
	
	let updatecurspos ev = 
		gcurspos :=  calcCursPos ev !gpan true; 
		!gcursordisp "cursor" (fst !gcurspos) (snd !gcurspos) ;
		render togl nulfun;
	in
	let modfind cb = (
		let m = try Some
			( List.find (fun t -> t#getHit ()) !gmodules )
			with Not_found -> None
		in
		(match m with
			| Some mm -> cb mm
			| None -> ()
		)
	)in
	let bindVtoVia () = 
		bind ~events:[`KeyPressDetail("v")] ~fields:[`MouseX; `MouseY] ~action:(fun ev -> 
			(* try inserting a via. if it does not fit, 
			the DRC code should highlight what is offending *)
			(* update cursor & snap. *)
			if !gcurnet > 0 then (
				gcurspos := calcCursPos ev !gpan true; 
				let via = new pcb_track in
				via#setType Track_Via ; 
				via#setNet !gcurnet ; 
				via#setLayer !glayer ; (* does this even matter for vias? *)
				via#setWidth !gviapad ; 
				via#setDrill !gviadrill ; 
				(* try to insert the via -- move it a few times if needed, 
				as required to validate DRC *)
				let rec placeVia suggest count = 
					via#setStart suggest; 
					via#setEnd suggest;
					let violation, (suggest1,_) =  testdrc via !gtracks !gmodules in
					if violation then (
						if count < 20 then ( (* 20 iterations usually will locate a suitable spot.. *)
							placeVia suggest1 (count+1) (* try again *)
						)else (
							false (* could not find a place for the via *)
						)
					) else true (* the via was placed *)
				in
				if not (placeVia !gsnapped 0) then (
					print_endline "via will not fit :(" ; 
				) else (
					(* a suitable position was found! good. *)
					print_endline ("adding via to board, net:" ^ (soi !gcurnet)); 
					via#update () ; 
					gtracks := (via :: !gtracks); 
					gratsnest#updateTracks !gcurnet !gtracks ; 
				) ; 
				render togl nulfun;  
			) else print_endline "via would not be attached to a net!" ; 
		) top; 
	in
	(* declare the mouse bindings *) 
	
	let bindMouseAddTrack () = (
		gmode := Mode_AddTrack ;
		bindVtoVia () ; 
		let workingnet = ref 0 in
		let reenable = ref (fun () -> ()) in
		Mouse.releasePress top; 
		Mouse.bindPress top ~onPress:
		(fun ev -> 
			updatecurspos ev; (* get a new hit *)
			workingnet := !gcurnet ; 
			if ((!glayer >= 20 && glayerEn.(!glayer)) || (not !gtrackDRC)) then (
				workingnet := 0; (* the drawing net. *)
			);
			if !workingnet > 0 || !glayer >= 20 || (not !gtrackDRC) then (
				if List.exists (fun z -> z#getHit () ) !gzones then (
					(* try adding a point to a zone *)
					let zones = List.filter (fun z -> z#getHit()) !gzones in
					List.iter (fun z -> z#add !gcurspos) zones ;
					render togl nulfun; 
				) else (
					(* we should figure out the orientation of the present track, if we are extending any *)
					(* in this way, we won't allow 90 or even 45 deg bends *)
					let hittracks = List.filter (fun t -> t#getHit()) !gtracks in
					let hitvec, hiton = 
						if List.length hittracks = 1 && !groute135 then (
							let t = List.hd hittracks in
							let u = t#getU () in
							if u = 0.0 then ( (* we hit the start! *)
								( Pts2.sub (t#getStart()) (t#getEnd()) ), true
							) else if u = 1.0 then (
								(Pts2.sub (t#getEnd()) (t#getStart()) ), true
							) else ( (* if it was hit in the middle, we do whatever *)
								(0.0, 0.0), false
							)
						) else ( (0.0, 0.0), false ) 
					in
					(* update the layer *)
					(* see if we hit a pad - if we hit a pad and a track, may not need to change layers *)
					(* therefore update the hit vars *)
					printf "adding track ..\n%!" ; 
					(* this is old code -- use the middle mouse button to change layers if need be. 
					ignore( List.fold_left (fun (nn,hitsize,hitz,clearhit) m -> 
						m#hit !gcurspos false nn hitsize hitz clearhit
					) (0, 1e24, -2e2, (fun() -> ()) ) !gmodules ); 
					let padHasLayer = List.exists (fun m-> 
						List.exists (fun p -> 
							(p#getHit ()) && (p#hasLayer !glayer)
						) (m#getPads ())
					) !gmodules in
					(* see if the present layer is in the hittracks *)
					let preslayergood = List.exists (fun t -> 
						t#getLayer() = !glayer || t#isVia() ) hittracks in
					(* if it's not, choose the one with the largest Z *)
					let sorted = List.sort (fun b a -> 
						compare (glayerZ.(a#getLayer())) (glayerZ.(b#getLayer()))) hittracks in
					let lay = if preslayergood || padHasLayer || List.length sorted = 0 then !glayer else 
						(List.hd sorted)#getLayer() in
					if lay <> !glayer then changelayercallback (layer_to_string lay); *)
					let track = new pcb_track in
					gtracks := (track :: !gtracks); 
					gcurspos := calcCursPos ~worknet:!workingnet ~onlyworknet:true ev !gpan true; 
					track#setStart !gsnapped ; 
					track#setEnd !gsnapped ; 
					track#setNet !workingnet ; 
					track#setLayer !glayer ; 
					track#setWidth !gtrackwidth ; 
					track#setMoving true ; 
					let track2 = new pcb_track in
					(* don't add the second track unless we are doing 135 routing. *)
					if !groute135 then gtracks := (track2 :: !gtracks); 
					track2#setStart !gsnapped ; 
					track2#setEnd !gsnapped ; 
					track2#setNet !workingnet ; 
					track2#setLayer !glayer ; 
					track2#setWidth !gtrackwidth ; 
					track2#setMoving true ; 
					reenable := (fun () -> 
						track#setMoving false ; 
						track2#setMoving false ; ) ; 
					track#update (); 
					render togl nulfun; 
					let lastgood = ref (!gsnapped) in
					let lastgoodmp = ref (!gsnapped) in
					let start = !gsnapped in
					Mouse.bindMove 1537 top ~action:
					(fun evinf -> 
						gcurspos := calcCursPos ~worknet:!workingnet ~onlyworknet:true evinf !gpan true; 
						(* will also clear the selected ratsnest *)
						let dx,dy = Pts2.sub !gsnapped start in
						let sign f = 
							if f = 0. then 0. 
							else if f < 0. then -1. 
							else 1.
						in
						let sx,sy= (sign dx),(sign dy) in
						let fx,fy = (fabs dx), (fabs dy) in
						let mpa, mpb, twosegments =  (* mp = mid point *)
							if dx = 0. || dy = 0. || not !groute135 then (
								!gsnapped , !gsnapped, false
							) else (
								(* method: project into the first quadrant & form midpoint 
								then project back into proper quadrant *)
								if fx > fy then (
									(dx -. fy *. sx, 0.) , (fy *. sx, fy *. sy), true
								) else (
									(0. , dy -. fx *. sy) , (fx *. sx, fx *. sy), true
								)
							)
						in
						(* given the previous track, switch the order that the midpoint is 
						tried out to lessen the chance we are making a 90 or 45 angle.*)
						let mpa2, mpb2 = 
							if hiton then (
								if (Pts2.dotnorm mpa hitvec) > (Pts2.dotnorm mpb hitvec) then (
									mpa, mpb
								) else ( mpb, mpa )
							) else (
								mpa, mpb
							)
						in
						(* try out both, default to the midpoint 'a' 
						(the vertical / horizontal first) *)
						if twosegments then (
							track#setEnd (Pts2.add start mpa2) ; 
							track2#setStart (Pts2.add start mpa2); 
							track2#setEnd !gsnapped ; 
							if !gtrackDRC && (testdrc2 track !gtracks !gmodules 
								|| testdrc2 track2 !gtracks !gmodules) then (
								(* that layout didn't work, try 'b' *)
								track#setEnd (Pts2.add start mpb2) ; 
								track2#setStart (Pts2.add start mpb2); 
								(* don't need to update the end of track2 *)
								if not(testdrc2 track !gtracks !gmodules) &&
									not (testdrc2 track2 !gtracks !gmodules) then (
									lastgood := !gsnapped ; 
									lastgoodmp := Pts2.add start mpb2 ; 
								); 
							) else (
								(*no problem with midpoint a *)
								lastgood := !gsnapped; 
								lastgoodmp := Pts2.add start mpa2 ; 
							); 
						) else (
							track#setEnd !gsnapped ; 
							if !gtrackDRC then (
								let vio, (suggest,_) = testdrc track !gtracks !gmodules in
								if vio && suggest != (0. , 0.) then (
									(* if there is a violation, try out the suggestion! *)
									(* try  to normalize track length though, so that it is (approximately)
									the length the user desires. *)
									let sugstart = Pts2.norm (Pts2.sub suggest start) in
									let desstart = Pts2.sub !gsnapped start in
									let suggest2 = Pts2.add start 
										(Pts2.scl sugstart (Pts2.length desstart)) in
									track#setEnd suggest2 ;
									gsnapped := suggest2 ;
									if testdrc2 track !gtracks !gmodules then (
										(* if extending it didn't work, stick with the original suggestion *)
										track#setEnd suggest ;
										gsnapped := suggest ;
									);
								) ;
								if not (testdrc2 track !gtracks !gmodules) then (
									lastgood := !gsnapped; 
									lastgoodmp := !gsnapped ; 
								)
							) else (
								lastgood := !gsnapped; 
								lastgoodmp := !gsnapped ; 
							); 
						) ; 
						let node = Ratnode.create !workingnet !lastgood in
						gratsnest#highlight node !workingnet (); 
						track#setEnd !lastgoodmp ; 
						track#update () ; 
						track2#setStart !lastgoodmp ; 
						track2#setEnd !lastgood ; 
						if !groute135 then track2#update () ; 
						gcurnet := !workingnet;
						render togl nulfun; 
					); 
				) ;
			);
		)
		~onRelease:
		(fun ev -> 
			!reenable () ;
			gbutton1pressed := false ;
			(* remove any zero-length tracks that may have been created in the process *)
			(* this may delete other tracks .. but ehwell !*)
			(*gtracks := List.filter (
				fun t -> t#manhattanLength() > 0. || t#getType() != Track_Track
			) !gtracks ; *)
			gratsnest#updateTracks !workingnet !gtracks ; 
			Mouse.releaseMove 1642 top ; 
			updatecurspos ev ; 
		) ; 
	)in
	
	let bindMouseMoveTrack () = (
		gmode := Mode_MoveTrack ;
		bindVtoVia () ; 
		let workingnet = ref 0 in
		let tracks = ref [] in
		let zones = ref [] in
		let startPoint = ref (0., 0.) in
		Mouse.releasePress top; 
		Mouse.bindPress top ~onPress:
		(fun ev -> 
			workingnet := !gcurnet ; 
			ignore(  calcCursPos ~worknet:!workingnet ~onlyworknet:true ev !gpan true ); 
			startPoint := !gsnapped ;
			tracks := List.filter (fun t -> t#getHit() ) !gtracks ; 
			zones := List.filter (fun z -> z#getHit ()) !gzones ;
			List.iter (fun z -> z#setMoving true false) !zones ; 
			if List.length !tracks > 0 then (
				print_endline "redoing tracks list in drag";
				(* make a list of the tracks that are directly connected to hit tracks 
					& update their hitpoints (u) *)
				(* do not look at tracks that cross this one, only ones that terminate
				on this one *)
				let net = !gcurnet in
				let layer = (List.hd !tracks)#getLayer() in
				let nettracks = List.filter (
					fun t-> t#getNet() = net
				) !gtracks in
				let addtracks = ref [] in
				(* set the move flags on all selected tracks *)
				List.iter (fun ht -> ht#setMoving true; ) !tracks ; 
				(* search for tracks connected to the hit tracks *)
				let rec hitTrack ht =
					(* if we hit the middle of a track, then drag the whole thing *)
					(* plus anything attached to it *)
					let u = ht#getU() in
					let alllayers = ht#getType() = Track_Via in
					if (u > 0. && u < 1.) || alllayers then (
						let st = ht#getStart () in
						let en = ht#getEnd () in
						let width2 = ht#getWidth() *. 0.5 in
						List.iter (fun t -> 
							if (t#getLayer() = layer || alllayers) && (not (t#getMoving())) then (
								let ost = t#getStart() in
								let oen = t#getEnd() in
								let hitstart = Pts2.tracktouch st en ost width2 
									|| Pts2.distance st ost < width2 in
								let hitend = Pts2.tracktouch st en oen width2
									|| Pts2.distance st oen < width2 in
								if hitend then (
									(* just reverse the track -- makes the later parts easier *)
									t#setStart oen ; 
									t#setEnd ost ; 
								) ; 
								let setConstraint isstart =
									let c = t#getStart() in
									let d = t#getEnd() in
									if not (Pts2.parallel st en c d) then (
										let fu = ( fun a b -> 
											let e = Pts2.intersectPt a b c d in
											t#setStart e ; 
											e
										) in
										t#setU (-1.) ; (* this is a flag indicating the move is being handled here *)
										if isstart then ( ht#setMoveStartConstraint fu
										) else ( ht#setMoveEndConstraint fu ); 
										(* constraints take a proposed line and project it onto the constraint line. *)
									) else (
										if isstart then ( ht#clearStartConstraint () 
										) else ( ht#clearEndConstraint () ); 
									) ; 
								in
								if (hitstart || hitend) then ( 
									t#setU 0. ; 
									(* snap to the endpoints *)
									if (Pts2.distance2 st (t#getStart())) < (Pts2.distance2 en (t#getStart())) || alllayers 
									then (
										t#setStart st ; 
										if !gtrackKeepSlope && not alllayers && not (t#isVia()) then 
											setConstraint true ; 
									) else (
										t#setStart en  ; 
										if !gtrackKeepSlope && not alllayers && not (t#isVia()) then 
											setConstraint false ; 
									);
									t#setMoving true ; (* so we don't count it twice *)
									addtracks := (t :: !addtracks) ; 
									(* if a via moves then all other tracks attached to it move *)
									if t#getType() = Track_Via && !gTrackDragConnected 
									then ( hitTrack t ); 
								) ; 
							) ; 
						) nettracks ; 
					)
				in
				if !gTrackDragConnected then List.iter hitTrack !tracks; 
				tracks := List.rev_append !addtracks !tracks ; 
				List.iter (fun t-> t#setHit true) !tracks ; 
				(* untracks := List.filter (fun t -> not (t#getHit())) !gtracks; *)
			) ; 
			let safemove = ref (0. , 0.) in 
			gbutton1pressed := true ; 
			Mouse.bindMove 1747 top ~action:
			(fun evinf ->
				let cx,cy = calcCursPos ~worknet:!workingnet 
					~onlyworknet:true evinf !gpan true in
				gdrag :=  Pts2.sub !gsnapped !startPoint ; 
				!gcursordisp "d" (fst !gdrag) (snd !gdrag) ; 
				(* simple method: try moving the tracks; if there is an error, 
				snap back to last safe position *)
				if !gsnapTracksToGrid then (
					let p = snapgrid (cx,cy) in 
					List.iter (fun t -> 
						let u = t#getU () in
						t#move (0.0,0.0); 
						let p2 = if u < 1.0 then t#getStart () else t#getEnd () in
						t#move (Pts2.sub p p2) ;
					) !tracks
				) else (
					List.iter (fun t -> t#move !gdrag ) !tracks ; 
				); 
				List.iter (fun t -> t#setDirty true ) !tracks ; 
				(* try drcpush before drctest *)
				let tab = ref 0 in
				if !gpushrouting then (
					List.iter (fun t -> incr tab; ignore(pushdrc t !gtracks !gmodules ((soi !tab) ^ "/") )) !tracks 
				) ; 
				let violation = 
					(* note: don't have to update the graphics to test DRC. *)
					List.exists (fun t -> testdrc2 t !gtracks !gmodules; ) !tracks in
					(* need to think about how to use the suggest DRC output here *)
				if violation then (
					List.iter (fun t -> t#move !safemove ) !tracks ; 
				) else (
					safemove := !gdrag ; 
				); 
				(* update the tracks moved by the pushdrc *)
				List.iter (fun t -> if t#getDirty() then t#update () ) !gtracks ; 
				(* now, move the zones *)
				let curspos = if !ggridSnap then (
					snapgrid (cx,cy)
				) else cx,cy in
				List.iter (fun z -> z#move curspos) !zones ; 
				gcurnet := !workingnet;
				render togl nulfun;
			) ; 
		)
		~onRelease:
		(fun ev -> 
			gbutton1pressed := false ;
			Mouse.releaseMove 1797 top ; 
			(* need to update the moved tracks, if there were any *)
			List.iter (fun t-> 
				t#setMoving false; 
				t#setHit false;
			) !tracks ; 
			List.iter (fun z -> z#setMoving false false; z#empty ()) !zones ;
			(* do this second so that all constraints have a chance to be applied *)
			List.iter (fun t -> t#clearConstraint (); t#update () ) !tracks ; 
			gratsnest#updateTracks !workingnet !gtracks ; 
			updatecurspos ev ; 
		) ;
		let zonefind cb = (
			let m = try Some
				( List.find (fun t -> t#getHit ()) !gzones )
				with Not_found -> None
			in
			(match m with
				| Some mm -> cb mm
				| None -> ()
			)
		)in
		bind ~events:[`KeyPressDetail("e")] ~action:
			(fun _ -> zonefind (fun z-> z#edit top)) top; 
	)in
	
	let bindMouseMoveText () = (
		gmode := Mode_MoveText ; 
		let modules = ref [] in
		let startPos = ref (0. , 0.) in
		Mouse.releasePress top; 
		Mouse.bindPress top ~onPress:
		(fun ev -> 
			startPos :=  calcCursPos ev !gpan true; 
			if not !gbutton3pressed then (
				modules := List.filter (fun m -> let _,found = m#txthit() in found ) !gmodules ; 
				List.iter (fun m -> m#setMoving true ) !modules ;
				gbutton1pressed := true ; 
				Mouse.bindMove 1835 top ~action:
				(fun evinf ->
					let prescurspos = calcCursPos evinf !gpan true in
					(* text moves should alway work. *)
					gdrag :=  Pts2.sub prescurspos !startPos ; 
					!gcursordisp "d" (fst !gdrag) (snd !gdrag) ; 
					List.iter (fun m -> m#move !gdrag ) !modules ; 
					render togl nulfun;
				) ; 
			) ; 
		) 
		~onRelease:
		(fun evinf ->
			gbutton1pressed := false ;
			Mouse.releaseMove 1849 top ; 
			(* need to update the moved tracks, if there were any *)
			List.iter (fun m -> m#setMoving false; ) !modules ; 
			updatecurspos evinf ; 
		) ; 
		(* unbind the v-key *)
		bind ~events:[`KeyPressDetail("v")] ~action:(fun _ -> ()) top; 
		bind ~events:[`KeyPressDetail("e")] ~action:
			(fun _ -> modfind (fun m-> m#edit top)) top; 
	)in	
	
	let bindMouseMoveModule () = (
		gmode := Mode_MoveModule ;
		let modules = ref [] in 
		let safemove = ref (0. , 0.) in
		let startPos = ref (0. , 0.) in
		let tracks = ref [] in
		Mouse.releasePress top; 
		Mouse.bindPress top ~onPress:
		(fun ev -> 
			startPos :=  calcCursPos ev !gpan true; 
			tracks := [] ;
			modules := List.filter (fun m -> m#getHit() ) !gmodules ; 
			(* maybe do a selection of layer here.. later. *)
			List.iter (fun m -> m#setMoving true ) !modules ;
			(* move any tracks with ends on the pads, 
			snap them to the center of the pads, (if possible), and DRC test. *)
			(* turn this feature off, it sucks!
			List.iter (fun m-> 
				let pos = m#getPos() in
				let _, txtht = m#txthit () in
				if not txtht then (
					List.iter (fun p -> 
						List.iter (fun t-> 
							if p#hasLayer (t#getLayer()) then (
								let oldstart = t#getStart() in
								let oldend = t#getEnd() in
								let hit1,snap1 = p#pointInPad (Pts2.sub oldstart pos) in
								if hit1 then (
									t#setStart (Pts2.add pos snap1); 
									(* if the snap creates a violation, then don't snap *)
									if testdrc2 t !gtracks !gmodules then t#setStart oldstart ; 
									t#setU 0. ; 
									t#setMoving true ; 
									tracks := (t :: !tracks); 
								)else(
									let hit2,snap2 = p#pointInPad (Pts2.sub oldend pos) in
									if hit2 then (
										t#setEnd (Pts2.add pos snap2); 
										(* if the snap creates a violation, then don't snap *)
										if testdrc2 t !gtracks !gmodules then t#setEnd oldend ; 
										t#setU 1. ; 
										t#setMoving true ; 
										tracks := (t :: !tracks); 
									); 
								); 
							) ; 
						) !gtracks; 
					) (m#getPads()) ; 
				); 
			) !modules ; 
			*)
			(* printf "bindMouseMoveModule: binding movement\n%!" ; *)
			Mouse.bindMove 1912 top ~action:
			(fun evinf ->
				let prescurspos = calcCursPos evinf !gpan true in
				(* try the move; if it causes any problems, back up. *)
				gdrag :=  Pts2.sub prescurspos !startPos ; 
				!gcursordisp "d" (fst !gdrag) (snd !gdrag) ; 
				(* may want to snap to grid here (snap the center of the modules) *)
				if !ggridSnap then (
					List.iter (fun m -> 
						m#move (0.0 , 0.0);
						let ox, oy = m#getCenter true in
						m#move !gdrag ; (* move it, then snap *)
						let cx,cy = m#getCenter true in
						let snp = snapgrid (cx,cy) in
						m#move ((fst snp) -. ox, (snd snp) -. oy); 
					)! modules ; 
				) else (
					List.iter (fun m -> m#move !gdrag ) !modules ;
				); 
				(* update the drag position .. *)
				List.iter (fun t -> t#move !gdrag ) !tracks ; 
				let violation = 
					(* note: don't have to update the graphics to test DRC. *)
					List.exists (fun t -> testdrc2 t !gtracks !gmodules; ) !tracks in
				if violation then (
					List.iter (fun m -> m#move !safemove ) !modules ; 
					List.iter (fun t -> t#move !safemove ) !tracks ; 
				) else (
					safemove := !gdrag ; 
				); 
				(*need to update the graphics for the tracks (not needed for modules) *)
				List.iter (fun t-> t#update (); ) !tracks ; 
				render togl nulfun; 
			) ;
		)
		~onRelease: 
		(fun evinf ->
			(* printf "bindMouseMoveModule: releasing movement\n%!" ;*)
			Mouse.releaseMove 1950 top ; 
			(* need to update the moved tracks, if there were any *)
			List.iter (fun m -> m#setMoving false; m#setHit false) !modules ; 
			(* need to update the moved tracks, if there were any *)
			List.iter (fun t-> t#setMoving false; t#setHit false; ) !tracks ; 
			gratsnest#updateTracksAll !gtracks ; 
			updatecurspos evinf ; 
		) ; 
		(* bind the v and e keys. *)
		bind ~events:[`KeyPressDetail("e")] ~action:
			(fun _ -> modfind (fun m-> m#edit top)) top; 
		bind ~events:[`KeyPressDetail("v")] ~action:
			(fun _ -> modfind (fun m-> m#editValue top)) top;
	)in
	
	let bindMouseSelect () = (
		(* method: let the user drag the cursor to make a transparent rectangle
		when the mouse is released, select all modules & tracks & zones that intersect 
		the box, and keep them selected as long as 'shift' is held down. 
		upon the next click, move the modules and tracks. (using existing logic). *)
		(* use gmode to return to the old state once shift is released *)
		let selectPress _ = 
			ghithold := true; 
			Mouse.bindPress top ~onPress:
			(fun ev -> 
				let (sx,sy)  = calcCursPos ev !gpan true in
				if not !gbutton3pressed then (
					List.iter (fun m-> m#setHit false) !gmodules; 
					List.iter (fun t -> t#setMoving false) !gtracks ; 
					Mouse.bindMove 1979 top ~action:
					(fun evinf -> 
						let (px,py) = calcCursPos evinf !gpan true in
						gselectRect := ((fmin sx px),(fmin sy py),(fmax sx px),(fmax sy py)) ; 
						!gcursordisp "size" ((fmax sx px) -. (fmin sx px))  ((fmax sy py) -. (fmin sy py)); 
						(* iterate through the modules & update selected *)
						if !gdrawmods then (
							List.iter (fun m-> 
								if bbxIntersect (m#getBBX false) !gselectRect && m#getVisible() then 
									m#setHit true 
								else m#setHit false
							) !gmodules ; 
						);
						(* same for the tracks *)
						List.iter (fun t-> 
							if t#selectHit !gselectRect then 
								t#setHit true
							else t#setHit false
						) !gtracks ; 
						(* same for the zones *)
						List.iter (fun t-> 
							if t#selectHit !gselectRect then 
								t#setHit true
							else t#setHit false
						) !gzones ; 
						render togl nulfun; 
					)
				)
			)
			~onRelease: 
			(fun evinf -> 
				let modules = List.filter (fun m-> m#getHit ()) !gmodules in
				let tracks = List.filter (fun t-> t#getHit ()) !gtracks in
				let zones = List.filter (fun t-> t#getHit ()) !gzones in
				let worknets = List.fold_right (fun t -> SI.add (t#getNet () ))  tracks (SI.empty) in
				gselectRect := (1e99,1e99,1e99,1e99) ; 
				Mouse.releaseMove 2006 top; 
				updatecurspos evinf ; 
				printf "%d modules and %d tracks selected\n" (List.length modules) (List.length tracks);
				printf "please drag the selected modules .. (or release shift) \n%!" ; 
				printf "known bug: do not rotate and drag at the same time!\n%!" ; 

				(* also add in 'c' for copy tracks *)
				bind ~events:[`Modified([`Shift], `KeyPressDetail"C")] ~action:
				(fun _ ->
					printf "copying %d tracks and %d zones in drag...\n%!" 
						(List.length tracks) (List.length zones); 
					let newtracks = List.map (fun t -> Oo.copy t) tracks in
					let newzones = List.map (fun z -> Oo.copy z) zones in
					List.iter (fun t -> 
						t#setHit false; 
						t#applyMove () ) tracks ;
					List.iter (fun t -> 
						t#setHit false ; 
						t#applyMove () ) zones ;
					List.iter (fun t -> 
						t#newGrfx (); (* need to make a new graphics b/c the track is copied. *)
						t#update (); 
						t#setHit true) newtracks ;
					gtracks := List.rev_append newtracks !gtracks; (* add them to the global list *)
					List.iter (fun t -> 
						t#newGrfx (); (* need to make a new graphics b/c the track is copied. *)
						t#update (); 
						t#setHit true) newzones ;
					gzones := List.rev_append newzones !gzones; (* add them to the global list *)
					render togl nulfun; 
				) top;
				(* and a function to shift tracks between layers *)
				bind ~events:[`Modified([`Shift], `KeyPressDetail"L")] ~action:
				(fun _ ->
					printf "moving %d tracks to current layer %d...\n%!" 
						(List.length tracks) (List.hd !glayerZlist); 
					List.iter (fun t -> 
						t#setLayer (List.hd !glayerZlist); 
						t#update ()) tracks ;
					render togl nulfun; 
				) top;
				let tracks = List.filter (fun t-> t#getHit ()) !gtracks in
				let zones = List.filter (fun t-> t#getHit ()) !gzones in
				(* release the old press .. yes this is confusing now *)
				Mouse.releasePress top ; (* unbind this function even though it is executing! *)
				Mouse.bindPress top ~onPress:
				(fun evv -> 
					(* redefine tracks -- user may have copied some *)
					printf "dragging %d tracks and %d modules and %d zones\n%!" 
						(List.length tracks) (List.length modules) (List.length zones); 
					List.iter (fun m-> m#setMoving true ) modules ;
					List.iter (fun m-> m#setMoving true ) tracks ;
					List.iter (fun z-> z#setMoving true true) zones ; 
					List.iter (fun m-> m#setU 0.5 ) tracks ; 
					SI.iter (fun n -> gratsnest#updateTracks ~final:false n !gtracks ) worknets; 
					gbutton1pressed := true ;
					ignore( calcCursPos evv !gpan true ); 
					let startPos = !gsnapped in
					Mouse.bindMove 2221 top ~action:
					(fun evv -> 
						ignore( calcCursPos evv !gpan true ); 
						let prescurspos = !gsnapped in
						gdrag :=  Pts2.sub prescurspos startPos ;
						!gcursordisp "d" (fst !gdrag) (snd !gdrag) ; 
						List.iter (fun m -> m#move !gdrag ) modules ; 
						List.iter (fun t -> t#move !gdrag ) tracks ;
						List.iter (fun t -> t#move !gdrag ) zones ; 
						List.iter (fun t-> t#update (); ) tracks ; 
						render togl nulfun; 
					); 
				) 
				~onRelease: 
				(fun evv ->
					gbutton1pressed := false ;
					List.iter (fun m-> m#setMoving false ) modules ; 
					List.iter (fun m-> m#setMoving false ) tracks ;
					List.iter (fun z-> z#setMoving false false) zones ; 
					(* List.iter (fun m-> m#setHit false ) tracks ;  *)
					(* make a set of all the working nets, update them *)
					SI.iter (fun n -> gratsnest#updateTracks ~final:true n !gtracks ) worknets;
					Mouse.releaseMove 2066 top; 
					(* shiftRelease evv ; *)
					updatecurspos evv ; 
				); 
				printf "press backspace to remove associated tracks \n" ; 
				printf "middle click to rotate the modules\n%!"; 
				(* the code for this is below, in the default binding to backspace *)
			); 
		in
		let selectRelease _ = 
			ghithold := false ; 
			(* List.iter (fun m-> m#setHit false ) !gmodules ;  
			List.iter (fun m-> m#setHit false ) !gtracks ; *)
			Mouse.releasePress top ; 
			(* clear the old binding *)
			bind ~events:[`Modified([`Shift], `KeyPressDetail"C")] ~action:(fun _ -> ()) top ; 
		in
		
		bind ~events:[`KeyPressDetail("Shift_R")] ~action:selectPress top ; 
		bind ~events:[`KeyPressDetail("Shift_L")] ~action:selectPress top ; 
		
		bind ~events:[`KeyReleaseDetail("Shift_R")] ~action:selectRelease top ; 
		bind ~events:[`KeyReleaseDetail("Shift_L")] ~action:selectRelease top ; 
	)in 
	(* cursor mode radiobuttons *)
	let modelist = ["move Module";"move teXt";"move Track";"Add track"] in
	let mframe = Frame.create menubar in
	let mvar = Textvariable.create ~on:mframe () in
	Textvariable.set mvar (List.hd modelist);
	let setMode s = 
		(* unselect all when changing mode -- 
		otherwise, text can get 'stuck on' *)
		List.iter (fun m -> m#setHit false) !gmodules; 
		match (String.lowercase s) with 
			| "move module" -> bindMouseMoveModule () ; 
			| "move text" -> bindMouseMoveText () ; 
			| "move track" -> bindMouseMoveTrack () ;  
			| "add track" -> bindMouseAddTrack () ;
			| _ -> () ; 
	in
	let clist = ["#8537ff";"#48c7ff";"#e55c6a";"#e2873d"] in
	let mlist = List.map2 (fun choice color ->
		Radiobutton.create ~indicatoron:true ~text:choice ~value:choice
				~background:(`Color color)
				~variable:mvar 
				~command:(fun () -> setMode choice) mframe)
			modelist clist in
	let updateMode choice =
		List.iter2 (fun c b -> 
			if (String.lowercase c) = (String.lowercase choice) then (
				Radiobutton.select b ; 
				setMode choice ; 
			) else ())
		  modelist mlist ; 
		render togl nulfun;
	in
	Tk.pack ~side:`Left ~fill:`X mlist;
	
	(* to milimeter function *)
	let tomm gg = (sof gg) ^ " (" ^ (sof (gg *. 25.4)) ^ " mm)" in
	let viaprint pad drill = "pad " ^ (tomm pad) ^ " drill " ^ (tomm drill) in
	
	let trackAdd w = 
		gtrackwidth := w ;
		if not (List.mem w !gtrackwidthlist) then (
			gtrackwidthlist := w :: !gtrackwidthlist; 
			gtrackwidthlist := List.sort compare !gtrackwidthlist; 
			(* find the sorted index *)
			let j = ref 0 in
			let indx = ref 1 in
			List.iter (fun ww -> 
				if ww = w then ( indx := !j ) ;
				incr j ; 
			) !gtrackwidthlist ; 
			(* add into the menu  .. sorted *)
			Menu.insert_command ~index:(`Num !indx) trackmenu 
				~label:(tomm w) ~command: (fun _ -> 
				gtrackwidth := w ; 
				print_endline ("track width set to " ^ (tomm w));
			) ; 
		) else (
			(* print_endline ("track width already in list " ^ (tomm w)); *)
		)
	in
	gTrackAdd := trackAdd ; 
	trackAdd 0.005 ;  (* useful defaults *)
	trackAdd 0.010 ; 
	trackAdd 0.015 ; 
	trackAdd 0.025 ; 
	let tracksFun _ = 
		let dlog = Toplevel.create top in
		ghithold := false ; (* because the user depressed 'shift' to get here *)
		Wm.title_set dlog "Track sizes" ; 
		(* make a bunch of buttons for selecting the track size, 
		plus one at the end for adding a new track size *)
		let buttons = List.map (fun w -> 
			let frame = Frame.create dlog in
			let button = Button.create ~text:("width " ^ (tomm w))
				~command:(fun () -> 
					gtrackwidth := w; 
					print_endline ("track width set to " ^ (sof w));
				(* Tk.destroy dlog; *) 
				)
				frame 
			in
			Tk.pack ~fill:`Both ~expand:true ~side:`Left [button] ; 
			frame) !gtrackwidthlist ; 
		in
		let frame = Frame.create dlog in
		let msg = Message.create ~text:"new width:"  frame in
		let newwidth = Entry.create ~width:10 frame in
		let button = Button.create ~text:("add") ~command: 
			(fun () -> 
				let w = fos (Entry.get newwidth) in
				trackAdd w ;
				Tk.destroy dlog; 
				print_endline "sorry closing the dialog as I don't know how to add a button to an existing frame"; 
			) frame in
		Tk.pack ~side:`Left ~fill:`Both ~expand:true [Tk.coe msg; Tk.coe newwidth; Tk.coe button] ; 
		let all = frame :: buttons in
		Tk.pack ~fill:`Both ~expand:true all; 
	in
	(* vias callback *)
	let setvia pad drill = 
		gviapad := pad; 
		gviadrill := drill ;  
		print_endline ("via set to " ^ (viaprint pad drill));
	in
	let viaAdd pad drill = 
		setvia pad drill ; 
		if not (List.mem (pad,drill) !gviasizelist) then (
			gviasizelist := ( pad ,drill ) :: !gviasizelist; 
			gviasizelist := List.sort compare_I2 !gviasizelist; 
			(* find the sorted index *)
			let j = ref 0 in
			let indx = ref 1 in
			List.iter (fun pd -> 
				if pd = (pad,drill) then ( indx := !j ) ;
				incr j ; 
			) !gviasizelist ; 
			(* add into the menu  .. sorted *)
			Menu.insert_command ~index:(`Num !indx) viamenu 
				~label:( viaprint pad drill )
				~command: (fun _ -> setvia pad drill 
				) ; 
		) else (
			(* print_endline ("via size already present: "^(viaprint pad drill));  *)
		)
	in
	gViaAdd := viaAdd ;
	viaAdd 0.016 0.006 ; (* some good defaults .. *)
	viaAdd 0.025 0.010 ; 
	viaAdd 0.045 0.020 ; 
	let viasFun _ = 
		let dlog = Toplevel.create top in
		ghithold := false ; (* because the user depressed 'shift' to get here *)
		Wm.title_set dlog "Via sizes" ; 
		(* make a bunch of buttons for selecting the track size, 
		plus one at the end for adding a new track size *)
		let buttons = List.map (fun (pad,drill) -> 
			let frame = Frame.create dlog in
			let button = Button.create ~text: (viaprint pad drill)
				~command:(fun () -> 
					setvia pad drill ; 
					(* Tk.destroy dlog; *)
				)
				frame 
			in
			Tk.pack ~fill:`Both ~expand:true ~side:`Left [button] ; 
			frame) !gviasizelist ; 
		in
		let frame = Frame.create dlog in
		let msg = Message.create ~text:"new pad:"  frame in
		let newpad = Entry.create ~width:10 frame in
		let msg2 = Message.create ~text:"drill:"  frame in
		let newdrill = Entry.create ~width:10 frame in
		let button = Button.create ~text:("add") ~command: 
			(fun () -> 
				let pad = fos (Entry.get newpad) in
				let drill = fos (Entry.get newdrill) in
				viaAdd pad drill ; 
				print_endline "sorry closing the dialog as I don't know how to add a button to an existing frame"; 
				Tk.destroy dlog; 
			) frame in
		Tk.pack ~side:`Left ~fill:`Both ~expand:true 
			[Tk.coe msg; Tk.coe newpad; Tk.coe msg2; Tk.coe newdrill; Tk.coe button] ; 
		let all = frame :: buttons in
		Tk.pack ~fill:`Both ~expand:true all; 
	in
	(* let the user expand this list *)
	Menu.add_command trackmenu ~label:"add ..." ~command:tracksFun; 
	(*likewise for vias. *)
	Menu.add_command viamenu ~label:"add ..." ~command:viasFun;
	
	let gridAdd w = 
		ggrid.(0) <- w ;
		if not (List.mem w !ggridsizelist) then (
			ggridsizelist := w :: !ggridsizelist; 
			ggridsizelist := List.sort compare !ggridsizelist; 
			(* find the sorted index *)
			let j = ref 0 in
			let indx = ref 1 in
			List.iter (fun ww -> 
				if ww = w then ( indx := !j ) ;
				incr j ; 
			) !ggridsizelist ; 
			(* add into the menu  .. sorted *)
			Menu.insert_command ~index:(`Num !indx) gridmenu 
				~label:(tomm w) ~command: (fun _ -> 
				ggrid.(0) <- w ; 
				print_endline ("grid snap size set to " ^ (tomm w));
			) ; 
		) else (
			(* print_endline ("track width already in list " ^ (tomm w)); *)
		)
	in 
	gridAdd 0.001 ;  (* useful defaults *)
	gridAdd 0.002 ;  
	gridAdd 0.005 ; 
	gridAdd 0.010 ; 
	gridAdd 0.050 ; 
	gridAdd 0.060 ; 
	gridAdd 0.100 ;
	gridAdd 0.120 ;
	gridAdd 0.240 ;
	gridAdd 0.500 ; 
	gridAdd 1.000 ; 
	
	(* cells *)
	let cellMenuAdd () = 
		let dlog = Toplevel.create top in
		Wm.title_set dlog "Cells" ; 
		(* have a bunch of checkboxes per cell, plus a box for adding a new one. *)
		let buttons = List.map (fun c -> 
			let cframe = Frame.create dlog in (* horizontal *)
			let v = Textvariable.create ~on:cframe () in
			Textvariable.set v (if (c#getVisible ()) then "On" else "Off" ) ; 
			let ckbutton = Checkbutton.create cframe ~text:(c#getName ())
				~indicatoron:true ~variable:v 
				~offvalue:"Off" ~onvalue:"On"
				~command:(fun () -> 
					c#setVisible ((Textvariable.get v)=="On")
					) in
			let swbutton = Button.create cframe ~text:(c#getName ())
				~command:(fun () -> gcurrentcell := Some c) in
			Tk.pack ~side:`Left ~fill:`Y ~expand:true [Tk.coe ckbutton; Tk.coe swbutton]; 
			cframe 
		) !gcells in
		let f2 = Frame.create dlog in
		let msg = Message.create ~text:"new cell:"  f2 in
		let newcell = Entry.create ~width:10 f2 in
		let button = Button.create ~text:("add") ~command: 
			(fun () -> 
				let cell = new pcb_cell in
				cell#setName (Entry.get newcell) ; 
				gcells := cell :: !gcells;  
				!gCellMenuRefresh () ; 
				Tk.destroy dlog; 
				print_endline "sorry closing the dialog as I don't know how to add a button to an existing frame"; 
			) f2 in
		Tk.pack ~side:`Left ~fill:`Both ~expand:true [Tk.coe msg; Tk.coe newcell; Tk.coe button] ; 
		let all = f2 :: buttons in
		Tk.pack ~fill:`Both ~expand:true all; 
	in
	let cellMenuRefresh () =
		Menu.insert_command ~index:(`Num 0) cellmenu ~label:"All" 
			~command:(fun _ -> gcurrentcell := None) ; 
		List.iteri (fun i c -> 
			Menu.insert_command ~index:(`Num (i+1)) cellmenu 
				~label:(c#getName ()) ~command: (fun _ ->
					gcurrentcell := Some c) ) 
			(List.sort (fun a b -> String.compare (a#getName ()) (b#getName()))
				!gcells); 
		Menu.insert_command ~index:(`Num ((List.length !gcells) + 1)) cellmenu
			~label:"Add" ~command:cellMenuAdd ; 
	in
	gCellMenuRefresh := cellMenuRefresh ; 
	cellMenuRefresh (); (* to get the 'all' and 'add' entries. *)
	
	(* add in the sub-options menus .. *)
	let displaySub = Menu.create optionmenu in (* pass the contatining menu as the final argument *)
	let tracksSub = Menu.create optionmenu in
	let viasSub = Menu.create ~tearoff:false optionmenu in
	let textsSub = Menu.create ~tearoff:false optionmenu in
	let zonesSub = Menu.create ~tearoff:false optionmenu in
	let gridsSub = Menu.create ~tearoff:false optionmenu in
	let ratsnestSub = Menu.create ~tearoff:false optionmenu in
	let alignSub = Menu.create optionmenu in
	let checkSub = Menu.create optionmenu in
	let miscSub = Menu.create optionmenu in
	(* add them *)
	Menu.add_cascade ~label:"Display options ..."  ~menu:displaySub optionmenu ; 
	Menu.add_cascade ~label:"Tracks ..."  ~menu:tracksSub optionmenu; 
	Menu.add_cascade ~label:"Vias ..."  ~menu:viasSub optionmenu; 
	Menu.add_cascade ~label:"Texts ..."  ~menu:textsSub optionmenu; 
	Menu.add_cascade ~label:"Zones ..."  ~menu:zonesSub optionmenu; 
	Menu.add_cascade ~label:"Grids ..."  ~menu:gridsSub optionmenu; 
	Menu.add_cascade ~label:"Ratsnest ..."  ~menu:ratsnestSub optionmenu; 
	Menu.add_cascade ~label:"Align ..."  ~menu:alignSub optionmenu; 
	Menu.add_cascade ~label:"Check ..."  ~menu:checkSub optionmenu; 
	Menu.add_cascade ~label:"Misc ..."  ~menu:miscSub optionmenu; 
	(*add options*)
	let addOption ?(bindkey="") menu label cmd ic = 
		let v = Textvariable.create() in
		Textvariable.set v (if ic then "On" else "Off") ; 
		if bindkey <> "" then (
			bind ~events:[`KeyPressDetail(bindkey)] 
			~action:(fun _ ->
				Textvariable.set v (if (Textvariable.get v)="Off" then "On" else "Off") ; 
				cmd( (Textvariable.get v)="On" ) ; 
			) top; 
		) ;
		Menu.add_checkbutton menu ~label ~indicatoron:true
			~accelerator:bindkey
			~variable:v
			~offvalue:"Off" ~onvalue:"On"
			~command:(fun () -> cmd( (Textvariable.get v)="On" )) ; 
	in 
	addOption displaySub "z buffer" (fun b -> genabledepth := b ) !genabledepth; 
	addOption displaySub "draw tracks" (fun b -> gdrawtracks := b) !gdrawtracks ; 
	addOption displaySub "draw zones" (fun b -> gdrawzones := b) !gdrawzones ; 
	addOption displaySub "grid draw" (fun b -> ggridDraw := b ) !ggridDraw; 
	addOption displaySub "draw module text" (fun b -> gdrawText := b) !gdrawText ; 
	addOption displaySub "draw hidden text" (fun b -> gshowHiddenText := b) !gshowHiddenText ; 
	addOption displaySub "draw modules" (fun b -> gdrawmods := b) !gdrawmods ; 
	addOption displaySub "draw ratsnest" (fun b -> gdrawratsnest := b) !gdrawratsnest ; 
	addOption displaySub "draw pad numbers" (fun b -> gshowPadNumbers := b) !gshowPadNumbers ; 
	
	Menu.add_command tracksSub ~label:"Tracks dialog (Ctrl-T)" ~command:tracksFun ; 
	addOption tracksSub "135 degree routing" ~bindkey:"5" (fun b -> 
			groute135 := b; 
			printf "135 deg routing %s\n%!" (if b then "on" else "off") ;
		) !groute135 ; 
	addOption tracksSub "when dragging tracks mantain slope" (fun b -> gtrackKeepSlope := b) !gtrackKeepSlope ; 
	addOption tracksSub "push routing" (fun b -> gpushrouting := b) !gpushrouting ; 
	addOption tracksSub "enable DRC on tracks" (fun b -> gtrackDRC := b) !gtrackDRC;
	addOption tracksSub "snap tracks to eachother" (fun b -> gdosnap := b) !gdosnap; 
	addOption tracksSub "only track endpoints active" (fun b -> gTrackEndpointOnly := b) !gTrackEndpointOnly;
	addOption tracksSub "drag connected tracks too" (fun b -> gTrackDragConnected := b) !gTrackDragConnected; 
	Menu.add_command tracksSub ~label:"Remove duplicate tracks" ~command:
	(fun () -> 
		List.iter (fun t -> t#setDirty false) !gtracks; (* dirty flag for removal *)
		let maxlayer = List.fold_left (fun l t -> max l (t#getLayer())) 0 !gtracks in
		for lay = 0 to maxlayer do
			printf "working on layer %s\n%!" (layer_to_string lay); 
			let rec recmatch trks =
				match trks with 
				| hd :: tl -> (
					let w = hd#getWidth () in
					List.iter (fun t -> 
						let dw = t#getWidth() -. w in
						if dw < 0.0001 && dw > -0.0001 && not (t#getDirty()) then (
							let st = hd#getStart () in
							let en = hd#getEnd () in
							let stt = t#getStart() in
							let ent = t#getEnd() in 
							let d = (Pts2.distance st stt) +.
										(Pts2.distance en ent) in
							let d2 = (Pts2.distance en stt) +.
										(Pts2.distance st ent) in
							if d < 0.0001 || d2 < 0.0001 then (
								t#setDirty true
							(* see if they are parallel, and share a endpoint -- if so, merge *)
							) else (if Pts2.parallel2 st en stt ent then (
(* 								printf "parallel!\n%!";  *)
								if Pts2.distance st stt < 0.0001 || 
									Pts2.distance st ent < 0.0001 ||
									Pts2.distance en stt < 0.0001 ||
									Pts2.distance en ent < 0.0001 then (
									(* grow the present track *)
									(* have to figure out the endpoints: the two points that are furthest apart *)
									let g = ref 0.0 in
									let rec longest pl = 
										match pl with
										| p :: ptl -> (
											let rec longest2 y =
												match y with 
												| p2 :: ptl2 -> 
													let dd = Pts2.distance p p2 in
													if dd > !g then (
														g := dd; 
														hd#setStart p; 
														hd#setEnd p2; 
													) ; 
													longest2 ptl2
												| _ -> () 
											in
											longest2 ptl; 
											longest ptl )
										| _ -> ()
									in
									longest [st; en; stt; ent]; 
									hd#update (); 
									t#setDirty true; 
								)
							)); 
						)
					) tl; 
					recmatch tl; 
				)
				| [] -> () in
			recmatch (List.filter (fun t -> t#getLayer() == lay) !gtracks)
		done; 
		printf "removed %d tracks\n%!" (List.length (List.filter (fun t -> t#getDirty () ) !gtracks));
		gtracks := List.filter (fun t -> not (t#getDirty () )) !gtracks; 
	); 
	
	Menu.add_command viasSub ~label:"Vias dialog (Ctrl-V)" ~command:viasFun ;
	Menu.add_command viasSub ~label:"Adjust via drill sizes" ~command:viaDrillAdjust ; 
	Menu.add_command viasSub ~label:"Teardrop vias" ~command:(fun () -> 
		gtracks := List.rev_append (Teardrop.teardrop !gtracks !gmodules) !gtracks ; 
		let vias,tracks = List.partition (fun t-> t#getType() = Track_Via) !gtracks in
		gtracks := List.rev_append tracks vias; (* so that the vias are drawn last *)
		render togl nulfun; ); 
	Menu.add_command viasSub ~label:"Remove teardrops" ~command:(fun () -> 
		gtracks := Teardrop.unteardrop !gtracks ; 
		render togl nulfun; ); 
	
	
	Menu.add_command textsSub ~label:"Adjust text sizes per module" ~command:textSizeAdjust ; 
	Menu.add_command textsSub ~label:"Adjust minimum text sizes" ~command:minTextSizeAdjust ; 
	Menu.add_command textsSub ~label:"Adjust text position from template" ~command:textPositionAdjust ; 
	
	addOption zonesSub "draw zones" (fun b -> gdrawzones := b) !gdrawzones ; 
	Menu.add_command zonesSub ~label:"Add zone" 
		~command:(fun () -> 
			let z = new zone in
			z#set_corners (List.map (fun (x,y) -> 
				let cx, cy = !gpan in
				let gz = 2.0 *. !gzoom in
				(x/. gz) -. cx, (y /. gz) -. cy
			) [(1.0,1.0);(-1.0,1.0);(-1.0,-1.0);(1.0,-1.0);]); 
			z#update (); 
			gzones := z :: !gzones; 
			z#edit top ); 
	Menu.add_command zonesSub ~label:"Refill all zones"
		~command: (fun () -> List.iter (fun z -> z#fill !gtracks !gmodules) !gzones) ; 
	Menu.add_command zonesSub ~label:"Empty all zones"
		~command: (fun () -> List.iter (fun z -> z#empty ()) !gzones) ; 
	Menu.add_command zonesSub ~label:"Show zone fill algorithm window"
		~command: (fun () -> Mesh.makewindow top ) ; 
	
	Menu.add_command gridsSub ~label:"Grids ... " 
		~command:(fun _ -> Grid2.dialog top (fun () -> render togl nulfun) gridAdd ) ;
	addOption gridsSub "grid draw" (fun b -> ggridDraw := b ) !ggridDraw; 
	addOption gridsSub "grid snap" (fun b -> ggridSnap := b ) !ggridSnap; 
	addOption gridsSub "snap tracks to grid" (fun b -> gsnapTracksToGrid := b) !gsnapTracksToGrid; 
	
	Menu.add_command ratsnestSub ~label:"Propagate netcodes to unconn. (nn=0) tracks" 
		~command:(fun () -> 
			propagateNetcodes2 gmodules gtracks false false top 
				(fun () -> render togl nulfun) ; 
			redoRatNest () ); 
	Menu.add_command ratsnestSub ~label:"Propagate netcodes to all tracks" 
		~command:(fun () -> 
			propagateNetcodes2 gmodules gtracks true false top 
				(fun () -> render togl nulfun) ; 
			redoRatNest () ); 
	(*the following option no longer works *)
	addOption ratsnestSub "show all nets when selected" (fun b -> gdragShowAllNets := b ) !gdragShowAllNets; 
	addOption ratsnestSub "show 4 smallest nets when selected" (fun b -> gdragShow5Smallest := b ) !gdragShow5Smallest; 
	addOption ratsnestSub "limit ratnest computation to nets with < 200 nodes"
		(fun b -> gLimitRatnest200 := b) !gLimitRatnest200 ; 
		
	let addAlignCmd label cmd = 
		Menu.add_command alignSub ~label ~command:
		(fun () -> 
			cmd (List.filter (fun m-> m#getHit ()) !gmodules); 
			render togl nulfun; 
		); 
	in
	addAlignCmd "Align X (vertical)" Align.alignX ; 
	addAlignCmd "Align Y (horizontal)" Align.alignY ; 
	addAlignCmd "Distribute X (horizontal)" Align.distributeX ; 
	addAlignCmd "Distribute Y (vertical)" Align.distributeY ; 

	Menu.add_command checkSub ~label:"Check soldermask on through-hole pads" 
		~command:padSolderMaskAdjust ; 
	Menu.add_command checkSub ~label:"Check pad connectivity" 
		~command:(fun () -> 
			propagateNetcodes2 gmodules gtracks true true top 
				(fun () -> render togl nulfun) ; 
			redoRatNest () ); 
	Menu.add_command checkSub ~label:"Check DRC (track/pad copper spacing)"
		~command:(testdrcAll gtracks gmodules top (fun () -> render togl nulfun)); 
	Menu.add_command checkSub ~label:"Check for component overlaps"
		~command:(fun _ -> 
		let drcerr = ref [] in
		let rec checkmod mlist = 
			(match mlist with 
				| hd :: tl -> (
					let bbx = hd#getBBX false in
					let lay = hd#getLayer () in
					List.iter (fun m -> 
						let bbx2 = m#getBBX false in
						let lay2 = m#getLayer () in
						if lay = lay2 && bbxIntersect bbx bbx2 then (
							(* pick whichever is the smaller *)
							let bbx3 = if (bbxSize bbx) < (bbxSize bbx2) then bbx else bbx2 in
							drcerr := ((bbxCenter bbx3),hd,m) :: !drcerr; 
						)
					) tl ; 
					checkmod tl; 
				)
				| [] -> ()
			)
		in
		checkmod !gmodules; 
		if List.length !drcerr > 0 then (
			(* make a dialog *)
			let dlog = Toplevel.create top in
			Wm.title_set dlog "Overlaps" ;
			let err = ref [] in
			let min a b = if a < b then a else b in
			for i = 1 to (min 30 (List.length !drcerr)) do (
				err := ((List.nth !drcerr (i-1)) :: !err) ; 
			) done ; 
			let cnt = ref 0 in
			let buttons = List.map (fun (p,m1,m2) -> 
				incr cnt ; 
				let gettxt m = try (List.hd (m#getTexts ()))#getText () with _ -> "?" in
				let txt1 = gettxt m1 in
				let txt2 = gettxt m2 in
				Button.create ~text:((soi !cnt) ^ ": " ^ (sof (fst p))^", "^(sof (snd p))^" "^txt1^" & "^txt2)
					~command:(fun () -> 
						gpan := (Pts2.scl p (-1.0)); 
						m1#setHit true;
						m2#setHit true; 
						render togl nulfun )
					dlog) !err ; 
			in
			Tk.pack ~fill:`Both ~expand:true ~side:`Top buttons ; 
		) else (
			printf "No overlaps found!\n"; 
		)
	); 

	addOption miscSub "cross probe transmit" (fun b -> gcrossProbeTX := b) !gcrossProbeTX ; 
	addOption miscSub "cross probe recieve" (fun b -> gcrossProbeRX := b) !gcrossProbeRX ; 

	Menu.add_command miscSub ~label:"Save bill of materials" 
		~command:makeBOM ; 
	Menu.add_command miscSub ~label:"Save XYR (part location) file" 
		~command:make_xyr ; 
	Menu.add_command miscSub ~label:"Save lua file (parts & pads)" 
		~command:save_lua ; 
	Menu.add_command miscSub ~label:"Filter modules by schematic sheet"
		~command:filterModulesBySheet; 
	Menu.add_command miscSub ~label:"Move modules based on schematic position"
		~command: ( fun () -> 
		let dlog = Toplevel.create top in
		Wm.title_set dlog "Move mods" ; 
		let msg = Message.create ~text:"scaling:"  dlog in
		let scaling = Entry.create ~width:10 dlog in
		Entry.insert ~index:(`Num 0) ~text:"4000" scaling ; 
		let msg2 = Message.create ~text:"sheet: (blank for root)"  dlog in
		let sheet = Entry.create ~width:20 dlog in
		Entry.insert ~index:(`Num 0) ~text:"" sheet ; 
		let button = Button.create ~text:("move!")  dlog ~command:
		( fun () -> 
			(* see if we can find the sheet *)
			let sht = try 
				gschema#findSubSch2 (Entry.get sheet)
				with _ -> gschema
			in
			List.iter (fun m -> 
				let scl = fos (Entry.get scaling) in
				let ts = m#getTimeStamp () in
				let p = sht#componentPositon ts in
				(match p with
					| Some pp -> (
						m#setPos (Pts2.scl pp (1.0 /. scl)); 
						m#update (); )
					| None -> ()
				)
			) !gmodules ; 
			render togl nulfun; 
		) in
		let all = [Tk.coe msg; Tk.coe scaling; Tk.coe msg2; 
			Tk.coe sheet; Tk.coe button;] in
		Tk.pack ~fill:`Both ~expand:true all; 
	) ; 
	Menu.add_command miscSub ~label:"Move modules via simulated annealing"
		~command: ( fun () -> 
			gratsnest#clearAll (); 
			let dlog = Toplevel.create top in
			Wm.title_set dlog "Simulated annealing" ; 
			let frame = Frame.create dlog in
			let msg = Message.create ~text:"number of passes:"  frame in
			let passes = Entry.create ~width:10 frame in
			Entry.insert ~index:(`Num 0) ~text:"10" passes ; 
			let msg2 = Message.create ~text:"start temp:"  frame in
			let stemp = Entry.create ~width:10 frame in
			Entry.insert ~index:(`Num 0) ~text:"1" stemp ;
			let msg3 = Message.create ~text:"end temp:"  frame in
			let etemp = Entry.create ~width:10 frame in
			Entry.insert ~index:(`Num 0) ~text:"0.0" etemp ;
			let msg4 = Message.create ~text:"lock any modules with > pins:"  frame in
			let nlocke = Entry.create ~width:10 frame in
			Entry.insert ~index:(`Num 0) ~text:"22" nlocke ; 
			let button = Button.create ~text:("go") ~command: 
				(fun () -> 
					let pass = ios (Entry.get passes) in
					let starttemp = fos (Entry.get stemp) in
					let endtemp = fos (Entry.get etemp) in
					let nlock = ios (Entry.get nlocke) in
					gratsnest#clearAll (); 
					render togl nulfun; 
					(* remove the update callbacks *)
					List.iter (fun m -> 
						m#setUpdateCallback (fun _ -> ()) ; 
						List.iter (fun p -> 
							p#setMoveCallback (fun _ -> ()) ;
							p#setSelCallback (fun _ -> ()) ;
						) (m#getPads()); 
					) !gmodules; 
					let oldz = !genabledepth in
					genabledepth := false; 
					Anneal.doAnneal !gmodules (render togl) 
						starttemp endtemp pass nlock; 
					redoRatNest (); 
					genabledepth := oldz ;
				) frame in
			Tk.pack ~side:`Left ~fill:`Both ~expand:true 
				[Tk.coe msg; Tk.coe passes; Tk.coe msg2; Tk.coe stemp; 
				Tk.coe msg3; Tk.coe etemp; Tk.coe msg4; Tk.coe nlocke; 
				Tk.coe button] ; 
			let frame2 = Frame.create dlog in
			let msg5 = Message.create ~text:(
				"This tries to place components next to"^
				"other components that are connected to them.\n"^
				"It is not very accurate, and is not intended "^
				"to replace your intelligence :-)\n"^
				"However, it is good for moving things to reasonable "^
				"starting points, especially if you use \"Filter modules by sheet\"\n "^
				"to hide most of the modules (algorithm ignores hidden modules).\n"^
				"It also has no preset biases, and can give ideas for your layout.")frame2 in
			Tk.pack ~fill:`Both [Tk.coe msg5] ; 
			Tk.pack ~fill:`Both ~expand:true [frame2;frame]; 
		) ; 
	Menu.add_command miscSub ~label:"Enlarge tracks, mantain DRC"
		~command: ( fun () -> 
		let dlog = Toplevel.create top in
		Wm.title_set dlog "enlarge tracks" ; 
		let msg = Message.create ~text:"max track size:"  dlog in
		let size = Entry.create ~width:10 dlog in
		Entry.insert ~index:(`Num 0) ~text:"0.025" size ; 
		let button = Button.create ~text:("enlarge")  dlog ~command:
		( fun () -> 
			enlargeDrc !gtracks !gmodules 
				(fos (Entry.get size) ) 0.001
				(fun () -> render togl nulfun); 
		) in
		let all = [Tk.coe msg; Tk.coe size; Tk.coe button;] in
		Tk.pack ~fill:`Both ~expand:true all; 
	) ; 
	Menu.add_command miscSub ~label:"triangle meshing test" ~command:
	(fun () -> 
		let pts = Array.init 200 (fun _ -> (Random.float 4.0, Random.float 4.0)) in
		let p = List.rev_append (List.rev_map (fun (a,b) -> ((foi a),(foi b)))
			[(1,1);(1,1);(1,1);(1,1);(1,1);(1,1);(1,1);(1,1);(1,1); 
			 (0,0);(2,2);(4,4);(3,3);(5,5)] ) 
			(Array.to_list pts) in
		ignore(Mesh.mesh p [] (fun _ -> true)); 
	); 
	Menu.add_command miscSub ~label:"zone fill test" ~command:
	(fun () -> 
		let z = new zone in
		(* setup a simple square zone, 10x10*)
		z#set_corners (List.map (fun(x,y) -> foi x, foi y) [(0,0);(0,10);(10,10);(10,0)]);  
		let trk = new pcb_track in
		trk#setStart (2.0,5.0) ; 
		trk#setEnd (5.0, 2.0) ; 
		trk#setNet 1 ; 
		trk#setWidth 1.0 ; 
		let trk2 = new pcb_track in
		trk2#setStart (5.0,2.0) ; 
		trk2#setEnd (5.0, 8.0) ; 
		trk2#setNet 1 ; 
		trk2#setWidth 1.0 ; 
		let trk3 = new pcb_track in
		trk3#setStart (5.0,8.0) ; 
		trk3#setEnd (12.0, 5.0) ; 
		trk3#setNet 1 ; 
		trk3#setWidth 1.0 ; 
		let trk4 = new pcb_track in
		trk4#setStart (5.0,2.0) ; 
		trk4#setEnd (10.0, 1.0) ; 
		trk4#setNet 1 ; 
		trk4#setWidth 1.0 ; 
		z#fill [trk;trk2;trk3;trk4] []; 
	); 
	Menu.add_command miscSub ~label:"Export CIF" ~command:
	(fun () -> 
		let filetyp = [ {typename="Caltech Intermediate Format";extensions=[".cif"];mactypes=[]} ] in
		let fname2 = (getSaveFile ~defaultextension:".cif" 
			~filetypes:filetyp ~title:"Save CIF layout" ()) in
		exportCIF fname2; 
	); 
	Menu.add_command miscSub ~label:"Export Gerber" ~command:
	(fun () -> 
		let filetyp = [ {typename="Gerber 274x";extensions=[".gbr"];mactypes=[]} ] in
		let fname2 = (getSaveFile ~defaultextension:".gbr" 
			~filetypes:filetyp ~title:"Save gerber plot" ()) in
		exportGerber fname2; 
	); 

	Menu.add_command optionmenu ~label:"About" ~command:(helpbox "about" abouttext top) ; 
	(* get the menus working *)
	Menubutton.configure fileb ~menu:filemenu;
	Menubutton.configure optionb ~menu:optionmenu;
	Menubutton.configure viab ~menu:viamenu;
	Menubutton.configure trackb ~menu:trackmenu;
	Menubutton.configure gridb ~menu:gridmenu;
	Menubutton.configure cellb ~menu:cellmenu; 
	(* bind the buttons?? *)
	(* default *)
	bind ~events:[`ButtonPressDetail(3)] ~fields:[`MouseX; `MouseY] ~action:
		(fun ev -> 
			goldcurspos :=  calcCursPos ev !gpan false; 
			goldpan := !gpan ;
			Mouse.bindMove 2625 top ~action:
			(fun evinf ->
				let prescurs = calcCursPos evinf !goldpan false in
				gpan := Pts2.add (Pts2.sub prescurs !goldcurspos) !goldpan ; 
				render togl nulfun
			); 
		) top ;
	bind ~events:[`ButtonReleaseDetail(3)]  ~fields:[`MouseX; `MouseY] ~action:
		(fun _ -> 
			Mouse.releaseMove 2634 top ; 
			render togl nulfun;
		) top ; 
	
	let doRotate ev =
		gratsnest#clearSel (); 
		gcurspos := calcCursPos ev !gpan true; (* this will update the list of hit modules *)
		(* if shift is down, do the blockrotate *)
		if !ghithold then ( 
			(* printf "rotate modules\n%!"; *)
			let tracks = List.filter (fun t -> t#getHit()) !gtracks in
			Blockrotate.rotate 
				(List.filter (fun m -> m#getHit()) !gmodules)
				tracks ; 
			(* update the rats nest too. *)
			let worknets = List.fold_right (fun t -> SI.add (t#getNet () ))  tracks (SI.empty) in
			SI.iter (fun n -> gratsnest#updateTracks ~final:true n !gtracks ) worknets;
		) else (
			(* printf "rotate module\n%!"; *)
			List.iter (fun m -> m#rotate !gcurspos) !gmodules ; 
		); 
		render togl nulfun;
	in
	let doMirror ev vertical = 
		gratsnest#clearSel (); 
		gcurspos := calcCursPos ev !gpan true; (* this will update the list of hit modules *)
		(* operates independently of hithold .. simpler is better. *)
		let tracks = List.filter (fun t -> t#getHit()) !gtracks in
		Blockrotate.mirror tracks vertical;
		(* will have to manually update the rat's nest & connectivity. *)
		let worknets = List.fold_right (fun t -> SI.add (t#getNet () ))  tracks (SI.empty) in
		SI.iter (fun n -> gratsnest#updateTracks ~final:true n !gtracks ) worknets;
		render togl nulfun;
	in
	let switchSelectTrack ev = 
		gcurspos := calcCursPos ev !gpan false; (* don't update the list of hit modules .. rollover for that*)
		(* first see if nothing is selected .. *)
		let lay = try (List.find (fun t -> t#getHit()) !gtracks)#getLayer ()
			with _ -> ( 
				(* look at the modules *)
				let ll,_ = List.fold_left (fun d m-> 
					List.fold_left (fun (lay,siz) p -> 
						let bbx = p#getBBX() in
						if bbxInside bbx !gcurspos then (
							printf "inside pad\n%!"; 
							let psiz = bbxSize bbx in
							if  psiz < siz then (p#getLayer(),psiz)
							else (lay,siz)
						) else (lay,siz)
					) d (m#getPads ())
				) (!glayer,1e24) !gmodules in
				ll
			) in
		changelayercallback (layer_to_string lay); 
		(* 
		if (List.exists (fun t -> t#getHit()) !gtracks) then (
			(* then we want to switch layers to whichever would hit the smallest track *) 
			let layer,area = List.fold_left (fun (dlayer,darea) t -> 
				if t#touch !gcurspos then (
					let w = t#getWidth() /. 2.0 in
					let l = t#getLength() in
					let area = 2.0 *. 3.1415926 *. w *. w +. 2.0 *. l *. w in
					if area < darea then (t#getLayer(),area) 
					else (dlayer,darea) 
				) else (dlayer,darea) 
			) (!glayer,1e24) !gtracks in
			(* try looking for the pads too (if it's smaller)*)
			let layer2,area2 = List.fold_left (fun d m-> 
				List.fold_left (fun (lay,siz) p -> 
					let bbx = p#getBBX() in
					if bbxInside bbx !gcurspos then (
						printf "inside pad\n%!"; 
						let psiz = bbxSize bbx in
						if  psiz < siz then (p#getLayer(),psiz)
						else (lay,siz)
					) else (lay,siz)
				) d (m#getPads ())
			) (layer,area) !gmodules in
			printf "layer2 %d\n%!" layer2; 
			if area2 > 1e20 then (
				(* raise the oldest one then *)
				let lay2 = List.find (fun lay -> List.exists ((=) lay) !glayerPresent 
					&& lay <> (string_to_layer "Drawings")) (* don't edit drawings here.. *)
					(List.rev !glayerZlist) in
				changelayercallback (layer_to_string lay2); 
			) else changelayercallback (layer_to_string layer2);
		); *)
		(* now that we've changed layers, update the hit / track size / via size accordingly *)
		gcurspos := calcCursPos ev !gpan true; 
		let width = List.fold_left (fun default track -> 
			if track#getHit() && track#getType() = Track_Track then 
				track#getWidth() else default
		) !gtrackwidth !gtracks in
		if width <> !gtrackwidth then (
			gtrackwidth := width; 
			printf "track width updated to %s\n%!" (tomm width) ;
		); 
		(* do the same for the vias *)
		(* might be cool if you could copy any through-hole pad.. *)
		let viadefault = (!gviapad, !gviadrill) in
		let width,drill = List.fold_left (fun default track -> 
			if track#getHit() && track#getType() = Track_Via then 
				(track#getWidth(), track#getDrill()) else default
		) viadefault !gtracks in 
		if (width,drill) <> viadefault then (
			gviapad := width; 
			gviadrill := drill; 
			printf "via pad updated to %s\n%!" (tomm width) ;
			printf "via drill updated to %s\n%!" (tomm drill) ;
		); 
	in
	let doToggleShow _ = 
		if !gmode = Mode_MoveText then (
			List.iter (fun m -> m#toggleTextShow()) !gmodules ; 
			render togl nulfun;
		)
	in
	let center_found (x,y) = 
		gpan := -1.0 *. x , -1.0 *. y ; 
		render togl nulfun;
	in
	(* bindings! *)
	bindMouseMoveModule () ; (*default is to move modules *)
	Mouse.bindMove 2757 top ~action:updatecurspos ; (* default move action is to simply watch the cursor *)
	Mouse.bindDefaultMove updatecurspos ; 
	bindMouseSelect () ; (* bind the shift keys to selection *)
	(* the strings in the bindings are from X11's keysymdef.h ... *)
	bind ~events:[`KeyPressDetail("r")] ~action:doRotate top ;
	bind ~events:[`Modified([`Shift], `KeyPressDetail"R")] ~action:doRotate top ;
	bind ~events:[`ButtonPressDetail(2)] ~fields:[`MouseX; `MouseY] 
		~action:(fun ev -> 
			if !gmode = Mode_AddTrack || !gmode = Mode_MoveTrack then 
				switchSelectTrack ev
			else doRotate ev) top ; (* middle mouse button rotates, or selectsswitches *)
			

	bind ~events:[`Modified([`Shift], `KeyPressDetail"X")] ~action:(fun ev -> doMirror ev true) top;
	bind ~events:[`Modified([`Shift], `KeyPressDetail"Y")] ~action:(fun ev -> doMirror ev false) top;
	bind ~events:[`KeyPressDetail("Page_Up")] ~action:(fun _-> changelayercallback "Bot";) top;  
	bind ~events:[`KeyPressDetail("Page_Down")] ~action:(fun _ -> changelayercallback "Top";) top;  
	bind ~events:[`KeyPressDetail("F5")] ~action:(fun _ -> changelayercallback "L1";) top;  
	bind ~events:[`KeyPressDetail("F6")] ~action:(fun _ -> changelayercallback "L2";) top;  
	bind ~events:[`KeyPressDetail("F7")] ~action:(fun _ -> changelayercallback "L3";) top;  
	bind ~events:[`KeyPressDetail("F8")] ~action:(fun _ -> changelayercallback "L4";) top;   
	bind ~events:[`KeyPressDetail("m")] ~action:(fun _ -> updateMode "move module";) top;  
	bind ~events:[`KeyPressDetail("t")] ~action:(fun _ -> updateMode "move track";) top; 
	bind ~events:[`KeyPressDetail("a")] ~action:(fun _ -> updateMode "add track";) top; 
	bind ~events:[`KeyPressDetail("x")] ~action:(fun _ -> updateMode "move text";) top; 
	bind ~events:[`KeyPressDetail("space")] ~action:(fun _ -> 
		if !gmode <> Mode_MoveTrack then (updateMode "move track")
		else(if !gmode = Mode_MoveTrack then (updateMode "add track"))) top; 
	(*bind ~events:[`KeyPressDetail(" ")] ~fields:[`MouseX; `MouseY] ~action:switchSelectTrack top; this is bad, control toggles it too.*)
	bind ~events:[`Modified([`Control], `KeyPressDetail"t")] ~action:tracksFun top; 
	bind ~events:[`Modified([`Control], `KeyPressDetail"v")] ~action:viasFun top; 
	bind ~events:[`Modified([`Control], `KeyPressDetail"s")] ~action:(fun _ -> saveall !gfname; ) top; 
	bind ~events:[`Modified([`Control], `KeyPressDetail"o")] ~action:(fun _ -> openCommand ()) top; 
	bind ~events:[`Modified([`Control], `KeyPressDetail"f")] ~action:
		(fun _ -> Find.find_dlg top !gmodules center_found) top ; 
	bind ~events:[`KeyPressDetail("F3")] ~action:(fun _ -> Find.find_next center_found;) top;  
	bind ~events:[`KeyPressDetail("h")] ~action:doToggleShow top ;
	bind ~events:[`KeyPressDetail("Escape")] ~action:(fun _ -> Mouse.releaseMove 2959 top) top; 
	bind ~events:[`KeyPressDetail("o")] ~action:(fun _ -> ggridorigin := !gsnapped; render togl nulfun) top; 
	bind ~events:[`Modified([`Shift],`KeyPressDetail"O")] ~action:(fun _ -> ggridorigin := (0.0,0.0); render togl nulfun) top; 
	bind ~events:[`KeyPressDetail("BackSpace")] ~action:
		(fun _ -> (* remove any tracks that were hit *)
			let track,found = try 
				( List.find (fun t -> t#getHit ()) !gtracks ), true
				with Not_found -> (List.hd !gtracks), false
			in
			if found then (
				let nn = track#getNet () in
				gtracks := List.filter (fun t -> not (t#getHit () ) ) !gtracks ; (*so easy :) *)
				gratsnest#updateTracks nn !gtracks ; 
			) ; 
			(* and also zone corners *)
			let zones = List.filter (fun z -> z#getHit ()) !gzones in
			List.iter (fun z -> z#delete ()) zones ; 
			render togl nulfun; 
		) top ; 
	bind ~events:[`KeyPressDetail("Delete")] ~action:
		(fun _ -> (* remove any tracks that were hit, AND ones that they connect to*)
			let track,found = try 
				( List.find (fun t -> t#getHit ()) !gtracks ), true
				with Not_found -> (List.hd !gtracks), false
			in
			let rec trackdelete tracks = 
				let tracks2 = ref [] in
				List.iter (fun t1 -> 
					let st = t1#getStart () in
					let en = t1#getEnd () in
					let layer = t1#getLayer() in
					let tvia = t1#getType() = Track_Via in
					let bbx = t1#getDrcBBX () in
					let w = t1#getWidth() *. 0.5 in
					List.iter (fun t2 -> 
						if bbxIntersect bbx (t2#getDrcBBX()) then (
							if t1 != t2 then (
								if t2#getLayer() = layer || t2#getType() = Track_Via || tvia then (
									let w2 = t2#getWidth() *. 0.5 in
									let dd = (w +. w2) *. (w +. w2) in
									if Pts2.distance2 st (t2#getStart()) < dd ||
										Pts2.distance2 st (t2#getEnd()) < dd ||
										Pts2.distance2 en (t2#getStart()) < dd ||
										Pts2.distance2 en (t2#getEnd()) < dd  then (
										gtracks := List.filter (fun t -> t <> t2) !gtracks ;
										tracks2 := (t2 :: !tracks2); 
									); 
								); 
							); 
						); 
					) !gtracks ; 
				) tracks ; 
				if(List.length !tracks2) > 0 then trackdelete !tracks2
			in
			
			if found then (
				let nn = track#getNet () in
				gtracks := List.filter (fun t -> t <> track) !gtracks ;
				trackdelete [track]; 
				gratsnest#updateTracks nn !gtracks ; 
				render togl nulfun; 
			)
		) top ; 
	bind ~events:[`KeyPressDetail("b")] ~fields:[`MouseX; `MouseY] ~action:
		(* break track under cursor *)
		(* breaking a track does not effect DRC, so don't bother testing *)
		(fun ev -> 
			ignore( calcCursPos ev !gpan true ); 
			let midpoint = !gsnapped in
			let track, found = try 
				(List.find (fun t -> t#getHit() ) !gtracks),true
				with Not_found -> (List.hd !gtracks), false
			in
			if found then (
				print_endline "breaking track...";
				let track2 = Oo.copy track in
				track2#setU 0.5 ; 
				track2#newGrfx (); (* need to make a new graphics b/c the track is copied. *)
				track#setEnd midpoint ; 
				track2#setStart midpoint ; 
				gtracks := (track2 :: !gtracks); 
				track#update () ; 
				track2#update () ; 
				gratsnest#updateTracks (track#getNet()) !gtracks ; 
				render togl nulfun; 
				updateMode "move track"; (*you always want to move a track after breaking it *)
			) else (
				if List.exists (fun z -> z#getHit () ) !gzones then (
					(* try adding a point to a zone *)
					let zones = List.filter (fun z -> z#getHit()) !gzones in
					List.iter (fun z -> z#add !gcurspos) zones ;
					render togl nulfun; 
					updateMode "move track";
				)
			)
		) top; 
	bind ~events:[`KeyPressDetail("f")] ~fields:[`MouseX; `MouseY] ~action:
		(* fuse two tracks together.  DRC is effected, so test before 
		commiting changes *)
		(fun ev -> 
			ignore(  calcCursPos ev !gpan true ); 
			if !gmode = Mode_AddTrack || !gmode = Mode_MoveTrack then (
				let track, found = try 
					(List.find (fun t -> t#getHit() ) !gtracks),true
					with Not_found -> (List.hd !gtracks), false
				in
				if found then (
					track#setHit false; 
					let track2, found2 = try 
						(List.find (fun t -> t#getHit() ) !gtracks),true
						with Not_found -> (List.hd !gtracks), false
					in
					if found2 then (
						(* need to determine the common point *)
						(* make it the end of track & start of track2 *)
						let st1 = track#getStart() in
						let en1 = track#getEnd() in
						let st2 = track2#getStart() in
						let en2 = track2#getEnd() in
						let w = ((track#getWidth()) +. (track2#getWidth())) *. 0.5 in
						let st, _, en, touch = 
							if Pts2.distance st1 st2 < w then (
								(* the starts are touching *)
								en1, st1, en2, true
							) else if Pts2.distance st1 en2 < w then (
								en1, st1, st2, true
							) else if Pts2.distance en1 st2 < w then (
								st1, en1, en2, true
							) else if Pts2.distance en1 en2 < w then (
								st1, en1, st2, true
							) else (0. , 0.),(0. , 0.),(0. , 0.),false
						in
						if touch then (
							(*try it! (only move the first one)*)
							track#setStart st; 
							track#setEnd en; 
							if testdrc2 track !gtracks !gmodules then (
								(* it didn't work, revert *)
								track#setStart st1; 
								track#setEnd en1; 
							)else (
								(* it did work! *)
								(* remove the other track *)
								gtracks := List.filter (fun t-> t != track2) !gtracks; 
								track#update(); 
							);
							render togl nulfun; 
						); 
					); 
				); 
			) else (
				(* flip a module *)
				let m = try Some (List.find (fun m->m#getHit()) !gmodules)
					with _ -> None in
				match m with 
					| Some mm -> (
						mm#flip (); 
						render togl nulfun; 
					)
					| None -> ()
			)
		) top; 
	bind ~events:[`KeyPressDetail("w")] ~fields:[`MouseX; `MouseY] ~action:
		(* change width of currently hit tracks *)
		(fun ev -> 
			ignore(  calcCursPos ev !gpan true ); 
			if !gmode = Mode_AddTrack || !gmode = Mode_MoveTrack then (
				let tracks = List.filter (fun t -> t#getHit() ) !gtracks in
				List.iter (fun t -> t#setWidth !gtrackwidth ) tracks; 
				List.iter (fun t -> t#update () ) tracks; 
				render togl nulfun; 
			); 
		) top; 
	bind ~events:[`KeyPressDetail("KP_Enter")] ~fields:[`MouseX; `MouseY] ~action:
	(fun ev -> (* cross-probe *)
		updatecurspos ev ; 
		List.iter (fun m-> m#crossprobe ())!gmodules; 
	) top; 
	let doZoom ev zm = 
		updatecurspos ev ; 
		let l = Pts2.add !gpan !gcurspos in
		let s = 1. /. zm -. 1. in
		gpan := Pts2.add (Pts2.scl l s) !gpan; 
		gzoom := !gzoom *. zm ; 
		render togl nulfun
	in
	bind ~events:[`ButtonPressDetail(5)] 
		~fields:[`MouseX; `MouseY] ~action:(fun ev -> doZoom ev (1. /. 1.2) ) top; 
	bind ~events:[`ButtonPressDetail(4)] 
		~fields:[`MouseX; `MouseY] ~action:(fun ev -> doZoom ev 1.2) top;
	(* pull function *)
	bind ~events:[`KeyPressDetail("Control_R")] ~fields:[`MouseX; `MouseY] ~action:
	(fun ev -> 
		updatecurspos ev ; 
		if not !gpulling && !gmode = Mode_MoveModule then (
			gpulling := true;
			let magnets = List.filter (fun m-> m#getHit()) !gmodules in
			if (List.length magnets) > 0 then (
				gratsnest#clearAll (); (* no need for the ratsnest while this is going on*)
				let modules = Pull.filtermods !gmodules !gtracks in
				let magnet = List.hd magnets in
				printf "Pull.filtermods: output %d\n%!" (List.length modules);
				let rec loop () = 
					if !gpulling then (
						Pull.pull magnet modules;
						render togl nulfun ;
						ignore(Timer.add ~ms:20 ~callback:loop)
					);
				in
				loop ()
			); 
		) ; 
	) top; 
	bind ~events:[`KeyReleaseDetail("Control_R")] ~fields:[`MouseX; `MouseY] ~action:
	(fun ev -> 
		printf "p release!\n%!"; 
		updatecurspos ev ; 
		gpulling := false; 
		Pull.stop !gmodules (fun () -> render togl nulfun); 
		redoRatNest (); 
		render togl nulfun ;
	) top; 
	(* display them *)
	pack ~side:`Left 
		[Tk.coe fileb; Tk.coe optionb; Tk.coe viab ; Tk.coe trackb ; 
		Tk.coe gridb ;  Tk.coe cellb; Tk.coe infodisp; 
		Tk.coe cursorbox ; Tk.coe snapbox ; Tk.coe originbox; Tk.coe mframe; Tk.coe layerframe];
	place ~height:32 ~x:0 ~y:0 ~relwidth:1.0 menubar ; 
	(* return a function pointer for changing the info text *)
	;;

let reshape togl = 
	Togl.make_current togl ; 
	let w = (float_of_int (Togl.width togl))  in
	let h = (float_of_int (Togl.height togl))  in
	let ar = w /. h in
	gwindowsize := (Togl.width togl) , (Togl.height togl);
	GlDraw.viewport ~x:0 ~y:0 ~w:(Togl.width togl) ~h:(Togl.height togl);
	GlMat.mode `projection ; 
	GlMat.load_identity () ; 
	GlMat.ortho ~x:(-1. *. ar, 1. *. ar) ~y:(-1. ,1. ) ~z:(-1., 1.);
	GlMat.mode `modelview ; 
	GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
	GlArray.enable `vertex;
	render togl nulfun; 
	;;
	
let _ = 
	(* read in the preferences *)
	let geo = ref "" in
	let fil = getPrefsFile () in
	let cin = try Some (open_in fil)
		with _ -> None
	in
	let extract line pat var = 
		if Pcre.pmatch ~pat line then (
			var := Pcre.pmatch ~pat:"true" line 
		)
	in
	let fextract line vname present = 
		if Pcre.pmatch ~pat:vname line then (
			let pat = String.concat vname [" ([\d\-\.e]+)"] in
			float_of_string (Pcre.extract ~pat line).(1) 
		) else present
	in
	(
	match cin with 
		| Some chan -> (
			let okok = ref true in
			while !okok do (
				let line,ok = try (input_line chan) ^ "  ",true
					with End_of_file -> "", false
				in
				if ok then (
					if String.get line 0 = '/' then (
						(* printf " pref line: %s\\n \n%!" line ;  *)
						let space = String.index line ' ' in
						(* printf "space @ %d\n%!" space ; *)
						let stripSpace x = 
							Pcre.qreplace ~pat:"\s" ~templ:"" x 
						in
						let fname = stripSpace (String.sub line 0 space) in
						let sname = stripSpace 
							(String.sub line (space+1) ((String.length line) - space-1)) in
						printf " pref fname:%s\\s sname:%s\\s\n%!" fname sname ;
						gfilelist := ((fname, sname) :: !gfilelist); 
						okok := true; 
					) ;
					if Pcre.pmatch ~pat:"Geometry" line then (
						geo := (Pcre.extract ~pat:"Geometry ([\w\d\+]+)" line).(1); 
					) ; 
					(* go through the list of preferences. *)
					extract line "genabledepth" genabledepth;
					extract line "groute135" groute135;
					extract line "gtrackKeepSlope" gtrackKeepSlope;
					extract line "gdrawtracks" gdrawtracks; 
					extract line "gdrawmods" gdrawmods; 
					extract line "gdrawzones" gdrawzones; 
					extract line "gdrawratsnest" gdrawratsnest;
					extract line "gdrawratsnest" gdrawratsnest;
					extract line "gshowPadNumbers" gshowPadNumbers; 
					extract line "gshowHiddenText" gshowHiddenText; 
					extract line "ggridDraw" ggridDraw; 
					extract line "ggridSnap" ggridSnap; 
					extract line "gsnapTracksToGrid" gsnapTracksToGrid; 
					extract line "gdrawText" gdrawText;
					extract line "gdosnap" gdosnap;
					extract line "gTrackEndpointOnly" gTrackEndpointOnly;
					extract line "gTrackDragConnected" gTrackDragConnected; 
					extract line "gtrackDRC" gtrackDRC; 
					ggrid.(0) <- fextract line "ggrid0" ggrid.(0);
					ggrid.(1) <- fextract line "ggrid1" ggrid.(1); 
					ggrid.(2) <- fextract line "ggrid2" ggrid.(2); 
					ggrid.(3) <- fextract line "ggrid3" ggrid.(3); 
					ggrid.(4) <- fextract line "ggrid4" ggrid.(4); 
				) else (
					okok := false; 
				)
			) done ; 
		);
		| None -> ()
	); 
	gfilelist := List.rev !gfilelist ;
	let top = openTk () in
	(*Wm.aspect_set top ~minnum:4 ~mindenom:3 ~maxnum:4 ~maxdenom:3;*)
	Wm.title_set top "Kicad Ocaml";
	if !geo <> "" then Wm.geometry_set top !geo ; 
	printf "Geometry set at %s ; delete line in %s to reset\n%!" !geo fil; 
	let togl = Togl.create ~width:1024 ~height:700 
		~rgba:true ~double:true ~depth:true top in
	Togl.display_func togl ~cb:(fun () -> render togl nulfun);
	Togl.reshape_func togl ~cb:(fun () -> reshape togl );
	pack ~fill:`Both ~expand:true [togl];
	makemenu top togl !gfilelist;
	
	(* setup the netnumber -> name lookup function *) 
	let netlookup n =  
		try (List.find (fun nn -> nn#getNet() = n) !gnets)#getName()
		with Not_found -> "unkown"
	in
	let invNetLookup n =
		try (List.find (fun nn -> nn#getName () = n) !gnets)#getNet()
		with Not_found -> 0
	in
	glookupnet := netlookup ; 
	gInvLookupNet := invNetLookup ; 
	
	(*set up the layer stack *)
	for i = 0 to 31 do (
		glayerZlist := (i :: !glayerZlist); 
	) done ; 
	glayerZlist := List.rev !glayerZlist ;
	update_layer_z () ; 	
	
	(* callback function to refresh in case of cross-probe *)
	(* open up a unix socket to cross-probe eeschema *)
	let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	let sockin = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	(
	try
		Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1",4243)); 
			print_endline "cross-probing socket connected to eeschema" ; 
		gcrossprobe := ( fun s -> 
			if !gcrossProbeTX then (
				ignore(try Unix.send sock s 0 (String.length s) [] with _ -> 0; )
			) ; 
		) ; 
		gclosesock := (fun () -> 
			print_endline "closing cross-probing socket"; 
			Unix.shutdown sock Unix.SHUTDOWN_ALL; ) ; 
	with 
		| Unix.Unix_error(Unix.ECONNREFUSED, "connect", "") -> 
			print_endline "could not open a unix socket to eeschema for cross-probing" ; 
	(* i guess we should listen, too *)
	); 
	(
	try 
		Unix.setsockopt sockin Unix.SO_REUSEADDR true ;
		Unix.bind sockin (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1",4242)); 
		Unix.listen sockin 1 ; 
		Unix.set_nonblock sockin ;
		let clients = ref (FDSet.singleton sockin) in
		let crossprobein () =
			let readstr ss = 
				let arr,fnd = try (Pcre.extract ~pat:"\$PART: ([\w\d]+)" ss), true 
					with Not_found -> [||],false in
				let pinarr, pinfind = try (Pcre.extract ~pat:"\$PIN: ([\w\d]+)" ss),true
					with _ -> [||], false in
				if fnd then (
					let part = arr.(1) in
					print_endline part ; 
					if !gcrossProbeRX then (
						List.iter (fun (m:Mod.pcb_module) -> 
							let txt = m#getRef() in
							if( String.compare txt part ) = 0 then (
								let ctr = if pinfind then (
									let pin = pinarr.(1) in
									try (List.find (fun p -> (p#getPadName ()) = pin) 
										(m#getPads()))#getCenter()
										with _ -> m#getCenter false
								) else (
									m#getCenter false
								) in
								gpan := Pts2.scl ctr (-1.); 
								gcurspos := ctr ; 
								print_endline "found module! " ; 
								render togl nulfun;
							) ; 
						) !gmodules ; 
					) ; 
				); 
			in
			let s = "                                                                                                                   " in
			let sl = String.length s in
			let (can_read, _, _) = Unix.select (FDSet.elements !clients) [] [] 0.00 in
			List.iter ( fun client -> 
				if client = sockin then (
					let (client_sock, _ ) = Unix.accept sockin in
					print_endline "accepted socket connection"; 
					Unix.set_nonblock client_sock;
					clients := FDSet.add client_sock !clients;
					let rcvlen,got = try Unix.read client_sock s 0 sl , true
						with Unix.Unix_error(_,"read",_) -> 0, false in
					if got then (
						let ss = String.sub s 0 rcvlen in
						printf "%s\n" ss; 
						readstr ss ;
					); 
				) else (
					(* read .. *)
					let chars_read =
						try
						Some (Unix.read client s 0 sl)
						with Unix.Unix_error (error, _, _) ->
						prerr_endline (Unix.error_message error);
						None 
					in
					match chars_read with
						| None | Some 0 ->
						(* This would be the end of file, so close the client *)
							Unix.close client ; 
							clients := FDSet.remove client !clients;
							print_endline "socket disconnected"
						| Some chars_read -> (
							let ss = String.sub s 0 chars_read in
							readstr ss ;
						)
				)
			) can_read ; 
		in
		(*check for cross-probe event every 50 ms *)
		Togl.timer_func ~ms:50 ~cb:crossprobein ; 
		(* ignore(Thread.create (fun (soc,modules) ->  threads in ocaml don't seem to work well ?
		polling turned out to work much more quickly using the Unix.select function. *)
		print_endline "cross-probing socket listening for eeschema" ; 
		gclosesock := (fun () -> 
			print_endline "closing cross-probing socket"; 
			Unix.shutdown sockin Unix.SHUTDOWN_ALL; 
			(*Thread.join listenthread ;*) ); 
	with 
		| Unix.Unix_error(Unix.ECONNREFUSED, "connect", "") -> 
			print_endline "could not open a unix socket to listen for eeschema" ; 
		| Unix.Unix_error(Unix.EADDRINUSE, "bind", "") -> 
			print_endline "could not open a unix socket to listen for eeschema: port already used (probably by pcbnew)" ; 
	); 
	(* this for testing (so we can get a backtrace... *) 
	(* use ocamlrun -b *)
	(* openFile top "/home/tlh24/sewing_machine/layout3/layout3.brd"; *)
	
 	Printexc.record_backtrace true ; (* ocaml 3.11 *)
 	Printexc.print_backtrace stdout ; (* ocaml 3.11 *)
 	Printexc.print mainLoop () ;
	;;
		
	
