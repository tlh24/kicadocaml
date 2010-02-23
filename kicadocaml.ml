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
					let l = (Pcre.extract ~pat:"(\d+)" !line).(1) in
					gclearance := fois (ios l) ; 
					print_endline ( "global track clearance = " ^ sof !gclearance ); 
				); 
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

class pcb_drawsegment = 
object
	val mutable m_shape = 0
	val mutable m_stx = 0
	val mutable m_sty = 0
	val mutable m_enx = 0
	val mutable m_eny = 0
	val mutable m_width = 0
	val mutable m_layer = 0
	val mutable m_type = 0
	val mutable m_angle = 0
	val mutable m_g = new grfx
	method getLayer () = m_layer ; 
	method update () = (
		m_g#updateLayer false m_layer ; (* this also sets z, needed for vertices *)
		m_g#empty () ;
		m_g#makeTrackInt m_stx m_sty m_enx m_eny m_width ; 
	)
	method draw bbox = (
		m_g#draw ~hit:false bbox ; 
	)
	method read ic = (
		let line = input_line2 ic in
		let sp = Pcre.extract ~pat:"Po (\d+) ([\d-]+) ([\d-]+) ([\d-]+) ([\d-]+) (\d+)" line in
		m_shape <- ios sp.(1) ; 
		m_stx <- ios sp.(2) ; 
		m_sty <- ios sp.(3) ; 
		m_enx <- ios sp.(4) ; 
		m_eny <- ios sp.(5) ; 
		m_width <- ios sp.(6) ; 
		let line2 = input_line2 ic in
		let sp2 = Pcre.extract ~pat:"De (\d+) (\d+) (\d+)" line2 in
		m_layer <- ios sp2.(1) ;
		m_type <- ios sp2.(2) ;
		m_angle <- ios sp2.(3) ;
		(*read the $EndDRAWSEGMENT line *)
		let d = ref "" in
		d := input_line2 ic ; 
	)
	method save oc = (
		fprintf oc "$DRAWSEGMENT\n" ; 
		fprintf oc "Po %d %d %d %d %d %d\n" 
			m_shape m_stx m_sty m_enx m_eny m_width ; 
		fprintf oc "De %d %d %d 0 0\n" 
			m_layer m_type m_angle ; 
		fprintf oc "$EndDRAWSEGMENT\n" ; 
	)
end;; 

let gnets = ref [] (*this must be a reference, as we are updating! *)
let gtracks = ref []
let gmodules = ref []
let gzones = ref []
let gdrawsegments = ref []
let ggeneric = ref []
let gzoom = ref 1.0
let gdrag = ref (0.0, 0.0)
let goldpan = ref (0.0, 0.0)
let ghithold = ref false
let genabledepth = ref true 
let file_header  = ref "" 
let gratsnest = new ratsNest
let gclosesock = ref (fun () -> print_endline "nothing" ; )
let gbutton3pressed = ref false
let gbutton1pressed = ref false
let gtrackwidth = ref 0.01 
let gtrackwidthlist = ref [] 
let gviasizelist = ref [] 
let gviapad = ref 0.047
let gviadrill = ref 0.015
let gfname = ref "" 
let groute135 = ref true
let gtrackKeepSlope = ref true
let gfilelist = ref [] 
let gdrawtracks = ref true
let gdrawzones = ref true
let gdrawratsnest = ref true
let gpushrouting = ref false
let gcrossProbeTX = ref true
let gcrossProbeRX = ref true
let gTrackAdd = ref (fun _ -> ())
let gViaAdd = ref (fun _ _ -> ())

let readlines ic =
	(*remove the old modules *)
	gnets := [] ; 
	gmodules := [] ; 
	gdrawsegments := [] ; 
	gtracks := [] ; 
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
	(* read the header *)
	file_header := input_line2 ic ; 
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
					let drawseg = new pcb_drawsegment in
					drawseg#read ic; 
					gdrawsegments := drawseg :: !gdrawsegments; 
				)
				| "$TRACK" -> (
					readtracks ic ; 
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
	print_endline ( "number of nets:" ^ string_of_int(List.length !gnets) ) ; 
	print_endline ( "number of modules:" ^ string_of_int(List.length !gmodules) ) ; 
	print_endline ( "number of tracks:" ^ string_of_int(List.length !gtracks) ) ; 
	print_endline ( "number of drawsegments:" ^ string_of_int(List.length !gdrawsegments) ) ; 
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
	List.iter sv (List.rev !gdrawsegments ); 
	fprintf oc "$TRACK\n" ;
	List.iter sv (List.rev  !gtracks ); 
	fprintf oc "$EndTRACK\n" ;
	List.iter sv zones;
	List.iter sv (List.rev !gzones ); 
	fprintf oc "$EndBOARD\n" ;
	flush oc ; (* this is necessary!!! *)
	(* addToFilelist filename ; *)
	close_out_noerr oc ; 
	;;
(* UI stuff *)
let abouttext =
"Kicad PCB editor\n" ^
"written in Ocaml (yay) \n" ^
"by Tim Hanson, sideskate@gmail.com \n" ^
"for use with the Kicad suite of programs.\n" ^
"   (not meant to work wholly by itself -- netlist/lib load incomplete) \n\n" ^
"Relevant hotkeys / commands: \n" ^ 
" right mouse button --- pan \n" ^ 
" scroll wheel --- zoom \n" ^
" middle mouse button --- \n" ^
"    * in move module & move text mode, rotate; \n" ^
"    * in add or move track mode, select layer and update width\n"^
"       (based on the track currently under the cursor).\n"^
" .... \n" ^
" a - add track \n" ^
" Ctrl-T - select track width \n" ^ 
" b - break track under cursor \n" ^
" e - edit (only text edit mplemented srry)\n" ^
" f - In track editing/adding mode:\n\tfuse (join) tracks - two tracks must be highlighted \n" ^
"     In module moving mode:\n\tflip a module from the top to the bottom of the board, and vice-versa\n"^
" h - hide / unhide selected text in move text mode\n" ^
" m - move module \n" ^
" t - move track \n" ^
" v - insert a via (in add tracks mode) \n" ^
" v - edit module value (in move module mode)\n"^
" Ctrl-V - select via size \n" ^
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
			Grid.draw screenbbx g !alpha; 
			if !genabledepth then alpha := !alpha +. 0.04; 
			GlMat.pop() ; 
		) ggrid ;
		(* if the depth buffer is disabled we don't need to change the alpha level. *)
	) ; 
	!ginfodisp( "" ) ;
	let more = cb () in
	if not more then (
		List.iter (fun m -> 
			m#draw screenbbx
			) !gmodules ; 
		(* draw the tracks back to front so that the alpha-blending makes sense *)
		(* this requires slightly more iteration but.. eh well, it is the correct way to do it*)
		let layerZlist = List.filter (fun a -> glayerEn.(a)) (List.rev !glayerZlist) in
		let lastLayer = try List.hd (List.rev layerZlist) with _ -> 0 in
		List.iter ( fun lay -> 
			let lastiter = (lay = lastLayer) in
			GlMat.push () ; 
			GlMat.translate ~x:0. ~y:0. ~z:glayerZ.(lay) () ; 
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
						zon#draw () ; 
					); 
				) !gzones ;
			) ; 
			(* and for the drawsegments *)
			List.iter (fun seg -> 
				if seg#getLayer() = lay then (
					seg#draw screenbbx; 
				); 
			) !gdrawsegments ;
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
		(* let drawCursor (x,y) = 
			drawRect (x -. s,y -. s,x +. s,y +. s) 
		in *)
		GlDraw.color ~alpha:0.7 (match !gmode with
			| Mode_AddTrack -> (1.,0.80,0.22) (* orange *)
			| Mode_MoveTrack -> (1.,0., 0.2) (* red *)
			| Mode_MoveModule -> (0.55,0.16,1.) (* purple-blue *)
			| Mode_MoveText -> (0.22,0.77,1.) (* aqua *)
		); 
		drawCrosshairs !gcurspos ; 
		(* drawRing !gcurspos (0.17 *. 0.7) (0.2 *. 0.7); 
		GlDraw.color ~alpha:1. (1. , 1., 1. ); 
		drawCursor !gcurspos ; 
		GlDraw.color ~alpha:1. (0.4 , 1., 0.8 ); 
		drawCursor !gsnapped ; *)
		GlDraw.color ~alpha:0.5 (0.4 , 1., 0.8 ); 
		drawCrosshairs !gsnapped ; 
		(* draw crosshairs too ... may be useful! *)
		
		
		(* draw the selection box *)
		if bbxIntersect !gselectRect screenbbx then (
			GlDraw.color ~alpha:0.2 (0.8 , 0.4, 1.0 ); 
			drawRect !gselectRect ;
		) ; 
	);
	(*
	let pp = if !gbutton1pressed then 
		Pts2.add !gcurspos !gdrag 
		else !gcurspos
	in
	!gcursordisp "cursor" (fst pp) (snd pp) ;  *)
	(* more test code : 
	GlDraw.begins `quads ; 
	GlDraw.color ~alpha:0.5 (1. , 1. , 1.); 
	let x , y = !gpan in
	GlDraw.vertex3 ( x -. s , y -. s, 0.5) ; 
	GlDraw.vertex3 ( x -. s , y +. s, 0.5) ; 
	GlDraw.vertex3 ( x +. s , y +. s, 0.5) ; 
	GlDraw.vertex3 ( x +. s , y -. s, 0.5) ; 
	GlDraw.ends (); 
	*)
	GlMat.pop() ; 
	Gl.flush ();
	Togl.swap_buffers togl ; 
	(* Printexc.print_backtrace stdout ocaml 3.11 *)
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
	
(* open a file *)
let openFile top fname = 
	let ic = open_in fname in
	glayerPresent := [] ; (* clear the old layer list, this file may have different layers present *)
	readlines ic ; 
	(* also add the other known layers -- see kicad!*)
	glayerPresent := [20; 21;24] @ !glayerPresent; 
	(* update the enabled list ... *)
	for i = 0 to 31 do glayerEn.(i) <- false done;
	List.iter (fun u -> glayerEn.(u) <- true) !glayerPresent;  
	gfname := fname ; (* if we get this far, then we can assign it *)
	(* addToFilelist fname ; *)
	Wm.title_set top ("Kicad Ocaml " ^ fname );
	List.iter (fun m -> m#update()) !gmodules ; 
	List.iter (fun m -> m#update()) !gtracks ; 
	List.iter (fun z -> z#update ()) !gzones ; 
	List.iter (fun s -> s#update ()) !gdrawsegments ; 
	(* if the board was saved in pcbnew, then we need to propagate the netcodes to all tracks. *)
	(* otherwise, adding tracks to existing ones becomes impossible ... *)
	let trackzero = List.length (List.filter (fun t -> (t#getNet ()) <= 0) !gtracks) in
	let tracklen = List.length !gtracks in
	printf "number of read tracks: %d; number of tracks with net of zero %d \n%!" tracklen trackzero ; 
	
	if (foi trackzero) /. (foi tracklen) > 0.4 then (
		printf "It seems that this board was previously saved in PCBnew...\n%!"; 
		propagateNetcodes gmodules gtracks true false top (fun () -> ()) (fun () -> ()) (); 
	); 
	gratsnest#clearAll (); 
	gratsnest#make !gmodules !gtracks; 
	(* figure out the (approximate) center of the board *)
	let center = bbxCenter (List.fold_left 
		(fun b m -> bbxMerge b (m#getBBX false )) 
		((List.hd !gmodules)#getBBX false) !gmodules) in
	gpan := Pts2.scl center (-1.0) ; 
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
	
	linenum := 0 ; 
	let schfil = selectSch top fname in
	if testfile schfil then (
		printf "reading schematic ... %!"; 
		gschema#openFile schfil "00000000" "root" (ref []); 
		gschema#collapseAr ""; 
		printf " done.\n%!"; 
	) ;
	(* gschema#print "" ;*)
	;;

let makemenu top togl filelist = 
	(* create menu bar *)
	let menubar = Frame.create ~borderwidth:0 ~relief:`Raised top in
	let fileb = Menubutton.create ~text:"File" menubar
	and optionb = Menubutton.create ~text:"Options" menubar
	and viab = Menubutton.create ~text:"Via" menubar 
	and trackb = Menubutton.create ~text:"Track" menubar in
	let filemenu = Menu.create ~tearoff:false fileb
	and optionmenu = Menu.create ~tearoff:false optionb
	and viamenu = Menu.create ~tearoff:false viab 
	and trackmenu = Menu.create ~tearoff:false trackb in
	let infodisp = Text.create ~width:45 ~height:2 menubar in
	let cursordisp = Text.create ~width:17 ~height:2 menubar in
	(* information display callback *)
	ginfodisp := ( fun s ->  
		Text.delete ~start:(`Linechar (1, 1) , [`Linestart]) ~stop:(`End, []) (infodisp) ; 
		Text.insert  ~index:(`End, []) ~text:s infodisp;
	 ); 
	 ginfodispappend := ( fun s ->  
		Text.insert  ~index:(`End, []) ~text:s infodisp;
	 ); 
	gcursordisp := (fun str x y -> 
		let s = Printf.sprintf "%s x %2.4f\n%s y %2.4f" str x str y in
		Text.delete ~start:(`Linechar (1, 1) , [`Linestart]) ~stop:(`End, []) (cursordisp) ; 
		Text.insert  ~index:(`End, []) ~text:s cursordisp; 
	) ; 
		
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
	
	let make_xyr () = 
		(* make a xyr file, which contains the x, y, and rotation for each component, 
		along with pin 1 absolute location. 
		positions are in inches, decimal *)
		printf "Warning! Only looking at the top of the board! \n%!" ; 
		let filetyp = [ {typename="x y rotation";extensions=[".xyr"];mactypes=[]} ] in
		let filename = (getSaveFile ~defaultextension:".xyr" ~filetypes:filetyp ~title:"save XYR" ()) in
		let oc = open_out filename in
		fprintf oc "#PCB parts location file\n"; 
		fprintf oc "#refdes	value	foot	x	y	r	p1x	p1y\n"; 
		List.iter ( fun m->
			m#update(); (* just in case .. *)
			let x,y = m#getCenter false in
			let r = (foi (m#getRot())) /. 10.0 in
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
		) (List.filter (fun m -> m#getLayer() = 15) !gmodules ); 
		(* also we should add in through-hole pads for alignment -- 
		this because the surface mount pads will be covered in paste when actually pnp.. *)
		let fidnum = ref 0 in
		List.iter (fun m -> 
			List.iter (fun p -> 
				if p#getShape () = Pad_Circle then (
					let x,y = p#getCenter () in
					if p#hasLayer (string_to_layer "Cop") && 
					   p#hasLayer (string_to_layer "Cmp") then (
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
		let count0603 mods = 
			List.fold_left (fun k m -> 
				if m#getLibRef () = "0603" then k+1 else k) 0 mods 
		in
		let orig = count0603 !gmodules in
		let mods,nets = read_netlist fname gmodules in
		gmodules := mods; 
		gnets := nets; 
		let neu = count0603 !gmodules in
		printf "** original: %d new: %d\n%!" orig neu ; 
		(* redo the rat's nest. *)
		propagateNetcodes gmodules gtracks true false top 
			(fun () -> render togl nulfun) 
			(fun () -> gratsnest#clearAll (); gratsnest#make !gmodules !gtracks) 
			(); 
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
			let prefs = "/home/" ^ (Unix.getlogin ()) ^ "/.kicadocaml" in
			let oc = open_out prefs in
			print_endline( "writing preferences " ^ prefs ) ; 
			List.iter (fun (board, schematic) -> 
				fprintf oc "%s %s\n" board schematic;
			) !gfilelist ; 
			fprintf oc "Geometry %s\n" geo; 
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
		let curlayer = Message.create ~width:60 ~text:"Current L." frame0 in
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
	let updateLayers layer b = 
		if glayerEn.(layer) != b  || b then (
			glayerEn.(layer) <- b ; 
			(* need to recreate the via color *)
			let copperlayers = List.filter ((>) 16) !glayerPresent in 
			(* the > seems backwards to me .. ehwell. *)
			(*print_endline "copperlayers:";
			List.iter (fun u -> print_endline (soi u)) copperlayers; *)
			gviaColor := layers_to_color copperlayers glayerEn ; 
			List.iter (fun m -> m#updateLayers() ) !gmodules ; 
			List.iter (fun m -> 
				m#setHit false; 
				if m#isVia() then m#updateColor () ; 
			) !gtracks ;
			render togl nulfun; 
		)
	in
	let raiseLayer lay = 
		if List.exists ((=) lay) !glayerPresent then (
			glayer := lay ; 
			glayerZlist := List.filter (fun l -> not (l = lay)) !glayerZlist ; (* remove it *)
			glayerZlist := (lay :: !glayerZlist); (* put layer at head *)
			update_layer_z () ; 
			updateLayers lay true; 
		) else ( print_endline "layer not present in board"; ) ; 
	in
	let (layerframe,changelayercallback) = makeLayerFrame 
		["Cop";"L1";"L2";"L3";"L4";"Cmp";"SS_Cop";"SS_Cmp";]
		["Cop";"L1";"L2";"L3";"L4";"Cmp";"SS_Cop";"SS_Cmp";"Drawings"]
		(fun s -> raiseLayer (string_to_layer s) )
		(fun s b ->
			let lay = string_to_layer s in
			if List.exists ((=) lay) !glayerPresent then (
				updateLayers lay b; 
			) else ( print_endline "layer not present in board"; ) ; 
		)
		top in
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
		if dosnap && nonemoving then (
			gsnapped := out ; 
			(* set the hit flags& get a netnum *)
			let (nn,hitsize2,hitz2,hitclear2) = List.fold_left (fun (netnum,hitsize,hitz,clearhit) m -> 
				m#hit out !ghithold onlyworknet netnum hitsize hitz clearhit
			) (worknet, 1e24, -2e2, (fun() -> ()) ) !gmodules 
			in
			(* printf "after mod: hitsize %f hitz %f\n%!" hitsize2 hitz2;*)
			let netn,_,_,_ = 
				if !gdrawtracks then (
					List.fold_left (fun (netn1,hitsize,hitz,hitclear) track -> 
						track#hit (out, onlyworknet, hitsize, hitz, hitclear, !ghithold, netn1)
					) (nn,hitsize2,(hitz2 -. 1.0),[hitclear2]) !gtracks 
					(* subtract one so that we can hit tracks if we don't hit a module *)
				) else nn,hitsize2,hitz2,[]
			in
			gcurnet := netn ; 
		); 
		out
	in
	
	let updatecurspos ev = 
		gcurspos :=  calcCursPos ev !gpan true; 
		!gcursordisp "cursor" (fst !gcurspos) (snd !gcurspos) ; 
		render togl nulfun;
	in
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
			if !workingnet > 0 then (
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
				ignore( List.fold_left (fun (nn,hitsize,hitz,clearhit) m -> 
					m#hit !gcurspos false false nn hitsize hitz clearhit
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
				if lay <> !glayer then changelayercallback (layer_to_string lay); 
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
				Mouse.bindMove top ~action:
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
						if testdrc2 track !gtracks !gmodules 
							|| testdrc2 track2 !gtracks !gmodules then (
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
				)
			); 
		) 
		~onRelease:
		(fun ev -> 
			!reenable () ;
			gbutton1pressed := false ;
			(* remove any zero-length tracks that may have been created in the process *)
			(* this may delete other tracks .. but ehwell !*)
			gtracks := List.filter (
				fun t -> t#manhattanLength() > 0. || t#getType() != Track_Track
			) !gtracks ; 
			gratsnest#updateTracks !workingnet !gtracks ; 
			Mouse.releaseMove top ; 
			updatecurspos ev ; 
		) ; 
	)in
	
	let bindMouseMoveTrack () = (
		gmode := Mode_MoveTrack ;
		bindVtoVia () ; 
		let workingnet = ref 0 in
		let tracks = ref [] in
		let startPoint = ref (0., 0.) in
		Mouse.releasePress top; 
		Mouse.bindPress top ~onPress:
		(fun ev -> 
			workingnet := !gcurnet ; 
			ignore(  calcCursPos ~worknet:!workingnet ~onlyworknet:true ev !gpan true ); 
			startPoint := !gsnapped ;
			tracks := List.filter (fun t -> t#getHit() ) !gtracks ; 
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
									if t#getType() = Track_Via then ( hitTrack t ); 
								) ; 
							) ; 
						) nettracks ; 
					)
				in
				List.iter hitTrack !tracks; 
				tracks := List.rev_append !addtracks !tracks ; 
				List.iter (fun t-> t#setHit true) !tracks ; 
				(* untracks := List.filter (fun t -> not (t#getHit())) !gtracks; *)
			) ; 
			let safemove = ref (0. , 0.) in 
			gbutton1pressed := true ; 
			Mouse.bindMove top ~action:
			(fun evinf ->
				ignore (calcCursPos ~worknet:!workingnet ~onlyworknet:true evinf !gpan true ); 
				gdrag :=  Pts2.sub !gsnapped !startPoint ; 
				!gcursordisp "d" (fst !gdrag) (snd !gdrag) ; 
				(* simple method: try moving the tracks; if there is an error, 
				snap back to last safe position *)
				List.iter (fun t -> t#move !gdrag ) !tracks ; 
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
				gcurnet := !workingnet;
				render togl nulfun;
			) ; 
		)
		~onRelease:
		(fun ev -> 
			gbutton1pressed := false ;
			Mouse.releaseMove top ; 
			(* need to update the moved tracks, if there were any *)
			List.iter (fun t-> 
				t#setMoving false; 
				t#setHit false;
			) !tracks ; 
			(* do this second so that all constraints have a chance to be applied *)
			List.iter (fun t -> t#clearConstraint (); t#update () ) !tracks ; 
			gratsnest#updateTracks !workingnet !gtracks ; 
			updatecurspos ev ; 
		) ; 
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
				Mouse.bindMove top ~action:
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
			Mouse.releaseMove top ; 
			(* need to update the moved tracks, if there were any *)
			List.iter (fun m -> m#setMoving false; ) !modules ; 
			updatecurspos evinf ; 
		) ; 
		(* unbind the v-key *)
		bind ~events:[`KeyPressDetail("v")] ~action:(fun _ -> ()) top; 
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
			Mouse.bindMove top ~action:
			(fun evinf ->
				let prescurspos = calcCursPos evinf !gpan true in
				(* try the move; if it causes any problems, back up. *)
				gdrag :=  Pts2.sub prescurspos !startPos ; 
				!gcursordisp "d" (fst !gdrag) (snd !gdrag) ; 
				(* may want to snap to grid here (snap the center of the modules) *)
				if !ggridSnap then (
					let grd = ggrid.(0) in
					List.iter (fun m -> 
						m#move (0.0 , 0.0);
						let ox, oy = m#getCenter true in
						m#move !gdrag ; (* move it, then snap *)
						let cx,cy = m#getCenter true in
						m#move ((snap cx grd) -. ox,(snap cy grd) -. oy); 
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
			Mouse.releaseMove top ; 
			(* need to update the moved tracks, if there were any *)
			List.iter (fun m -> m#setMoving false; m#setHit false) !modules ; 
			(* need to update the moved tracks, if there were any *)
			List.iter (fun t-> t#setMoving false; t#setHit false; ) !tracks ; 
			gratsnest#updateTracksAll !gtracks ; 
			updatecurspos evinf ; 
		) ; 
		(* bind the v and e keys. *)
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
		bind ~events:[`KeyPressDetail("e")] ~action:
			(fun _ -> modfind (fun m-> m#edit top)) top; 
		bind ~events:[`KeyPressDetail("v")] ~action:
			(fun _ -> modfind (fun m-> m#editValue top)) top;
	)in
	
	let bindMouseSelect () = (
		(* method: let the user drag the cursor to make a transparent rectangle
		when the mouse is released, select all modules & tracks that intersect 
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
					Mouse.bindMove top ~action:
					(fun evinf -> 
						let (px,py) = calcCursPos evinf !gpan true in
						gselectRect := ((fmin sx px),(fmin sy py),(fmax sx px),(fmax sy py)) ; 
						!gcursordisp "size" ((fmax sx px) -. (fmin sx px))  ((fmax sy py) -. (fmin sy py)); 
						(* iterate through the modules & update selected *)
						List.iter (fun m-> 
							if bbxIntersect (m#getBBX false) !gselectRect && m#getVisible() then 
								m#setHit true 
							else m#setHit false
						) !gmodules ; 
						(* same for the tracks *)
						List.iter (fun t-> 
							if t#selectHit !gselectRect then 
								t#setHit true
							else t#setHit false
						) !gtracks ; 
						render togl nulfun; 
					)
				)
			)
			~onRelease: 
			(fun evinf -> 
				gselectRect := (1e99,1e99,1e99,1e99) ; 
				Mouse.releaseMove top; 
				updatecurspos evinf ; 
				
				printf "please drag the selected modules .. (or release shift) \n%!" ; 
				printf "known bug: do not rotate and drag at the same time!\n%!" ; 
				let modules = List.filter (fun m-> m#getHit ()) !gmodules in
				let tracks = List.filter (fun t-> t#getHit ()) !gtracks in
				let worknets = List.fold_right (fun t -> SI.add (t#getNet () ))  tracks (SI.empty) in
				(* release the old press .. yes this is confusing now *)
				Mouse.releasePress top ; (* unbind this function even though it is executing! *)
				Mouse.bindPress top ~onPress:
				(fun evv -> 
					printf "dragging %d tracks and %d modules \n%!" 
						(List.length tracks) (List.length modules); 
					List.iter (fun m-> m#setMoving true ) modules ;
					List.iter (fun m-> m#setMoving true ) tracks ; 
					List.iter (fun m-> m#setU 0.5 ) tracks ; 
					SI.iter (fun n -> gratsnest#updateTracks ~final:false n !gtracks ) worknets; 
					gbutton1pressed := true ;
					let startPos = calcCursPos evv !gpan true in
					Mouse.bindMove top ~action:
					(fun evv -> 
						let prescurspos = calcCursPos evv !gpan true in
						let cx,cy =  Pts2.sub prescurspos startPos in
						if !ggridSnap then (
							(* move it an integer multiple of the smallest grid *)
							let grd = ggrid.(0) in
							gdrag := (snap cx grd),(snap cy grd) ; 
						) else (
							gdrag := cx,cy ; 
						); 
						!gcursordisp "d" (fst !gdrag) (snd !gdrag) ; 
						List.iter (fun m -> m#move !gdrag ) modules ; 
						List.iter (fun t -> t#move !gdrag ) tracks ; 
						List.iter (fun t-> t#update (); ) tracks ; 
						render togl nulfun; 
					); 
				) 
				~onRelease: 
				(fun evv ->
					gbutton1pressed := false ;
					List.iter (fun m-> m#setMoving false ) modules ; 
					List.iter (fun m-> m#setMoving false ) tracks ; 
					(* List.iter (fun m-> m#setHit false ) tracks ;  *)
					(* make a set of all the working nets, update them *)
					SI.iter (fun n -> gratsnest#updateTracks ~final:true n !gtracks ) worknets;
					Mouse.releaseMove top; 
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
	
	(* add in the sub-options menus .. *)
	let displaySub = Menu.create ~tearoff:false optionmenu in (* pass the contatining menu as the final argument *)
	let tracksSub = Menu.create ~tearoff:false optionmenu in
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
	addOption displaySub "draw ratsnest" (fun b -> gdrawratsnest := b) !gdrawratsnest ; 
	addOption displaySub "draw pad numbers" (fun b -> gshowPadNumbers := b) !gshowPadNumbers ; 
	
	Menu.add_command tracksSub ~label:"Tracks dialog (Ctrl-T)" ~command:tracksFun ; 
	addOption tracksSub "135 degree routing" ~bindkey:"5" (fun b -> 
			groute135 := b; 
			printf "135 deg routing %s\n%!" (if b then "on" else "off") ;
		) !groute135 ; 
	addOption tracksSub "when dragging tracks mantain slope" (fun b -> gtrackKeepSlope := b) !gtrackKeepSlope ; 
	addOption tracksSub "push routing" (fun b -> gpushrouting := b) !gpushrouting ; 
	
	Menu.add_command viasSub ~label:"Vias dialog (Ctrl-V)" ~command:viasFun ;
	Menu.add_command viasSub ~label:"Adjust via drill sizes" ~command:viaDrillAdjust ; 
	
	Menu.add_command textsSub ~label:"Adjust text sizes per module" ~command:textSizeAdjust ; 
	Menu.add_command textsSub ~label:"Adjust minimum text sizes" ~command:minTextSizeAdjust ; 
	Menu.add_command textsSub ~label:"Adjust text position from template" ~command:textPositionAdjust ; 
	
	addOption zonesSub "draw zones" (fun b -> gdrawzones := b) !gdrawzones ; 
	Menu.add_command zonesSub ~label:"Refill all zones"
		~command: (fun () -> List.iter (fun z -> z#fill !gtracks !gmodules) !gzones) ; 
	Menu.add_command zonesSub ~label:"Empty all zones"
		~command: (fun () -> List.iter (fun z -> z#empty ()) !gzones) ; 
	Menu.add_command zonesSub ~label:"Show zone fill algorithm window"
		~command: (fun () -> Mesh.makewindow top ) ; 
	
	Menu.add_command gridsSub ~label:"Grids ... " 
		~command:(fun _ -> Grid.dialog top (fun () -> render togl nulfun) );
	addOption gridsSub "grid draw" (fun b -> ggridDraw := b ) !ggridDraw; 
	addOption gridsSub "grid snap" (fun b -> ggridSnap := b ) !ggridSnap; 
	
	Menu.add_command ratsnestSub ~label:"Propagate netcodes to unconn. (nn=0) tracks" 
		~command:(propagateNetcodes gmodules gtracks false false top 
			(fun () -> render togl nulfun) redoRatNest ); 
	Menu.add_command ratsnestSub ~label:"Propagate netcodes to all tracks" 
		~command:(propagateNetcodes gmodules gtracks true false top 
			(fun () -> render togl nulfun) redoRatNest ); 
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
		~command:(propagateNetcodes gmodules gtracks true true top 
			(fun () -> render togl nulfun) redoRatNest );  
	Menu.add_command checkSub ~label:"Check DRC (track/pad copper spacing)"
		~command:(testdrcAll gtracks gmodules top (fun () -> render togl nulfun)); 

	addOption miscSub "cross probe transmit" (fun b -> gcrossProbeTX := b) !gcrossProbeTX ; 
	addOption miscSub "cross probe recieve" (fun b -> gcrossProbeRX := b) !gcrossProbeRX ; 

	Menu.add_command miscSub ~label:"Save bill of materials" 
		~command:makeBOM ; 
	Menu.add_command miscSub ~label:"Save XYR (part location) file" 
		~command:make_xyr ; 
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

	Menu.add_command optionmenu ~label:"About" ~command:(helpbox "about" abouttext top) ; 
	(* get the menus working *)
	Menubutton.configure fileb ~menu:filemenu;
	Menubutton.configure optionb ~menu:optionmenu;
	Menubutton.configure viab ~menu:viamenu;
	Menubutton.configure trackb ~menu:trackmenu; 
	(* bind the buttons?? *)
	(* default *)
	bind ~events:[`ButtonPressDetail(3)] ~fields:[`MouseX; `MouseY] ~action:
		(fun ev -> 
			goldcurspos :=  calcCursPos ev !gpan false; 
			goldpan := !gpan ;
			Mouse.bindMove top ~action:
			(fun evinf ->
				let prescurs = calcCursPos evinf !goldpan false in
				gpan := Pts2.add (Pts2.sub prescurs !goldcurspos) !goldpan ; 
				render togl nulfun
			); 
		) top ;
	bind ~events:[`ButtonReleaseDetail(3)]  ~fields:[`MouseX; `MouseY] ~action:
		(fun _ -> 
			Mouse.releaseMove top ; 
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
	let switchSelectTrack ev = 
		gcurspos := calcCursPos ev !gpan false; (* don't update the list of hit modules *)
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
	Mouse.bindMove top ~action:updatecurspos ; (* default move action is to simply watch the cursor *)
	bindMouseSelect () ; (* bind the shift keys to selection *)
	(* the strings in the bindings are from X11's keysymdef.h ... *)
	bind ~events:[`KeyPressDetail("r")] ~action:doRotate top ;
	bind ~events:[`ButtonPressDetail(2)] ~fields:[`MouseX; `MouseY] 
		~action:(fun ev -> 
			if !gmode = Mode_AddTrack || !gmode = Mode_MoveTrack then 
				switchSelectTrack ev
			else doRotate ev) top ; (* middle mouse button rotates, or selectsswitches*)
	bind ~events:[`KeyPressDetail("Page_Up")] ~action:(fun _-> changelayercallback "Cop";) top;  
	bind ~events:[`KeyPressDetail("Page_Down")] ~action:(fun _ -> changelayercallback "Cmp";) top;  
	bind ~events:[`KeyPressDetail("F5")] ~action:(fun _ -> changelayercallback "L1";) top;  
	bind ~events:[`KeyPressDetail("F6")] ~action:(fun _ -> changelayercallback "L2";) top;  
	bind ~events:[`KeyPressDetail("F7")] ~action:(fun _ -> changelayercallback "L3";) top;  
	bind ~events:[`KeyPressDetail("F8")] ~action:(fun _ -> changelayercallback "L4";) top;   
	bind ~events:[`KeyPressDetail("m")] ~action:(fun _ -> updateMode "move module";) top;  
	bind ~events:[`KeyPressDetail("t")] ~action:(fun _ -> updateMode "move track";) top; 
	bind ~events:[`KeyPressDetail("a")] ~action:(fun _ -> updateMode "add track";) top; 
	bind ~events:[`KeyPressDetail("x")] ~action:(fun _ -> updateMode "move text";) top; 
	(*bind ~events:[`KeyPressDetail(" ")] ~fields:[`MouseX; `MouseY] ~action:switchSelectTrack top; this is bad, control toggles it too.*)
	bind ~events:[`Modified([`Control], `KeyPressDetail"t")] ~action:tracksFun top; 
	bind ~events:[`Modified([`Control], `KeyPressDetail"v")] ~action:viasFun top; 
	bind ~events:[`Modified([`Control], `KeyPressDetail"s")] ~action:(fun _ -> saveall !gfname; ) top; 
	bind ~events:[`Modified([`Control], `KeyPressDetail"o")] ~action:(fun _ -> openCommand ()) top; 
	bind ~events:[`Modified([`Control], `KeyPressDetail"f")] ~action:
		(fun _ -> Find.find_dlg top !gmodules center_found) top ; 
	bind ~events:[`KeyPressDetail("F3")] ~action:(fun _ -> Find.find_next center_found;) top;  
	bind ~events:[`KeyPressDetail("h")] ~action:doToggleShow top ; 
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
				render togl nulfun; 
			)
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
			ignore(  calcCursPos ev !gpan true ); 
			let midpoint = !gsnapped in
			let track, found = try 
				(List.find (fun t -> t#getHit() ) !gtracks),true
				with Not_found -> (List.hd !gtracks), false
			in
			if found then (
				print_endline "breaking track...";
				let track2 = Oo.copy track in
				track2#setU 0.5 ; 
				track2#clearGrfx (); (* need to make a new graphics b/c the track is copied. *)
				track#setEnd midpoint ; 
				track2#setStart midpoint ; 
				gtracks := (track2 :: !gtracks); 
				track#update () ; 
				track2#update () ; 
				gratsnest#updateTracks (track#getNet()) !gtracks ; 
				render togl nulfun; 
				updateMode "move track"; (*you always want to move track after breaking it *)
			); 
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
	(* display them *)
	pack ~side:`Left 
		[Tk.coe fileb; Tk.coe optionb; Tk.coe viab ; Tk.coe trackb ; 
		Tk.coe infodisp; Tk.coe cursordisp ; Tk.coe mframe; Tk.coe layerframe];
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
	(* first, test the linedistance algorithm *)
	let testLineDistance a b c d = 
		let l,e,f = Pts2.linedistance a b c d in
		let printpoint a = 
			print_string " x " ; 
			print_float (fst a) ; 
			print_string " y " ; 
			print_float (snd a) ; 
		in
		print_endline "points:" ; 
		printpoint a; 
		printpoint b; print_endline " "; 
		printpoint c; 
		printpoint d; print_endline " "; 
		print_endline ( "distance: " ^ (sof l) ) ; 
		print_endline "suggested points: " ; 
		printpoint e ; 
		printpoint f ; 
		print_endline " " ; 
		print_endline "---" ; 
	in
	print_endline "distance should be 1.0";
	testLineDistance (0., 0.) (2., 0.) (1., 1.) (2., 2.) ; 
	print_endline "should be 0.0";
	testLineDistance (-2., -2.) (-5., -2.) (-2., 0.) (-3., -4.) ;
	print_endline "should be 6.708";
	testLineDistance (0., -4.) (0., -4.) (-5., 1.) (-6., -1.) ; 
	print_endline "should be 5.0";
	testLineDistance (0., -4.) (0., 4.) (-5., 1.) (-6., 1.) ;
	print_endline "should be 1.0";
	testLineDistance (-10., 0.) (10., 0.5) (-10., 2.) (30., 1.) ;
	print_endline "should be 0.0";
	testLineDistance (0., 0.) (2., 2.) (0., 2.) (1., 1.) ;
	print_endline "should be 0.0";
	testLineDistance (0., 2.) (1., 1.) (0., 0.) (2., 2.) ;
	print_endline "should be 2.0";
	testLineDistance (0., -1.) (2., -1.) (-0.5, 0.99999) (1., 1.) ;
	print_endline "should be 2.0";
	testLineDistance (0., -1.) (2., -1.) (0.5, 1.) (0.5, 6.) ;
	print_endline "should be 2.828427";
	testLineDistance (1., -1.) (2., -1.) (-1., 1.) (-1., 1.) ;
	
	(* read in the preferences *)
	let geo = ref "" in
	let fil = "/home/" ^ (Unix.getlogin ()) ^ "/.kicadocaml" in
	let cin = try Some (open_in fil)
		with _ -> None
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
	glookupnet := netlookup ; 
	
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
	(* Mesh.makewindow top ; *)
	(* test out the mesh module 
	printf "-- testing meshing --\n%!"; 
	let pts = List.map  (fun(x,y) -> foi x, foi y)
		[(0,0);(1,0);(1,1);(0,1)] in
	ignore(Mesh.mesh pts) ;  *)
	(* this for testing (so we can get a backtrace...  
	openFile top "/home/tlh24/svn/myopen/emg_dsp/stage3/stage3.brd"; 
	gmodules := read_netlist "/home/tlh24/svn/myopen/emg_dsp/stage4/stage4.net" gmodules; 
	gratsnest#clearAll (); 
	Anneal.doAnneal !gmodules (fun () -> render togl nulfun); 
	redoRatNest (); *)
	(* let schema = new schematic in 
	schema#openFile "/home/tlh24/svn/myopen/emg_dsp/stage2.sch" "00000000" "root" ; 
	schema#print "" ; 
	*)
	(* Printexc.record_backtrace true ; ocaml 3.11 *)
	Printexc.print mainLoop () ;
	;;
		
	