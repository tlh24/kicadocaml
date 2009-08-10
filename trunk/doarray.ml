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
open Ratnest
open Propagate
open Drc
open Schematic

let doArray sheets template ax ay 
	modules tracks render redoRatNest top = 
	(*first task: build up an associative array mapping 
	sheet name to sheet timestamp. 
	we used to do this by looking at the board itself, 
	but due to possible confusion, we now look at the 
	source schematics themselves *)
	print_endline "** be patient, this may take some time, "; 	 
	print_endline "** as the algorithm works by testing copper continuity " ; 	 
	print_endline "** to determine which tracks to delete and which to duplicate & move" ;
	let sp = (Pcre.extract ~pat:"([^\{]+)\{(\d+)\.+(\d+)\.+(\d+)\}" sheets) in
	let basename = sp.(1) in
	let startsheet = ios sp.(2) in
	let incrsheet = ios sp.(3) in
	let endsheet = ios sp.(4) in
	let templatesheet = ios (Pcre.extract ~pat:"(\d+)$" template).(1) in
	let templatets = ref "" in
	(* need to figure out the length of the array  *)
	let rec range a b c = 
		if a > c then []
		else a :: range (a+b) b c
	in
	let sheetlist = range startsheet incrsheet endsheet in
	let ns = List.length sheetlist in
	printf "number of sheets to move: %d\n%!" ns; 
	let lut = Array.make ns (0,basename, "",[]) in
	let d = ref 0 in
	List.iter (fun i -> 
		let sn = basename ^ (soi i) in
		let (found, ar) = gschema#findSubSch sn "" in
		if (not found) then ( raise Not_found ) ; (* wonder why this was commented out? *)
		lut.(!d) <- (i, sn, ar, []) ;
		if (String.compare template sn) == 0 then templatets := ar ; 
		incr d ; 
	) sheetlist ; 
	(*print it out for sanity check..*)
	Array.iter (fun (_,s,a,_) -> print_endline( s ^ " -> " ^ a)) lut ; 
	print_endline ("TemplateTS -> " ^ !templatets) ; 
	(*first task: move components*)
	(* to do this, we should make a list of template modules *)
	let tmplmods = List.filter (fun m -> m#pathHas !templatets ) !modules in
	print_endline( soi(List.length tmplmods) ^ " template modules found.") ; 
	(* and an array of lists of the modules to be moved, split by sheet *)
	let tomovelist = ref [] in (*a list of the moved modules *)
	Array.iteri ( fun k (i,sn,ar,_) -> 
		if (String.compare sn template) != 0 then ( (* don't move the templates! *)
			let l = List.filter (fun m -> m#pathHas ar) !modules in
			print_endline( soi(List.length l) ^ " modules in sheet " ^ sn) ; 
			lut.(k) <-  (i,sn,ar,l) ; 
			tomovelist := List.rev_append l !tomovelist ; 
		)
	) lut ;  
	(* before moving any modules, have to delete the old tracks. *)
	Array.iter (fun (_,_,_,tm) -> 
		List.iter (fun m ->
			(* tracks: set the module as 'moving', 
				so that when we go back we can flag the tracks 
				(using propagatenetcode) and delete them based on 
				flag*)
			m#setDeleteAttachedTracks true ; 
		) tm; 
	) lut; 
	(* set the flags for the tracks attached to the moved modules *)
	print_endline "calculating tracks associated with to-be-moved modules" ; 
	propagateNetcodes modules tracks true false top render (fun () -> ()) (); 
	(* filter these tracks out *)
	print_endline "removing tracks associated with moved modules" ; 
	tracks := List.filter (fun t -> (t#getNet()) >= 0) !tracks; 
	(* clear the flags on the modules *)
	Array.iter (fun (_,_,_,tm) -> 
		List.iter (fun m ->
			m#setDeleteAttachedTracks false ;
		) tm; 
	) lut ; 
	render() ; 
	(* build up a list of the template tracks in the same way. *) 	 
	List.iter (fun m-> m#setDeleteAttachedTracks true ) tmplmods ; 	 
	print_endline "calculating tracks associated with template modules (by continuity test)" ; 	 
	propagateNetcodes modules tracks true false top render (fun () -> ()) (); 
	let templatetracks = List.filter (fun t -> (t#getNet()) < 0) !tracks in (* delete -> set neetcode < 0  *)
	(* clear the flag *) 	 
	List.iter (fun m-> m#setDeleteAttachedTracks false ) tmplmods ;
	(*go over each of tomove, find the corresponding template module. *)
	print_endline "moving the modules" ;
	Array.iter (fun (i,_,_,tm) -> 
		List.iter (fun m ->
			let r = m#pathLast () in (*r = module timestamp*)
			let (p,found) =
				try ((List.find (fun p -> p#pathHas r) tmplmods), true)
				with Not_found -> (m, false)
			in
			if found then (
				let ox, oy = p#getPos () in
				let fi = (foi ( i - templatesheet )) /. (foi incrsheet) in
				let x = fi *. ax  +. ox in
				let y = fi *. ay  +. oy in
				(* we also want to copy text size & position from the template 
				( in case it has been moved ) *)
				List.iter (fun ptxt -> 
					List.iter (fun mtxt -> 
						if ptxt#getType() = mtxt#getType() then (
							(* copy the settings (except the value, of course)*)
							mtxt#setSize (ptxt#getSize()) ; 
							mtxt#setPos (ptxt#getPos()) ; 
							mtxt#setShow (ptxt#getShow()) ; 
							mtxt#setRot (ptxt#getRot()) ; 
						); 
					) (m#getTexts()) ; 
				) (p#getTexts()); 
				m#setPos (x, y) ;
				m#setRot (p#getRot () ) ; 
				m#update() ;
			); 
		) tm; 
	) lut ; 
	render() ; 
	(* now need to replicate the tracks associated with template module.*)
	print_endline "duplicating tracks associated with template modules" ; 
	d := 0 ; 
	List.iter (fun k ->  
		let fi = (foi ( k - templatesheet )) /. (foi incrsheet) in
		if k != templatesheet then (
			List.iter (fun t -> 
				let mm = Oo.copy t in
				mm#setNet 0; 
				mm#setU 0.5 ; 
				mm#move ((fi *. ax), (fi *. ay)) ; 
				mm#clearGrfx (); (* need to make a new graphics b/c the track is copied. *)
				mm#applyMove () ; 
				mm#update ();
				tracks := (mm :: !tracks); 
			) templatetracks; 
			render() ; 
		); 
	) sheetlist ; 
	(* set the netcodes ..  all tracks, since we made the template tracks negative when selecting them *)
	print_endline "setting netcodes of all tracks" ; 	  
	propagateNetcodes modules tracks true false top render redoRatNest(); 
	render() ; 
	;;