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
open Pcre
open Comm
open Grfx
open Modtext
open Shape
open Pad
open Mod
open Track
open Ratnest

type tt_shp = Typ_Circle | Typ_Track ;;

type tt = {
	t_s: float*float; (* pad or start location *)
	t_e: float*float; 
	t_siz : float ; 
	t_shp : tt_shp ; 
	t_pad : pcb_pad option; 
	t_track : pcb_track option ; 
	mutable t_connected : bool ; 
	mutable t_net : int ;
	t_layers : int list ; 
	t_via : bool ; 
}

(* this is from PLEAC, http://pleac.sourceforge.net/pleac_ocaml/arrays.html *)
let fisher_yates_shuffle a = (
	for i = Array.length a - 1 downto 1 do
		let x = a.(i)
		and r = Random.int (i+1) in
		a.(i) <- a.(r);
		a.(r) <- x;
	done
)

let shuffle list = (
	let array = Array.of_list list in
	fisher_yates_shuffle array;
	Array.to_list array
)

let rec propagateNetcodes2 modules tracks doall checkpads top rendercb = (
	(* look at all the unnumbered (0) tracks & try to set their netcode 
	based on connectivity. *)
	(* if doall=true, then look at all the tracks. *)
	if doall then (
		List.iter (fun t -> t#setNet 0; ) !tracks ;
	); 
	(* if we are testing pad connectivity, set all pads except the first to be disconnected *)
	let padnetnums = ref SI.empty in
	if checkpads then (
		List.iter (fun m-> 
			List.iter (fun p->
				let n = p#getNet() in
				if n > 0 then (
					if SI.exists ((=)n) !padnetnums then (
						p#setConnect false ; 
					) else (
						p#setConnect true ; 
						padnetnums := SI.add n !padnetnums ; 
					);
					(* note that this will mark all hanging pads - 
					pads on a net with no other pads - as connected *)
				);
			) (m#getPads())
		) (shuffle !modules); (* not as good as if we updated the net of all pads
		whenever another pad was added - this would allow us to detect truly unconnected things, 
		but it requires a slightly more memory and time *)
	) else (
		List.iter (fun m-> 
			List.iter (fun p->
				p#setConnect true ; 
			) (m#getPads())
		) !modules ; 
	); 
	(* convert to a more efficient data structure. *)
	let objs = ref [] in
	List.iter (fun m -> 
		let flag = if m#getDeleteAttachedTracks () then -1 else 1 in
		List.iter (fun p -> 
			let sx,sy = bbxWH (p#getBBX ()) in
			let siz = (min sx sy)/. 2.0 in
			let q = {
				t_s = p#getCenter () ; 
				t_e = (0.0,0.0) ; 
				t_siz = siz ; 
				t_shp = Typ_Circle ; 
				t_pad = Some p;
				t_track = None ; 
				t_connected = p#getConnect ();
				t_net = flag * (p#getNet ()); 
				t_layers = p#getLayers (); 
				t_via = false; 
			} in
			objs := q :: !objs; 
		) (m#getPads()); 
	) !modules; 
	(* iterate over the tracks, too *)
	List.iter (fun t -> 
		let w = t#getWidth () /. 2.0 in
		let q = {
			t_s = t#getStart (); 
			t_e = t#getEnd (); 
			t_siz = w ;
			t_shp = (match t#getType () with 
				| Track_Via -> Typ_Circle
				| _ -> Typ_Track ); 
			t_pad = None ; 
			t_track = Some t; 
			t_connected = false ; 
			t_net = t#getNet (); 
			t_layers = [t#getLayer()]; 
			t_via = t#isVia();
		} in
		objs := q :: !objs; 
	) !tracks ; 
	let viaviaConn a b = 
			let d = Pts2.distance a.t_s b.t_s in
			d < (a.t_siz +. b.t_siz)
		in
		let viatrackConn a b = (* via, then track *)
			let d = Pts2.distance a.t_s b.t_s in
			let e = Pts2.distance a.t_s b.t_e in
			let f = a.t_siz +. b.t_siz in
			d < f || e < f
		in
		let tracktrackConn a b = 
			let d = Pts2.distance a.t_s b.t_s in
			let e = Pts2.distance a.t_s b.t_e in
			let f = Pts2.distance a.t_e b.t_s in
			let g = Pts2.distance a.t_e b.t_e in
			let h = a.t_siz +. b.t_siz in
			d < h || e < h || f < h || g < h
		in
		let rec propagate unconn changed layercheck = (
			(* each iteration conn are those that have been *changed* *)
			printf "propagate netcodes: changed %d\n%!" (List.length changed); 
			let newchanged = ref [] in
			List.iter (fun o -> (* changed modules *)
				List.iter (fun q -> (* unconnected modules *)
					let conn = if layercheck o q then (
						if q.t_shp = Typ_Circle then (
							if o.t_shp = Typ_Circle then 
								viaviaConn o q
							else 
								viatrackConn q o
						) else (
							if o.t_shp = Typ_Circle then 
								viatrackConn o q
							else 
								tracktrackConn q o
						) 
					) else false in
					if conn then (
						q.t_connected <- true; 
						q.t_net <- o.t_net ; 
						newchanged := q :: !newchanged ; 
					) ;
				) unconn ; 
			) changed ; 
			if List.length !newchanged > 0 then (
				propagate (List.filter (fun o -> not o.t_connected) unconn) !newchanged layercheck; 
			) ; 
		) in
	(* thought: should sort by layer (duhh!) *)
	for layer = 0 to 15 do (
		let active = List.filter (fun o -> 
			List.exists ((=) layer) o.t_layers
		) !objs in
		printf "layer %s active %d\n%!" (layer_to_string layer) (List.length active) ; 
		let conn,unconn = List.partition (fun o -> o.t_connected) active in
		propagate unconn conn (fun _ _ -> true); 
	) done; 
	(* do all layers, as vias switch layers and by only looking at layers we may miss them *)
	printf "checking all layers\n"; 
	let conn,unconn = List.partition (fun o -> o.t_connected) !objs in
	propagate unconn conn (fun a b -> 
		if a.t_via || b.t_via then true else  
		List.exists (fun l1 -> List.exists ((=) l1) b.t_layers) a.t_layers
	); 
	(* copy the attributes *)
	List.iter (fun o -> 
		(match o.t_pad with 
			| Some p -> p#setConnect o.t_connected ; (* don't change the nets ! *)
			| None -> ()
		); 
		(match o.t_track with 
			| Some t -> t#setNet o.t_net ; 
			| None -> ()
		); 
	) !objs ; 
	(* see if there are any pads not connected *)
	if checkpads then (
		let connerr = ref [] in
		List.iter (fun o ->
			(match o.t_pad with 
				| Some _ -> if not o.t_connected && o.t_net > 0 then 
					connerr :=  ( "not connected", o) :: !connerr ; 
				| None -> ()
			); 
		) !objs ; 
		(* probably want to make a window with these errors in buttons so you can click on them *)
		if List.length !connerr > 0 then (
			let dlog = Toplevel.create top in
			Wm.title_set dlog "Connection errors" ;
			(* make a series of buttons -- but only the first 30 errors *)
			let err = ref [] in
			for i = 1 to (min 30 (List.length !connerr)) do (
				err := ((List.nth !connerr (i-1)) :: !err) ; 
			) done ; 
			let cnt = ref 0 in
			let buttons = List.map (fun (e,o) -> 
				incr cnt ; 
				Button.create ~text:((soi !cnt) ^ ": " ^ e)
					~command:(fun () -> 
						gpan := (Pts2.scl o.t_s (-1.0)); 
						gcurspos := o.t_s; 
						(* also highlight the tracks that are not connected to this net .. but should be. *)
						let net = o.t_net in
						List.iter (fun b -> 
							if not b.t_connected then (
								(match b.t_pad with 
									| Some p -> 
										if p#getNet () == net then p#setHighlight true;
									| None -> ()
								); 
								(match b.t_track with 
									| Some t -> 
										if t#getNet () == net then t#setHighlight true;
									| None -> ()
								); 
							)
						) !objs ; 
						rendercb (); )
					dlog) !err ; 
			in
			Tk.pack ~fill:`Both ~expand:true ~side:`Top buttons ; 
			(* need to redo the netcodes so we can put down new tracks! *)
			propagateNetcodes2 modules tracks doall false top rendercb ; 
		) else (
			print_endline "all pads were found to be connected, no errors encountered."; 
			print_endline "good job!"; 
		); 
	) ; 
)

let rec propagateNetcodes modules tracks doall checkpads top rendercb = 
	(* look at all the unnumbered (0) tracks & try to set their netcode 
	based on connectivity. *)
	(* if doall=true, then look at all the tracks. *)
	if doall then (
		List.iter (fun t -> t#setNet 0; ) !tracks ;
	); 
	(* if we are testing pad connectivity, set all pads except the first to be disconnected *)
	let padnetnums = ref SI.empty in
	if checkpads then (
		List.iter (fun m-> 
			List.iter (fun p->
				let n = p#getNet() in
				if n > 0 then (
					if SI.exists ((=)n) !padnetnums then (
						p#setConnect false ; 
					) else (
						p#setConnect true ; 
						padnetnums := SI.add n !padnetnums ; 
					);
					(* note that this will mark all hanging pads - 
					pads on a net with no other pads - as connected *)
				);
			) (m#getPads())
		) !modules ; 
	) else (
		List.iter (fun m-> 
			List.iter (fun p->
				p#setConnect true ; 
			) (m#getPads())
		) !modules ; 
	); 
	(* list of all the tracks to be set.. *)
	print_endline ("tracks to be examined: " ^ (soi (List.length !tracks))) ; 
	let change = ref 1 in
	let pass = ref 1 in
	let connerr = ref [] in
	let changedtracks = ref [] in
	let oldchangedtracks = ref [] in
	oldchangedtracks := !tracks ; (* all tracks are changed at the start *)
	while !change > 0 do (
		let unconn, _ = List.partition (fun t -> t#getNet() = 0) !tracks in
		change := 0 ;
		changedtracks := [] ;
		List.iter (fun t-> 
			let st = t#getStart () in
			let en = t#getEnd () in
			let layer = t#getLayer() in
			let tvia = t#getType() = Track_Via in
			let bbx = t#getDrcBBX () in
			(* first, look over all the pads to see if we are connected to one *)
			(* only have to do this the first pass unless we are checking the pads, 
			in which case the pad connectivity may change.*)
			if !pass = 1 || checkpads then (
				List.iter (fun m-> 
					if bbxIntersect bbx (m#getDrcBBX()) then (
						let rot = m#getRot () in
						List.iter (fun p -> 
							if p#getNet() > 0 && p#hasLayer layer && p#getConnect() then (
								let hitstart, _ = p#pointInPad rot (Pts2.sub st (m#getPos())) in
								let hitend, _ = p#pointInPad rot (Pts2.sub en (m#getPos())) in
								if hitstart || hitend then (
									if (m#getDeleteAttachedTracks()) then (
										t#setNet (-1 * (p#getNet()) ) 
										(* flag to indicate 'delete' used in arraying function *)
									) else (
										t#setNet (p#getNet())
									);
									incr change ; 
									changedtracks := (t :: !changedtracks); 
								); 
							); 
						) (m#getPads()) ; 
					); 
				) !modules ; 
			); 
			(* now, look over the tracks that changed in the iteration
 			to see if we touch them *)
			let w = t#getWidth() *. 0.5 in
			List.iter (fun t2 -> 
				if bbxIntersect bbx (t2#getDrcBBX()) then (
					if t != t2 && t2#getNet() != 0 then (
						if t2#getLayer() = layer || t2#getType() = Track_Via || tvia then (
							let w2 = t2#getWidth() *. 0.5 in
							let dd = (w +. w2) *. (w +. w2) in
							if Pts2.distance2 st (t2#getStart()) < dd ||
								Pts2.distance2 st (t2#getEnd()) < dd ||
								Pts2.distance2 en (t2#getStart()) < dd ||
								Pts2.distance2 en (t2#getEnd()) < dd  then (
								t#setNet (t2#getNet()) ; 
								incr change ; 
								changedtracks := (t :: !changedtracks); 
							); 
						); 
					); 
				); 
			) !oldchangedtracks ; 
			(*print_endline ("propagateNetcodes: changed " ^ (soi !change) ^ " out of " ^ (soi (List.length !track0))); *)
		) unconn ; 
		(* if we are checking the pads, then we need to see if this track hits any unconnected pads *)
		if checkpads then (
			List.iter (fun t-> 
				if t#getNet() > 0 then (
					let st = t#getStart () in
					let en = t#getEnd () in
					let layer = t#getLayer() in
					let bbx = t#getDrcBBX () in
					let tn = t#getNet() in
					List.iter (fun m-> 
						if bbxIntersect bbx (m#getDrcBBX()) then (
							let rot = m#getRot () in
							List.iter (fun p -> 
								let pn = p#getNet() in
								if pn > 0 && not (p#getConnect()) && p#hasLayer layer then (
									let hitstart, _ = p#pointInPad rot (Pts2.sub st (m#getPos())) in
									let hitend, _ = p#pointInPad rot (Pts2.sub en (m#getPos())) in
									if hitstart || hitend then (
										incr change; 
										if pn != tn then (
											let s = Printf.sprintf "pad net %d connected to track net %d" pn tn in
											if hitstart then (
												connerr := ( (s, st) :: !connerr) ; 
											) else (
												connerr := ( (s, en) :: !connerr) ; 
											)
										) else (
											p#setConnect true ; 
											incr change ; 
										) ; 
									); 
								); 
							) (m#getPads()) ; 
						); 
					) !modules ; 
				) ; 
			) !changedtracks ; 
		); 
		print_endline ("propagateNetcodes: changed " ^ (soi !change) ^ " pass " ^ (soi !pass));
		(* print_endline ("(" ^(soi (List.length !changedtracks))^ " tracks)"); *)
		incr pass; 
		oldchangedtracks := !changedtracks ; 
	) done; 
	(* see if there are any pads not connected *)
	if checkpads then (
		List.iter (fun m->
			List.iter (fun p-> 
				if not (p#getConnect()) && p#getNet() > 0 then (
					connerr := ( ( "pad not connected", bbxCenter (p#getBBX())) :: !connerr) ; 
				)
			) (m#getPads())
		) !modules ; 
		(* probably want to make a window with these errors in buttons so you can click on them *)
		if List.length !connerr > 0 then (
			let dlog = Toplevel.create top in
			Wm.title_set dlog "Connection errors" ;
			(* make a series of buttons -- but only the first 30 errors *)
			let err = ref [] in
			let min a b = if a < b then a else b in
			for i = 1 to (min 30 (List.length !connerr)) do (
				err := ((List.nth !connerr (i-1)) :: !err) ; 
			) done ; 
			let cnt = ref 0 in
			let buttons = List.map (fun (e,p) -> 
				incr cnt ; 
				Button.create ~text:((soi !cnt) ^ ": " ^ e)
					~command:(fun () -> 
						gpan := (Pts2.scl p (-1.0)); 
						gcurspos := p; 
						rendercb (); )
					dlog) !err ; 
			in
			Tk.pack ~fill:`Both ~expand:true ~side:`Top buttons ; 
			(* need to redo the netcodes so we can put down new tracks! *)
			propagateNetcodes modules tracks doall false top rendercb ; 
		) else (
			print_endline "all pads were found to be connected, no errors encountered!!"; 
		); 
	) ; 
	(* remember, redo the ratsnest after this call *)
	;;