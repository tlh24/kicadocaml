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
open Comm
open Printf

module Ratnode =
struct
	type ty = {
		nn : int ; (* used for filtering *)
		mutable pos : (float * float) ; 
		mutable connected : int ; (* the sub-net that we are connected to *)
	}
	let create inn ipos = {	
			nn = inn ; 
			pos = ipos ; 
			connected = 0; 
		} 
		;;
	let setPos node p = node.pos <- p ;;
	let getx n = (fst n.pos) ;;
	let gety n = (snd n.pos) ;;
	let getPos node = node.pos ;;
	let getConn n = n.connected ;;
	let setConn n c = n.connected <- c ;;
	let getNN n = n.nn ;;
end

(* have to make this a class to keep around a non-reference Raw variable. *)
class ratsNest = 
object (self)
	(*store the nodes in an array of references of lists.  array index = net number. *)
	val mutable nodes = [| |]
	val mutable tracks1 = [| |] (*nodes associated with track starts *)
	val mutable tracks2 = [| |] (*nodes associated with track ends *)
	val mutable m_rn : (float * float * float * float * int ) list = []
	val mutable m_rnsel : (float * float * float * float * int ) list = []
	val mutable m_rawv = Raw.create_static `float 1
	val mutable m_rawvsel = Raw.create_static `float 1
	val mutable m_updateNN = SI.empty

method ratsNestMakeRaw rn sel= 
	(*make an openGL raw array from a ratsnest *)
	let len = List.length rn in
	(*print_endline ("m_rawv length " ^ (soi len)) ;*)
	if len > 0 then (
		let rawv = Raw.create_static `float (4 * len) in
		let i = ref 0 in
		List.iter (fun (sx,sy,ex,ey,_) ->
			Raw.set_float rawv ~pos:(4* !i + 0) sx ; 
			Raw.set_float rawv ~pos:(4* !i + 1) sy ; 
			Raw.set_float rawv ~pos:(4* !i + 2) ex ; 
			Raw.set_float rawv ~pos:(4* !i + 3) ey ; 
			incr i ; 
		) rn ; 
		if sel then (
			Raw.free_static m_rawvsel ; 
			m_rawvsel <- rawv ; 
		)else(
			Raw.free_static m_rawv ; 
			m_rawv <- rawv ; 
		) ; 
	) ;

method padPos (m: Mod.pcb_module) (p: Pad.pcb_pad) = 
	let a = (foi (m#getRot())) /. 572.9577951 in
	let pp = p#getPos() in
	let mp = m#getPos() in
	Pts2.add (rotate2 ~angle:a pp) mp 

method clearAll () =
	m_rn <- [] ; 
	m_rnsel <- [];  
	Raw.free_static m_rawv ; 
	m_rawv <- Raw.create_static `float 1 ;
	Raw.free_static m_rawvsel ; 
	m_rawvsel <- Raw.create_static `float 1 ;
	m_updateNN <- SI.empty ;

method make (mods : Mod.pcb_module list ) (tracks : Track.pcb_track list) = 
	(*first, iterate over all pads to determine the maximium net number *)
	let maxnn = ref 0 in
	List.iter (fun m -> 
		List.iter (fun pn -> 
			if pn > !maxnn then maxnn := pn; 
		) (m#getPadNets()) ; 
	) mods ; 
	(*iterate over all tracks, too*)
	List.iter (fun t -> 
		let tn = t#getNet() in
		if tn > !maxnn then maxnn := tn; 
	) tracks ; 
	incr maxnn ; 
	nodes <- Array.make !maxnn [] ; 
	(* then, fill up the array! *)
	List.iter (fun m->
		(*set the update command for when dragging stop *)
		m#setUpdateCallback (self#update) ; 
		List.iter (fun p-> 
			let nn = p#getNet () in
			if nn > 0 then (
				let nodepos = self#padPos m p in
				let node = Ratnode.create nn nodepos in
				(*set the move callbacks*)
				p#setMoveCallback (self#move m p node nn ) ; 
				p#setSelCallback (self#highlight node nn ) ; 
				nodes.(nn) <- ( node :: nodes.(nn) ) ; 
			); 
		) (m#getPads()) ; 
	) mods; 
	(*fill with nodes for tracks -- two nodes per track. *)
	tracks1 <- Array.make !maxnn [] ; 
	tracks2 <- Array.make !maxnn [] ; 
	List.iter (fun t-> 
		let st = t#getStart () in
		let en = t#getEnd () in
		let nn = t#getNet () in
		if nn > 0 then (
			let node1 = Ratnode.create nn st in
			let node2 = Ratnode.create nn en in
			tracks1.(nn) <- (node1 :: tracks1.(nn)) ; 
			tracks2.(nn) <- (node2 :: tracks2.(nn)) ; 
		);
	) tracks ;
	(* do this afterward to save time *)
	Array.iteri (fun nn nodelist -> 
		if (List.length nodelist) > 0 then 
			m_updateNN <- SI.add nn m_updateNN ; 
	) nodes ; 
	Array.iteri (fun nn nodelist -> 
		if (List.length nodelist) > 0 then 
			m_updateNN <- SI.add nn m_updateNN ; 
	) tracks1 ; 
	self#update ()
	
method updateTracks ?(final=true) nn (tracks : Track.pcb_track list) = 
	if nn != 0 then (
		print_endline ("updating net " ^ (soi nn) ) ; 
		tracks1.(nn) <- [] ; 
		tracks2.(nn) <- [] ; 
		List.iter (fun t-> 
			if (t#getNet()) = nn then (
				let st = t#getStart () in
				let en = t#getEnd () in
				let node1 = Ratnode.create nn st in
				let node2 = Ratnode.create nn en in
				tracks1.(nn) <- (node1 :: tracks1.(nn)) ; 
				tracks2.(nn) <- (node2 :: tracks2.(nn)) ; 
			); 
		) tracks ;
		(* remove old lines *)
		m_rn <- List.filter (fun (_,_,_,_,rnetnum) -> not (rnetnum = nn)) m_rn ; 
		(* reconnect *)
		if final then (
			self#connectMst nn ; 
			(* redo the lines *)
			self#ratsNestMakeRaw m_rn false; 
		);
	) ; 

method updateTracksAll (tracks : Track.pcb_track list) = 
	SI.iter (fun netnum -> 
		(* need to update the node positions of all tracks *)
		self#updateTracks ~final:false netnum tracks ; 
	) m_updateNN ;
	(* now can recalc the mst for affected nets *)
	self#update () ; 
	
(* move a pad.  curry + closure to make a function that takes a 
pad position & netnum & updates ratnode pos / adds the netnum to be 
redone *)
method move (m :Mod.pcb_module) (p:Pad.pcb_pad) node netnum () = 
	let pos = self#padPos m p in
	Ratnode.setPos node pos;
	m_updateNN <- SI.add netnum m_updateNN ; 
	(* don't need to call highlight -- automatically enabled by selection *)
	
method highlight node netnum () = 
	(* printf "running ratnest#highlight\n%!"; *)
	(* add lines for dragging & highlighting *)
	let pos = Ratnode.getPos node in
	if (Array.length nodes) > netnum then (
		if !gdragShowAllNets || !gdragShow5Smallest then (
			let rnselAppend bnode =
				if bnode != node then (
					let xc = Ratnode.getx bnode in
					let yc = Ratnode.gety bnode in
					m_rnsel <- ( ((fst pos),(snd pos),xc,yc,netnum) :: m_rnsel ); 
				)
			in
			(* add pads *)
			List.iter (fun bnode -> rnselAppend bnode ) nodes.(netnum) ; 
			if List.length m_rnsel < 50 then (
				(* tracks if it does not look like too big of a problem *)
				List.iter2 (fun bnode1 bnode2 -> 
					rnselAppend bnode1; 
					rnselAppend bnode2; 
				) tracks1.(netnum) tracks2.(netnum) ; 
			) ; 
			let rnLength (ax1,ay1,ax2,ay2,_) = 
				Pts2.distance2 (ax1,ay1) (ax2,ay2)
			in
			if !gdragShow5Smallest then (
				let sorted = ref [] in
				sorted :=List.sort (fun a b -> 
					let ad = rnLength a in
					let bd = rnLength b in
					if ad < bd then -1 else if ad > bd then 1 else 0
				) m_rnsel ; 
				(* select the top 5 *)
				m_rnsel <- [] ; 
				if List.length !sorted > 4 then (
					for i = 0 to 4 do (
						try (
							m_rnsel <- (List.hd !sorted :: m_rnsel); 
							sorted := List.tl !sorted ;
						) with _ -> () ; 
					) done; 
				)
			); 
		) else (
			if (List.length nodes.(netnum)) > 0 then (
				let (cnode,cdist) = 
					List.fold_left (fun (cnode1,cdist1) bnode -> 
						let d = Pts2.distance2 (Ratnode.getPos bnode) pos in
						if d < cdist1 && bnode != node then 
							(bnode, d)
						else
							(cnode1, cdist1)
					) ((List.hd nodes.(netnum) ), 1e24) nodes.(netnum)
				in
				(* tracks too *)
				let (cnode2,_) = 
					List.fold_left2 (fun (cnode1,cdist1) bnode1 bnode2 -> 
						let d1 = Pts2.distance2 (Ratnode.getPos bnode1) pos in
						let d2 = Pts2.distance2 (Ratnode.getPos bnode2) pos in
						let d,bnode = if d1 < d2
							then d1,bnode1
							else d2,bnode2 
						in
						if d < cdist1 then 
							(bnode, d)
						else
							(cnode1, cdist1)
					) (cnode,cdist) tracks1.(netnum) tracks2.(netnum)
				in
				let xc = Ratnode.getx cnode2 in
				let yc = Ratnode.gety cnode2 in
				m_rnsel <- ( ((fst pos),(snd pos),xc,yc,netnum) :: m_rnsel ); 
			); 
		)
	) ;

(* need a new algorithm to generate this minimum-spanning tree for each net *)
(* tracks are about the same as normal nodes, except they have already been contracted *)
method connectMst netnum = 
	(* printf "running connectMst\n%!"; *)
	let nodelist = nodes.(netnum) in
	let numtracks = List.length (tracks1.(netnum)) in
	let arraylen = (List.length nodelist) + numtracks in
	let narray = Array.make arraylen [] in
	let narraycount = Array.make arraylen 0 in
	let count = ref 0 in
	let totalnodes = (List.length nodelist) + 2 * numtracks in
	if totalnodes < 200 || not !gLimitRatnest200 then ( 
		(*iterate over all the module associated nodes*)
		List.iter (fun node -> 
			Ratnode.setConn node !count ; 
			narray.(!count) <- (node :: narray.(!count)) ; 
			narraycount.(!count) <- 1 ; 
			incr count ; 
		) nodelist ; 
		(* iterate over the tracks, too *)
		List.iter2 (fun node1 node2 ->
			Ratnode.setConn node1 !count ; 
			Ratnode.setConn node2 !count ; 
			narray.(!count) <- (node2 :: (node1 :: narray.(!count)));
			narraycount.(!count) <- 2 ; 
			incr count ; 
		) tracks1.(netnum) tracks2.(netnum) ; 
		(* combine these nodes with Boruvka-like algorithm *)
		(* probably could be sped up.. eh, later. *)
		let condensednodes = ref 0 in
		let findClosest node subnet (cnode,cdist) = 
			let count = ref 0 in
			Array.fold_left (fun (cnode1,cdist1) nlist -> 
				 let ret = if narraycount.(!count) > 0 && !count != subnet then (
					List.fold_left (fun (cnode2,cdist2) bnode ->
						let d = Pts2.distance2 (Ratnode.getPos node) (Ratnode.getPos bnode) in
						if d < cdist2 then
							(bnode, d)
						else
							(cnode2,cdist2)
						) (cnode1,cdist1) nlist
					) else (cnode1, cdist1)
				in
				incr count ; 
				ret
			) (cnode,cdist) narray 
		in
		let merge node1 node2 = 
			let sn1 = Ratnode.getConn node1 in
			let sn2 = Ratnode.getConn node2 in
			List.iter (fun node -> Ratnode.setConn node sn1;) narray.(sn2) ; 
			narray.(sn1) <- List.rev_append narray.(sn2) narray.(sn1) ; 
			narray.(sn2) <- [] ; 
			narraycount.(sn1) <- narraycount.(sn1) + narraycount.(sn2) ; 
			narraycount.(sn2) <- 0 ;
		in
		while !condensednodes < totalnodes do (
			Array.iteri (fun subnet nlist -> 
				if narraycount.(subnet) > 0 then (
					(*find the node in the present sub-net that is closest
					to a node in another sub-net *)
					let startnode = List.hd nlist in
					let (cnode,cdist,bnode) = List.fold_left (fun (cnode1,cdist1,bn) bnode1 -> 
							let (cnode2,cdist2) = findClosest bnode1 subnet (cnode1,cdist1) in
							if cdist2 < cdist1 then
								(cnode2,cdist2,bnode1)
							else
								(cnode1,cdist1,bn)
						) (startnode, 1e24,startnode) nlist
					in
					if cdist < 1e24 then (
						(*merge the groups down*)
						if (Ratnode.getConn bnode) < (Ratnode.getConn cnode) then (
							merge bnode cnode 
						) else (
							merge cnode bnode
						) ; 
						if cdist > 0.0001 then (
							let xb = Ratnode.getx bnode in
							let yb = Ratnode.gety bnode in
							let xc = Ratnode.getx cnode in
							let yc = Ratnode.gety cnode in
							let nn = Ratnode.getNN bnode in
							m_rn <- ((xb,yb,xc,yc,nn) :: m_rn ); 
						) ; 
					) ; 
				)
			) narray ;
			condensednodes := narraycount.(0) ; 
		) done; 
	)

method update () = 
	let count = ref 0 in
	SI.iter (fun netnum -> 
		(* clear out old rat lines *)
		m_rn <- List.filter (fun (_,_,_,_,rnetnum) -> not (rnetnum = netnum)) m_rn ; 
		incr count ;
	) m_updateNN ;
	if !count > 0 then (
		SI.iter self#connectMst m_updateNN;
		m_updateNN <- SI.empty ; 
		self#ratsNestMakeRaw m_rn false; 
	);
	
method clearSel () = 
	m_rnsel <- [] ; 
	
method draw () = 
	(*render the rat's nest *)
	if (Raw.length m_rawv > 2) then (
		GlDraw.color ~alpha:0.5 (1. , 1. , 1. ); 
		GlMat.push() ; 
		GlMat.translate ~x:(0.) ~y:(0.) ~z:(0.99) (); 
		(* need to put it on top of the other drawn items ... *)
		GlArray.vertex `two m_rawv; 
		GlArray.draw_arrays `lines 0 (Raw.length m_rawv / 2) ; 
		(* render the selected lines, activated on move *)
		if List.length m_rnsel > 0 then (
			self#ratsNestMakeRaw m_rnsel true ; 
			GlDraw.color ~alpha:1. (1. , 1. , 0.5 ); 
			GlArray.vertex `two m_rawvsel; 
			GlArray.draw_arrays `lines 0 (Raw.length m_rawvsel / 2) ; 
		); 
		GlMat.pop() ; 
	); 
end
