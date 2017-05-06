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

type pcb_track_type = Track_Track | Track_Via

class pcb_track = 
object (self)
		val mutable m_shape = 0 (* 0 = rounded; 1 = rectangle *)
		val mutable m_stx = 0. (* need these stored as floats so we don't run into *)
		val mutable m_sty = 0. (* roundoff issues when drc auomatically moves them *)
		val mutable m_enx = 0.
		val mutable m_eny = 0.
		val mutable m_width = 0.
		val mutable m_drill = 0.
		val mutable m_layer = 0
		val mutable m_type = Track_Track
		val mutable m_net = 0
		val mutable m_status = "0" 
(* 		val mutable	m_drawsegment = false (*for drawsegments*) *)
		val mutable	m_angle = 0
		val mutable m_g = new grfx
		val mutable m_hit = false
		val mutable m_washit = false
		val mutable m_u = 0.
		val mutable m_move = (0. , 0.)
		val mutable m_moving = false
		val mutable m_highlight = false
		val mutable m_dirty = false
		val mutable m_drcBBX = (0. , 0. , 0. , 0.)
		val mutable m_visible = true
		
		val mutable m_moveStartConstraint = (fun (_:(float * float)) (_:(float * float)) -> (0. , 0.))
		val mutable m_moveEndConstraint = (fun (_:(float * float)) (_:(float * float)) -> (0. , 0.))
		val mutable m_hasStartConstraint = false ; 
		val mutable m_hasEndConstraint = false ; 
		
		method getNet () = m_net
		method setNet n = m_net <- n
		method setDirty d = m_dirty <- d
		method getDirty () = m_dirty
		method setVisible b = m_visible <- b 
		method getVisible () = m_visible
		method getBBX () = m_g#getBBX ()
		method getShape () = m_shape; 
		method setShape s = 
			if s <> m_shape then (
				m_shape <- s; 
				self#update ()
			)
		method getStart () = 
			if m_type = Track_Track then (
				if m_hasStartConstraint then (
					m_moveStartConstraint 
						(Pts2.add (m_stx,m_sty) m_move) 
						(Pts2.add (m_enx,m_eny) m_move) 
				) else (
					if m_u < 1. then 
						Pts2.add (m_stx, m_sty) m_move
					else 
						(m_stx, m_sty) 
				)
			) else (
				Pts2.add (m_stx, m_sty) m_move
			)
		method getEnd () = 
			if m_type = Track_Track then (
				if m_hasEndConstraint then (
					m_moveEndConstraint 
						(Pts2.add (m_stx,m_sty) m_move) 
						(Pts2.add (m_enx,m_eny) m_move) 
				) else (
					if m_u > 0. then 
						Pts2.add  (m_enx, m_eny) m_move
					else 
						(m_enx, m_eny)
				)
			) else (
				Pts2.add (m_stx, m_sty) m_move
			)
		method setStart p = 
			let x,y = Pts2.sub p m_move in
			m_stx <-  x; m_sty <- y ; 
			if m_type = Track_Via then ( m_enx <- m_stx; m_eny <- m_sty; ) ;
			self#updateDrcBBX(); 
		method setEnd p =
			if m_type = Track_Track then (
				let x,y = Pts2.sub p m_move in
				m_enx <- x; m_eny <- y ; 
			); 
			self#updateDrcBBX(); 
		method flip () = (
			(* switch the start and the end *)
			if m_type = Track_Track then (
				let u = self#getStart() in
				self#setStart ( self#getEnd() ); 
				self#setEnd u ; 
			); 
		)
		method setLayer l = m_layer <- l ; 
		method getLayer () = m_layer ; 
		method getGrfx () = m_g ; 
		method getType () = m_type ;
		method isVia() = m_type = Track_Via ;
		method getWidth () = m_width
		method setWidth w = m_width <- w; 
		method getDrill () = m_drill
		method setDrill d = m_drill <- d ;
		method getU () = m_u 
		method setU u = 
			if m_type = Track_Track then m_u <- u ; 
		method setHighlight b = m_highlight <- b ;
		method setType t = m_type <- t ; 
		method getLength () = Pts2.distance (self#getStart()) (self#getEnd())
	
		method setMoveStartConstraint f = (
			m_moveStartConstraint <- f ; 
			m_hasStartConstraint <- true ; 
		)
		method setMoveEndConstraint f = (
			m_moveEndConstraint <- f ; 
			m_hasEndConstraint <- true ; 
		)
		method clearStartConstraint () = m_hasStartConstraint <- false ; 
		method clearEndConstraint () = m_hasEndConstraint <- false ; 
		method clearConstraint () = m_hasStartConstraint <- false ;  m_hasEndConstraint <- false ; 
		
		val mutable m_updateCallback = (fun () -> () )
		method setUpdateCallback f = m_updateCallback <- f ; 
		method update () = (
			m_dirty <- false ;
			m_g#empty () ; 
			m_g#setZ 0.0 ; 
			(* don't #updateLayer -- z should remain 0.0 for all tracks *)
			(* perform z-sorting with opengl translate operations. *)
			(
				let st = self#getStart() in
				let en = self#getEnd() in
				match m_type with 
					| Track_Track -> (
						if Pts2.distance2 st en = 0. then (
							m_g#makeCircle (fst st) (snd st) m_width m_width
						) else (
							if m_shape = 1 then (
								m_g#makeRectTrack st en m_width
							) else (
							m_g#makeTrack st en m_width ) ) )
					| Track_Via -> 
						m_g#makeRing st m_drill m_width; 
			);
			self#updateDrcBBX(); 
			self#updateColor (); 
			m_updateCallback () ; 
		)
		method updateColor () = (
			match m_type with 
				| Track_Track -> m_g#setColor (layer_to_color m_layer) ; 
				| Track_Via -> m_g#setColor !gviaColor ; (* for now, all vias are through-hole vias.. *)
		)
		method dispinfo () = (
			let len = self#getLength () in
			let s = match m_type with
				| Track_Track -> "track, width: "
				| Track_Via -> "via, diameter: "
			in
			!ginfodisp ( s ^ (format_dim m_width) ^"\nlength: "^ (format_dim len) )
				(* used to have netname in here  ^ " netname:" ^ (!glookupnet m_net) *)
		)
		method draw bbox = (
			if m_visible then (
				if m_hit then (
					self#dispinfo ()
				); 
				if !gcurnet = m_net && !gcurnet != 0 then (
					m_g#setAlpha 0.78
				) else ( m_g#setAlpha 0.50 ); 
				ignore(if m_highlight then (
					m_highlight <- false; 
					m_g#draw ~hit:true ~hcolor:(0.4, 1., 1.) bbox ; 
				) else (
					m_g#draw ~hit:m_hit bbox ; 
				));
			); 
		)
		method getSize () = (
			let w2 = m_width /. 2. in
			match m_type with
			| Track_Track -> 
				(Pts2.distance (m_stx,m_sty) (m_enx,m_eny)) *. m_width +.
				w2 *. w2 *. 3.1415926
			| Track_Via -> w2 *. w2 *. 3.1415926
		)
		method getHit () = m_hit ; 
		method setHit h = m_hit <- h ; 
		method hitclear () = m_hit <- false
		method hit (p, onlyworknet, netnum, hitsize, hitz, hitclear) = 
			(* do not update 'hit' if the first mouse button is down. *)
			(* hit bazed on z-sorting. *)
			(* hitclear is a list as we can hit more than one track at a time *)
			let mz = match m_type with
				| Track_Track -> glayerZ.(m_layer)
				| _ -> (
					let max = ref 0. in
					for i = 0 to 15 do (
						if glayerZ.(i) > !max then max := glayerZ.(i)
					) done; 
					!max
				)
			in
			let w2 = m_width /. 2. in
			let ms = self#getSize () in (* area or size of track *)
			(* z-sorting: want to be able to hit small items in the background *)
			(* don't want to allow hitting tracks on more than one layer - 
			if we do, must clear the previous *)
			if not m_moving && mz >= hitz && glayerEn.(m_layer) &&
				m_visible && (not onlyworknet || netnum = m_net) then (
				(* don't update the hit variable if we are moving*)
				(* don't hit if onlyworknet (e.g. when adding or removing a track)
				is true and we are not the workingnet. *)
				m_washit <- m_hit ; 
				m_hit <- self#touch p ; 
				
				let st = self#getStart()  in
				let en = self#getEnd() in

				if m_hit then (
					(* need to update u, the position on the track that we were hit. *)
					if m_type = Track_Track then (
						let u = Pts2.closestuonline st en p true in
						let tol = w2 /. (Pts2.distance st en) in
						m_u <- if u < tol then 0.
							else if u > 1. -. tol then 1. 
							else u ; 
						(* note we snap it to the endpoints. *)
					); 
					(* if it is a via, then m_u is always 0. *)
					(* manage the snaps *)
					if !gdosnap then (
						(match m_type with 
							| Track_Track -> (
								gsnapped := Pts2.closestpointonline st en p true ; 
								(* snap to the end caps *)
								if Pts2.distance !gsnapped st < w2 then
									gsnapped := st ; 
								if Pts2.distance !gsnapped en < w2 then
									gsnapped := en ; ); 
							| Track_Via -> gsnapped := st ;
						); 
					); 
					(* manage the return data *)
					if mz <> hitz then (
						(* clear all the old hit *)
						(* printf "clearing all old hit in track#hit z=%f\n%!" mz;*)
						List.iter (fun f -> f ()) hitclear; 
						m_net, ms, mz, [self#hitclear]
					)else(
						m_net, ms, mz, (self#hitclear :: hitclear)
					)
				)
				else ( m_hit <- false; netnum, hitsize, hitz, hitclear )
			) else  ( m_hit <- false; netnum, hitsize, hitz, hitclear )
			
		method touch p = (
			(* this is a softer version of hit - 
			it only sees if the point is within the track area. 
			it does not update anything, just returns a bool 
			only works if track is visible.*)
			let w2 = m_width /. 2. in
			let st = self#getStart()  in
			let en = self#getEnd() in
			if m_type = Track_Via then (
				(Pts2.distance st p ) < w2
			) else if !gTrackEndpointOnly then (
				(Pts2.distance st p ) < w2 || (Pts2.distance en p ) < w2
			)else if glayerEn.(m_layer) then (
				Pts2.tracktouch st en p w2
			) else false
		)
		method updateDrcBBX () = (
			let stx,sty = self#getStart () in
			let enx,eny = self#getEnd () in
			let minx,maxx = if stx < enx then stx,enx else enx,stx in
			let miny,maxy = if sty < eny then sty,eny else eny,sty in
			let g = !gclearance +. self#getWidth() *. 0.5 in
			m_drcBBX <- ( minx -. g , miny -. g, maxx +. g, maxy +. g); 
		)
		method getDrcBBX () = m_drcBBX 
		(* the drcBBX is used to hopefully speed up DRC calculation by culling out 
		tracks that could not possibly be intersecting *)
		method selectHit (xl,yl,xh,yh) = (
			(* do a relatively good track-box intersect 
			or the purposes of selecting tracks within a selection rectangle. *)
			if (not m_visible) || (not glayerEn.(m_layer)) then (
				false 
			) else (
				let st = self#getStart () in
				let en = self#getEnd () in
				if bbxInside (xl,yl,xh,yh) st then true 
				else if bbxInside (xl,yl,xh,yh) en then true 
				else (
					let ar = [|
						(Pts2.linedistance st en (xl,yl) (xl,yh) ) ;  (*vertical left *)
						(Pts2.linedistance st en (xh,yl) (xh,yh) ) ; (* vertical right *)
						(Pts2.linedistance st en (xl,yl) (xh,yl) ) ; (* horizontal bottom *)
						(Pts2.linedistance st en (xl,yh) (xh,yh) )  (* horizontal top *)
						|] in
					let min = ref 1e24 in
					let ee = ref (0. ,0.) in
					let ff = ref (0. ,0.) in
					Array.iter (fun (l,e,f) -> 
						if l < !min then (
							min := l; 
							ee := e; 
							ff := f; 
						); 
					) ar; 
					(* print_endline ("pad testdrc: min = " ^ (sof !min)) ; *)
					!min < self#getWidth() *. 0.5 
				)
			)
		)
		method move m = (
			if m_u <= 1. && m_u >= 0. then (
				m_move <- m ; 
				self#updateDrcBBX () ; (* also applies the callbacks through getStart() & getEnd() *)
			) ; 
		)
		method applyMove () = (
			let a = (self#getStart()) in
			let b = (self#getEnd()) in
			if m_u < 1.  && m_u >= 0. then (
				m_stx <- (fst a); 
				m_sty <- (snd a); 
			); 
			if m_u > 0. && m_u <= 1. then (
				m_enx <- (fst b); 
				m_eny <- (snd b); 
			); 
			m_moving <- false ; 
			m_move <- (0. , 0.) ;
			self#updateDrcBBX(); 
		)
		method setMoving b = (
			m_moving <- b ;
			if b then (m_move <- 0. , 0.  )
			else( self#applyMove () ) ; 
		)
		method getMoving () = m_moving
		method newGrfx () = m_g <- new grfx ; 
		method manhattanLength () = (abs_float (m_stx -. m_enx)) +. (abs_float (m_sty -. m_eny))
		
		(*below, timestamp and status always seem to be 0 - not including *)
		method read ic line = 
		(
			let sp = Pcre.extract ~pat:"Po (\d+) ([\.\d-]+) ([\.\d-]+) ([\.\d-]+) ([\.\d-]+) ([\.\d-]+) ([\.\d-]+)" line in
			m_shape <- ios sp.(1) ; 
			m_stx <- foss sp.(2) ; 
			m_sty <- foss sp.(3) ; 
			m_enx <- foss sp.(4) ; 
			m_eny <- foss sp.(5) ; 
			m_width <- foss sp.(6) ; 
			m_drill <- foss sp.(7) ; 
			let line2 = input_line2 ic in (*the De ... line*)
			let sp = if !gfver = 1 then 
				Pcre.extract ~pat:"De (\d+) (\d+) (\d+) \d+ (\w+)" line2 
			else
				Pcre.extract ~pat:"De (\d+) (\d+) (\d+) (\w+) \d+" line2 
			in
			(* !! version 2 has a different order here.. *)
			m_layer <- ios sp.(1) ; 
			m_type <- ( match sp.(2) with
				| "1" -> Track_Via ; 
				| _ -> Track_Track ; 
			); 
			m_net <- ios sp.(3) ; 
			m_status <- sp.(4) ; 
(* 			if m_layer = 24 (*drawings *) then m_drawsegment <- true ;  *)
			if m_type = Track_Via then m_shape <- 3 ;  (* this seems to be a requirement for importing .brd files with the new version of pcbnew (4.0.0) *)
		)
		method save oc = (
(* 			if not m_drawsegment then ( *)
			fprintf oc "Po %d %s %s %s %s %s %s\n"
				m_shape (sofs m_stx) (sofs m_sty) (sofs m_enx) (sofs m_eny) 
				(sofs m_width) (sofs m_drill);
			(* pcbnew expects vias to be on layer 15 (component) *)
			let layer = if m_type = Track_Via then 15 else m_layer in
			fprintf oc "De %d %d %d 0 %s\n" layer 
				(match m_type with
					| Track_Via -> 1
					| Track_Track -> 0 ) m_net m_status; 
			flush oc ; 
			(* ) else (
				fprintf oc "$DRAWSEGMENT\n" ; 
				fprintf oc "Po %d %s %s %s %s %s\n" 
					m_shape (sofs m_stx) (sofs m_sty) (sofs m_enx) (sofs m_eny) (sofs m_width) ; 
				fprintf oc "De %d %d %d 0 0\n" 
					m_layer 0 m_angle ; 
				fprintf oc "$EndDRAWSEGMENT\n" ; 
			)*)
		)
		(* treat drawsegments as tracks, too - this allows easier editing! *)
		method is_drawsegment () =  false (* m_drawsegment *)
		
		method read_drawsegment ic = (
			let line = input_line2 ic in
			let sp = Pcre.extract ~pat:"Po (\d+) ([\.\d-]+) ([\.\d-]+) ([\.\d-]+) ([\.\d-]+) ([\.\d-]+)" line in
			m_shape <- ios sp.(1) ; 
			m_stx <- foss sp.(2) ; 
			m_sty <- foss sp.(3) ; 
			m_enx <- foss sp.(4) ; 
			m_eny <- foss sp.(5) ; 
			m_width <- foss sp.(6) ; 
			let line2 = input_line2 ic in
			let sp2 = Pcre.extract ~pat:"De (\d+) (\d+) (\d+)" line2 in
			m_layer <- ios sp2.(1) ;
			m_type <- Track_Track ;
			m_angle <- ios sp2.(3) ;
			(*read the $EndDRAWSEGMENT line *)
			let d = ref "" in
			d := input_line2 ic ; 
			m_net <- 0;
(* 			m_drawsegment <- true;  *)
		)
end
