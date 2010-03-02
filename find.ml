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
open Tk
open Comm
open Modtext
open Mod

(* find a part based on value or reference designator *)
(* also mantain a list so you can F3 through it? *)

let gmatches = ref [| |]
let gmatchn = ref 0 

let find_output () = 
	let len = Array.length !gmatches in
	if len > 0 then (
		printf "match %d of %d : " (!gmatchn + 1) len ; 
		(* set the present as hit, all others as not hit *)
		Array.iter (fun m -> m#setHit false) !gmatches ; 
		let m = !gmatches.(!gmatchn) in
		m#setHit true ; 
		m#crossprobe () ; 
		printf "package:%s " (m#getLibRef ()) ; 
		List.iter (fun t -> printf "%s " (t#getText()) ; )
			(m#getTexts()) ; 
		printf "\n%!" ; 
		(bbxCenter (m#getDrcBBX())), true
	) else (
		printf "no matches! \n%!" ; 
		(0.0, 0.0), false
	)
;;
let find_str str (mods : Mod.pcb_module list ) = 
	let rex = Pcre.regexp ~flags:[`CASELESS] str in
	gmatches := Array.of_list (
		List.filter (fun m -> 
			List.exists (fun t -> 
				Pcre.pmatch ~rex (t#getText())
			) (m#getTexts()); 
		) mods 
	) ; 
	gmatchn := 0 ; 
	find_output () 
;;
let find_next callback = 
	gmatchn := !gmatchn + 1 ;  
	let len =  Array.length !gmatches in
	if !gmatchn >= len then gmatchn := 0 ; 
	let c,found = find_output () in
	if found then ( callback c ) ; 
;;
let find_dlg top mods callback = 
	let dlog = Toplevel.create top in
	Wm.title_set dlog "Find";
	let entry = Entry.create ~width:20 dlog in
	Entry.insert ~index:(`Num 0) ~text:"" entry ; 
	Focus.force entry ; (* set the focus so we can start typing immediately *)
	let msg = Message.create ~text:"note: regexp ok!"  ~width:120 dlog in
	let find_cmd _ =
		let c,found = find_str (Entry.get entry) mods in
		if found then ( callback c ) ; 
		Tk.destroy dlog ; 
	in
	bind ~events:[`KeyPressDetail("Return")] ~action:find_cmd entry ;
	bind ~events:[`KeyPressDetail("Escape")] ~action:(fun _ -> Tk.destroy dlog) entry ;
	let but = Button.create ~text:"Find!" ~command:find_cmd dlog in
	Tk.pack ~side:`Top ~fill:`X ~expand:true ~padx:3 ~pady:3 
		[Tk.coe entry; Tk.coe msg; Tk.coe but] ; 
;;