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
*)open Printf
open Comm (* common functions *)
open Mod

class component = 
object 
	val mutable m_ts = ""; (* U 456AD34E *)
	val mutable m_pos = (0.0, 0.0); 
	val mutable m_ref = ""; (* F0 *)
	val mutable m_type = ""(* F1 *)
	val mutable m_footprint = ""; (* F2 *)
	val mutable m_arlut = []; (* AR Path= ... *)
	val mutable m_path = ""; (* path *does not* include the module's timestamp *)
		(* since that is easy to just tag onto the end *)
	
	method read ic = (
		(* this is called after the $comp line. *)
		let line = ref (input_line2 ic) in
		let parse pattern  = 
			Pcre.extract ~pat:pattern !line
		in
		while not (Pcre.pmatch ~pat:"\$EndComp" !line ) do (
			let first = String.get !line 0 in
			(match first with 
				| 'U' -> (
					let sp = parse "U ([\d-]+) ([\d-]+) (\w+)" in
					m_ts <- sp.(3) ; 
				)
				| 'A' -> (
					let sp = parse "AR Path=\"([^\"]+)\" Ref=\"([^\"]+)" in
					m_arlut <- ( (sp.(1)) , (sp.(2)) ) :: m_arlut ; 
				)
				| 'P' -> (
					let sp = parse "P ([\d-]+) ([\d-]+)" in
					m_pos <- (fos sp.(1)),(fos sp.(2)) ; 
				)
				| 'F' -> (
					let sp = parse "F (\d+) " in
					let n = ios sp.(1) in
					(match n with 
						| 0 -> (
							let sp2 = parse "F 0 \"([^\"]+)\"" in
							m_ref <- sp2.(1); 
						)
						| 1 -> (
							let sp2 = parse "F 1 \"([^\"]+)\"" in
							m_type <- sp2.(1); 
						)
						| 2 -> (
							let sp2 = parse "F 2 \"([^\"]+)\"" in
							m_footprint <- sp2.(1); 
						)
						| _ -> ()
					)
				)
				| _ -> () 
			) ; 
			line := input_line2 ic ; 
		) done
	)
	method print tabs = (
		printf "%s component value %s ref %s footprint %s\n%!"
			tabs m_type m_ref m_footprint ; 
	)
	method collapseAr path = (
		(* use the AR LUT to determine the proper name of this component *)
		if m_path = "" && List.length m_arlut > 0 then (
			let fp =  path ^ "/" ^ m_ts in (* the 'full' path *)
			(* printf "collapseAr full path %s \n%!" fp ; *)
			let (_, rf) = List.find (fun (p, _) -> p = fp ) m_arlut in
			(* printf "collapseAr in component path %s ref %s\n%!" fp rf; *)
			m_path <- path ; 
			m_ref <- rf ;
			() (* don't need to return anything *)
		)
	)
	method getTimeStamp () = ( m_ts )
	method getPos () = ( m_pos )
end

class sheet =
object 
	val mutable m_ts = "" (* U 456AD34E *)
	val mutable m_ref = "" (* F0 *)
	val mutable m_fname = "" (* F1 *)
	
	method getFname () = m_fname
	method getTs () = m_ts 
	method getRef () = m_ref
	
	method read ic = (
		let line = ref (input_line2 ic) in
		let parse pattern  = 
			Pcre.extract ~pat:pattern !line
		in
		while not (Pcre.pmatch ~pat:"\$EndSheet" !line ) do (
			let s = parse "(\w+)\s([^\s]+)" in
			let first = s.(1) in
			let second = s.(2) in
			(match first with 
				| "U" -> ( m_ts <- second ; )
				| "F0" -> ( 
					let sp = Pcre.extract ~pat:"\"([^\"]+)" second in
					m_ref <- sp.(1) )
				| "F1" -> ( 
					let sp = Pcre.extract ~pat:"\"([^\"]+)" second in
					m_fname <- sp.(1) )
				| _ -> ()
			); 
			line := input_line2 ic ; 
		) done
	)
end

class schematic = 
object (self)
	val mutable m_components = [] ; (* another empty list *)
	val mutable m_sheets = [] ; 
	val mutable m_subSch = [] ; 
	val mutable m_ts = ""
	val mutable m_ref = "" (* the name of the schematic if it is sub to another *)
	val mutable m_fileName = ""
	
	method getRef () = m_ref
	method setRef r = m_ref <- r
	method setTs t = m_ts <- t
	method getFileName () = m_fileName
	
	method copy () = (
		(* this sholud be called after Oo.copy creates this object. *)
		let compnew = List.map Oo.copy m_components in
		m_components <- compnew ; 
		(* components contain no references, 
		so no need for calling the new objects copy method*)
		let sheetsnew = List.map Oo.copy m_sheets in
		m_sheets <- sheetsnew; 
		(* ditto above *)
		let subschnew = List.map Oo.copy m_subSch in
		List.iter (fun ss -> ss#copy()) subschnew; 
		m_subSch <- subschnew ; 
		(* this one requires recursive copy *)
		(* better hope there are no loops in the hierarchy! *)
	)
	
	method openFile fname ts fref schematic_list = (
		(* clear out the previous hierarchy *)
		m_components <- []; 
		m_sheets <- []; 
		m_subSch <- []; 
		(* read it in! *)
		(* printf "schematic.ml : openFile %s \n%!" fname ; *)
		let ic = open_in fname in
		linenum := 0 ;
		gfilereadname := fname ;
		m_fileName <- fname ; 
		m_ts <- ts ; 
		m_ref <- fref ;  
		self#read ic schematic_list; 
		close_in_noerr ic; 
	)
	
	method read ic schematic_list = (
		let line = ref (input_line2 ic) in
		while not (Pcre.pmatch ~pat:"\$EndSCHEMA" !line) do (
			(match !line with 
				| "$Sheet" -> (
					(* printf "new sheet line %d file %s\n%!" !linenum !gfilereadname; *)
					let s = new sheet in
					s#read ic ; 
					m_sheets <- s :: m_sheets ; 
				)
				| "$Comp" -> (
					(* printf "new component line %d file %s\n%!" !linenum !gfilereadname; *)
					let c = new component in 
					c#read ic ;
					m_components <- c :: m_components ; 
				)
				| _ ->  () (*printf "line %d %s \n%!" !linenum !line ;  *)
			) ; 
			line := input_line2 ic ; 
		) done ; 
		(* before loading the sub-sheets, 
		need to extract the file-system directory from the file name. *)
		let lastslash = String.rindex m_fileName '/' in
		let dir = String.sub m_fileName 0 (lastslash+1) in
		(* now we need to iterate over the sheets and make them into schematics. *)
		List.iter (fun ss -> 
			(* check to see if this has already been read in. 
			this saves time for highly hierarchal projects*)
			let fname = dir ^ (ss#getFname ()) in
			let extant = try Some (List.find 
				(fun s -> s#getFileName () = fname) 
				!schematic_list ) 
				with _ -> None in
			let schem = (match extant with 
				| Some schem1 -> (
					(* printf "copy found of schematic %s\n%!" fname; *)
					let schem2 = Oo.copy schem1 in
					(* we must also copy the references - components, sheets, subsheets *)
					(* components must be copied since they have different path/refdes *)
					schem2#copy (); 
					schem2#setRef (ss#getRef ());
					schem2#setTs (ss#getTs ()); 
					schem2
				)
				| None -> (
					let schem2 = new schematic in 
					schem2#openFile fname (ss#getTs()) (ss#getRef()) schematic_list;
					schematic_list := schem2 :: !schematic_list; 
					(* keep a running list of files we've read in *)
					schem2
				)
			) in
			m_subSch <- schem :: m_subSch ; 
		) m_sheets ; 
	)
	
	method print tabs = (
		printf "%s schematic name %s source file %s timestamp %s\n%!"
			tabs m_ref m_fileName m_ts ; 
		let t2 = tabs ^ "    " in
		List.iter (fun c -> c#print t2 ) m_components ; 
		List.iter (fun c -> c#print t2 ) m_subSch ; 
	)
	
	method getFp path = (
		if m_ts = "00000000" then path else path ^ "/" ^ m_ts
	)
	
	method collapseAr path = (
		let fp = self#getFp path in
		(*printf "path %s file %s\n%!" fp m_fileName;*)
		List.iter (fun c -> c#collapseAr fp) m_components ; 
		List.iter (fun s -> s#collapseAr fp) m_subSch ; 
	)
	
	method findSubSch name path = (
		(* returns if the sheet is found then the AR path of a given sub-schematic *)
		(* path should start as "" *)
		let fp = self#getFp path in
		if name = m_ref then 
			(true, fp)
		else (
			List.fold_left (fun a s -> 
				let (found, ffp) = s#findSubSch name fp in
				if found then (true, ffp) 
				else a
			) (false, "") m_subSch
		)
	)
	
	method findSubSch2 name = (
		(* returns an schematic object if a match is found; 
		otherwise, Not_found exception *)
		if List.length m_subSch = 0 then raise Not_found ; 
		List.fold_left (fun a b -> 
			if b#getRef () = name then b 
			else (
				try b#findSubSch2 name with _ -> a
			)
		) (List.hd m_subSch) m_subSch
	)
	
	method componentPositon (ts:string) = (
		(* find the component position *)
		let comp =
			try Some (List.find (fun c ->
				c#getTimeStamp () = ts
			) m_components )
			with Not_found -> (Printf.printf "comonent not found!\n"; None)
		in
		(match comp with 
			| Some c -> Some (c#getPos ()) ;
			| None -> None ;
		)
	)
end

(* global schematic *)
let gschema = new schematic
