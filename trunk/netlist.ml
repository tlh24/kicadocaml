(* this is just a test file for now - it is *not* in the makefile! *)
open Mod

type token =
    Kwd of string
  | Id of string
  | Int of int
  | Float of float
  | String of string
  | Path of string
  | Char of char

(* this copied & much simplfied from genlex.ml 
yea, I wish it could be simpler too! ..but it works.
I could have done this with Pcre etc, but I wanted to try
out lexing / parsing & learn how it works.  This may be a 
bit faster, too! (have not tested ..) *)

(* The string buffering machinery *)
let initial_buffer = String.create 32 ;;
let buffer = ref initial_buffer ;;
let bufpos = ref 0 ;;
let reset_buffer () = 
	buffer := initial_buffer; 
	bufpos := 0 ;;
let store c =
	if !bufpos >= String.length !buffer then (
		let newbuffer = String.create (2 * !bufpos) in
		String.blit !buffer 0 newbuffer 0 !bufpos; buffer := newbuffer
	); 
	String.set !buffer !bufpos c;
	incr bufpos ;;
let get_string () =
	let s = String.sub !buffer 0 !bufpos in 
	buffer := initial_buffer; 
	s ;;

(* The lexer *)

let soc c = String.make 1 c (* string_of_char *)

let make_lexer () =
	let rec next_token (strm__ : _ Stream.t) =
		match Stream.peek strm__ with
		Some (' ' | '\010' | '\013' | '\009' | '\026' | '\012') ->
			Stream.junk strm__; next_token strm__
		| Some ('(' | ')' | '{' | '}' as c) ->
			Stream.junk strm__; Some (Kwd (soc c)) (* explicit keywords *)
		| Some ('A'..'Z' | 'a'..'z' | '_' | '\192'..'\255' 
			| '0'..'9' | '-' | '.' | ':'  as c) -> (* tlh: added '/', '0'-'9' *)
			Stream.junk strm__;
			let s = strm__ in reset_buffer (); store c; ident s
		| Some
			('!' | '%' | '&' | '$' | '+' | '<' | '=' | '>' | (* tlh: removed '/' ':' '#'*)
			'?' | '@' | '\\' | '~' | '^' | '|' | '*' as c) ->
			Stream.junk strm__;
			let s = strm__ in reset_buffer (); store c; ident2 s
		| Some '"' ->
			Stream.junk strm__;
			let s = strm__ in reset_buffer (); Some (String (string s))
		| Some '#' -> 
			Stream.junk strm__; 
			let s = strm__ in reset_buffer (); Some (String (comment s))
		| Some '/' -> (* don't junk, we want the / *)
			let s = strm__ in reset_buffer (); Some (Path (path s))
		| Some c -> Stream.junk strm__; Some (Id (soc c))
		| _ -> None
	and ident (strm__ : _ Stream.t) =
		match Stream.peek strm__ with
		Some ('A'..'Z' | 'a'..'z' | '_' | '\192'..'\255' | 
			'/' | '0'..'9' | '-' | '.' | ':'  as c) -> (* tlh: added '/' ! - *)
			Stream.junk strm__; let s = strm__ in store c; ident s
		| _ -> Some (Id (get_string ()))
	and ident2 (strm__ : _ Stream.t) =
		match Stream.peek strm__ with
		Some
			('!' | '%' | '&' | '$' | '+' | '<' | '=' | (* removed '/', : - *)
			'>' | '?' | '@' | '\\' | '~' | '^' | '|' | '*' as c) ->
			Stream.junk strm__; let s = strm__ in store c; ident2 s
		| _ -> Some (Id (get_string ()))
	and string (strm__ : _ Stream.t) =
		match Stream.peek strm__ with
		Some '"' -> Stream.junk strm__; get_string ()
		| Some c -> Stream.junk strm__; let s = strm__ in store c; string s
		| _ -> raise Stream.Failure
	and comment (strm__ : _ Stream.t) = 
		match Stream.peek strm__ with
		Some ('\010' | '\012' | '\013' ) -> Stream.junk strm__; get_string ()
		| Some c -> Stream.junk strm__; let s = strm__ in store c; comment s
		| _ -> raise Stream.Failure
	and path (strm__ : _ Stream.t) = 
		match Stream.peek strm__ with
		Some ('A'..'Z' | 'a'..'z' | '_' | '-' | '0'..'9' | '/' as c) -> 
			Stream.junk strm__; let s = strm__ in store c; path s
		| _ -> get_string ()
	in
	fun input -> Stream.from (fun _ -> next_token input)
	
let list_of_stream stream =
	  let result = ref [] in
	  Stream.iter (fun value -> result := value :: !result) stream;
	  List.rev !result

let lexer = make_lexer () 
let lexwith s = list_of_stream (lexer (Stream.of_string s)) 

type expr = Mod of string * string * string * string * expr * expr list 
	| Pin of string * string 
	| Net of string * string
	| Leaf of string 
	| Branch of expr list
	
let rec parse_lib = parser (* note! use the -pp command to preprocess with camlp4o *)
	| [< 'Id "Lib"; 'Id "="; 'Id s >] -> Printf.printf "lib %s\n%!" s; Leaf s
and parse_pinpath = parser (* this to allow either paths or identifiers in the pin netnum *)
	| [< 'Id s >] -> s
	| [< 'Path s >] -> s
and parse_pin = parser
	| [< 'Kwd "("; 'Id s1; s2 = parse_pinpath; 'Kwd ")"; stream >] -> 
		Printf.printf "pin %s %s\n%!" s1 s2;
		(Pin(s1,s2))::(parse_pin stream)
	| [< >] -> []
and parse_expr = parser 
	| [< 'Path s1; 'Id s2; 'Id s3; 'Id s4; 'Kwd "{"; e1=parse_lib; 'Kwd "}"; e2=parse_pin ; stream >] -> 
		Printf.printf "mod %s %s %s %s\n%!" s1 s2 s3 s4; 
		(Mod(s1,s2,s3,s4,e1,e2)) :: (parse_expr stream)
	| [< 'Id "Net" ; 'Id s1 ; 'String s2 ; stream >] -> 
		Printf.printf "net %s %s\n%!" s1 s2; (Net(s1,s2)) :: (parse_expr stream)
	| [< 'String s1 ; stream >] ->
		Printf.printf "r2 %s\n%!" s1; (Leaf s1) :: (parse_expr stream)
	| [< 'Id s1 ; stream >] -> (* blank identifier *)
		Printf.printf "r3 %s\n%!" s1; (Leaf s1) :: (parse_expr stream)
	| [< 'Kwd "("; e = parse_expr; 'Kwd ")"; stream>] -> 
		print_endline "r4"; (Branch e) :: (parse_expr stream)
	| [< 'Kwd "{"; e = parse_expr; 'Kwd "}"; stream >] -> 
		print_endline "r5"; (Branch e) :: (parse_expr stream)
	| [< >] -> [] ;;

let read_netlist fname (boardmods:pcb_module list ref) = 
	let ic = open_in fname in 
	let strm = Stream.of_channel ic in
	let ostrm = lexer strm in
	let modlist = parse_expr ostrm in
	(* ok first clean up the netlist to only contain modules *)
	let mods = ref [] in
	let rec add_mod hd = 
		match hd with
		| (Mod (s1,s2,s3,s4,e1,e2)) -> mods :=  (Mod (s1,s2,s3,s4,e1,e2)) :: !mods 
		| (Branch b) -> get_mods b
		| _ -> ()
	and get_mods lst = 
		match lst with 
		| hd :: tl ->
			add_mod hd; 
			get_mods tl;
		| [] -> ()
	in
	get_mods modlist ; 
	Printf.printf "----------\n%!"; 
	List.iter (fun m -> 
		match m with 
			| Mod(path,foot,refdes,value,_,_) -> 
				Printf.printf "mod %s %s %s %s\n%!" path foot refdes value; 
			| _ -> ()
	) (List.rev !mods);
	(* do the same for the nets. *)
	let nets = ref [] in
	let rec add_net hd = 
		match hd with
		| (Net (s1,s2)) -> (
			let netnum = int_of_string s1 in
			let netname = match s2 with 
			| "" -> Printf.sprintf "N-%06d" netnum
			| ss -> ss in
			nets := (netnum,netname) :: !nets )
		| (Branch b) -> get_nets b
		| _ -> ()
	and get_nets lst = 
		match lst with 
		| hd :: tl ->
			add_net hd; 
			get_nets tl;
		| [] -> ()
	in
	get_nets modlist ;
	Printf.printf "----------\n%!"; 
	(* now stick these into a hashtable to map name to number. *)
	let netnumht = Hashtbl.create (List.length !nets) in
	List.iter (fun n -> 
		let netnum,netname = n in
		Printf.printf "net %d %s\n%!" netnum netname; 
		Hashtbl.add netnumht netname netnum ; 
	) (List.rev !nets);
	(* uh, now we have to check the netlist vs. the board *)
	(* first, check to see if there are any modules in the netlist that 
	aren't on the board *)
	List.iter (fun m -> 
		match m with 
			| Mod(path,foot,refdes,value,_,_) -> 
				if not (List.exists (fun mm -> 
					mm#getPath() = path &&
					mm#getFoot() = foot && 
					mm#getRef() = refdes &&
					mm#getValue() = value) !boardmods) then (
					(* ok, find one with the same footprint, copy and adjust 
					accordingly. *)
					let mf = try 
						Some( List.find (fun m2 -> m2#getFoot() = foot) !boardmods )
						with _ -> 
							Printf.printf "error! foot %s not found - cannot dup\n%!" foot; 
							None
					in
					match mf with
						| Some srcmod -> 
							Printf.printf "Adding module %s foot %s ref %s val %s\n%!"
								path foot refdes value; 
							let newmod = Oo.copy srcmod in
							(* reset the position, copy the value, refdes, then pin nets. *)
							newmod#setPath path; 
							newmod#setRef refdes; 
							newmod#setValue value; 
							newmod#setPos (0.0, 0.0) ; 
							newmod#update (); 
							boardmods := newmod :: !boardmods; 
						| _ -> ()
				); 
			| _ -> ()
	) !mods ; 
	(* second, check to see if there are any modules in the board that are not 
	in the netlist.  if so, remove them. *)
	boardmods := List.filter ( fun mm -> 
		let p = mm#getPath () in
		let f = mm#getFoot () in
		let r = mm#getRef () in
		let v = mm#getValue () in
		let fnd = List.exists (fun m -> 
			match m with 
				| Mod(path,foot,refdes,value,_,_) -> 
					p = path &&
					f = foot && 
					r = refdes &&
					v = value
				| _ -> false
		) !mods in
		if not fnd then Printf.printf 
			"Removing module %s foot %s ref %s val %s\n%!"
			p f r v; 
		fnd
	) !boardmods ;
	Printf.printf "updating the netnames and netnumbers.\n%!"; 
	(* third, update all of the netnames and netnumbers. *)
	List.iter (fun mm -> 
		(* find the match in mods *)
		let p = mm#getPath () in
		let f = mm#getFoot () in
		let r = mm#getRef () in
		let v = mm#getValue () in
		Printf.printf
			"looking at, %s foot %s ref %s val %s \n%!"
			p f r v ; 
		let m = try Some (List.find (fun mx -> 
			match mx with 
				| Mod(path,foot,refdes,value,_,_) ->
					p = path &&
					f = foot && 
					r = refdes &&
					v = value
				| _ -> false
		) !mods) 
		with _ -> Printf.printf
			"Error, %s foot %s ref %s val %s not found in netlist\n%!"
			p f r v ; 
			None
		in
		(match m with
			|  Some ( Mod(_,_,_,_,_,pins)) -> 
				List.iter (fun pin -> 
					match pin with 
					| Pin(pname,pnetname) -> 
						List.iter (fun mpin -> 
							if mpin#getPadName () = pname then (
								let netnum = try Some (Hashtbl.find netnumht pnetname)
								with _ -> None in
								(match netnum with 
								| Some pnn -> 
									Printf.printf "updating pin %s netname %s num %d\n%!"
										pname pnetname pnn;
									mpin#setNetName pnetname; 
									mpin#setNet pnn; 
								| _ -> ()
								)
							)
						) (mm#getPads ()) ;
					| _ -> ()
				) pins; 
				mm#update (); 
			| _ -> ()
		); 
	) !boardmods ; 
	!boardmods 
	(*
							List.iter (fun p -> 
								let pn = p#getPadname () in
								let _,netname = List.find (fun s -> 
									let pdname,nn = s in
									pdname = pn
								) pads in
								p#setNetName netname ; 
								p#setNetNum
							) (newmod#getPads ())
*)
