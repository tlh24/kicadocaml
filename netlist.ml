(* this is just a test file for now - it is *not* in the makefile! *)
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
bit faster, too! (have not tested .. *)

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
		| Some ('A'..'Z' | 'a'..'z' | '_' | '\192'..'\255' | 
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
			'/' | '0'..'9' | '-' | '.' | ':'  as c) -> (* tlh: added '/' ! *)
			Stream.junk strm__; let s = strm__ in store c; ident s
		| _ -> Some (Id (get_string ()))
	and ident2 (strm__ : _ Stream.t) =
		match Stream.peek strm__ with
		Some
			('!' | '%' | '&' | '$' | '+' | '-' | '<' | '=' | (* removed '/', : *)
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
		Some ('A'..'Z' | 'a'..'z' | '_' | '0'..'9' | '/' as c) -> 
			Stream.junk strm__; let s = strm__ in store c; path s
		| _ -> get_string ()
	in
	fun input -> Stream.from (fun count -> next_token input)
	
let list_of_stream stream =
	  let result = ref [] in
	  Stream.iter (fun value -> result := value :: !result) stream;
	  List.rev !result

let lexer = make_lexer () 
let lexwith s = list_of_stream (lexer (Stream.of_string s)) 

let ic = open_in "/home/tlh24/svn/neurorecord/microstim/microstim.net" 
let strm = Stream.of_channel ic 
(* list_of_stream (lexer strm)*)

let ostrm = lexer strm

type expr = Mod of string * string * string * string * expr * expr list 
	| Pin of string * string 
	| Leaf of string 
	| Branch of expr list
	
let rec parse_lib = parser
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
	| [< 'String s1 ; stream >] ->
		Printf.printf "r2 %s\n%!" s1; (Leaf s1) :: (parse_expr stream)
	| [< 'Id s1 ; stream >] -> (* blank identifier *)
		Printf.printf "r3 %s\n%!" s1; (Leaf s1) :: (parse_expr stream)
	| [< 'Kwd "("; e = parse_expr; 'Kwd ")"; stream>] -> 
		print_endline "r4"; (Branch e) :: (parse_expr stream)
	| [< 'Kwd "{"; e = parse_expr; 'Kwd "}"; stream >] -> 
		print_endline "r5"; (Branch e) :: (parse_expr stream)
	| [< >] -> [] ;;
	
let modlist = parse_expr ostrm

let check_modules () = 
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
	List.rev !mods
in
(* uh, now we have to check the netlist vs. the board *)
(* I'll do that later .. *)

