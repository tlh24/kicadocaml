(* program for reading in a CSV & creating a digikey weborder from it *)
open Printf
open Unix

let ios a = int_of_string a ;;
let soi a = string_of_int a ;;
let gsock = ref (socket PF_INET SOCK_STREAM 0) ;;

let gitline ic = try input_line ic, false 
		with | End_of_file -> "", true ;;

let sock_open () = 
	let hentry = gethostbyname "ordering.digikey.com" in
	gsock := socket PF_INET SOCK_STREAM 0 ;  (* must make a new one here! *)
	connect !gsock (ADDR_INET (hentry.h_addr_list.(0), 80))
	;;

let rec sock_send str =
	let len = String.length str in
	printf "sending %s\n%!" str ; 
	let sent = send !gsock str 0 len [] in (* function returns bytes sent *)
	if sent < len then (
		printf "error! bytes to be sent %d actual %d\n%!" len sent ; 
		(try close !gsock with _ -> () ); 
		sock_open () ; 
		(* try again .. *)
		sock_send str ; 
	) else ( printf "success!\n"; ) 
	;;
	
let check page = 
	(* if the server requested that we close the connection, 
	then close it *)
	let rex = Pcre.regexp ~flags:[`CASELESS] "connection:\s+close" in
	if Pcre.pmatch ~rex page then (
		(try close !gsock with _ -> () ); 
		printf "reopening TCP connection... \n" ; 
		sock_open () ; 
	) ; 
	printf "pause for checking ... \n%!" ; 
	(* ignore(read_line () ) *)
	;;

let rec sock_recv2 () =
	(* note!!  Unix.recv can only grab 16k at a time! *)
	let rec rx () = 
		let str = String.create (1024*16) in
		let recvlen = recv !gsock str 0 (1024*16) [] in
		if recvlen = 0 then (
			printf "error! no bytes received\n"  ; 
			close !gsock ; 
			sock_open () ; 
			(* try again .. *)
			rx()  
		) else (
			(* printf "recieved %d\n" recvlen ; *)
			if recvlen = (1024*16) then str :: (rx ())
			else [ ( String.sub str 0 recvlen) ]
		)
	in
	List.fold_left (fun a b -> a ^ b) "" (rx()) ;;
	
let sock_recv () = 
	(* this one waits until we get the end tag! *)
	let s = ref "" in
	let cnt = ref 0 in
	printf "waiting for response .. \n%!" ; 
	while not (Pcre.pmatch ~pat:"</html>" !s) && !cnt < 5000 do (
		s := !s ^ sock_recv2 () ; 
		for k = 1 to 1200 do printf "" done ; 
		incr cnt ; 
	) done ; 
	if !cnt > 5000 then (
		printf "error : timeout!\n" ; 
		raise Not_found ; 
	) ; 
	!s ;;

let _ = 
	if Array.length (Sys.argv) != 6 then (
		printf "please specify the CSV file to be read, \n"; 
		printf " followed by the column number of the digikey part number\n"; 
		printf " followed by the column number of the part count\n"; 
		printf " (note: columns start at 0)\n"; 
		printf "followed by the web_id\n" ; 
		printf "followed by the access_id (both from digikey webpage)\n" ; 
		printf "e.g: ./digikeyweborder bom_digikey.csv 2 5 26983993 41138\n"; 
	) else (
	let source = Sys.argv.(1) in
	let pn_col = ios ( Sys.argv.(2)) in
	let pc_col = ios ( Sys.argv.(3)) in
	let web_id = Sys.argv.(4) in
	let access_id = Sys.argv.(5) in
	(* read in the source file *)
	let ic = open_in source in
	let eof = ref false in 
	let pl = ref [] in (* part list *)
	while not !eof do (
		let line, eoff = gitline ic in
		let a = Array.of_list (Pcre.split ~pat:"," line) in
		let cnt = try ios a.(pc_col) with _ -> 0 in
		if cnt > 0 then (
			pl := ( a.(pn_col) , cnt ) :: !pl ; 
		) ; 
		eof := eoff ; 
	) done ; 
	(* echo the contents of the file *)
	List.iter (fun (pn,pc) -> 
		printf " %s , %d\n%!" pn pc ;  
	) (List.rev !pl) ; 
	
	(* ok, now open a connection *)
	printf "opening a connection.. \n%!"; 
	sock_open () ; 
	
	let httphdr = ("HTTP/1.1\r\n"^
		"Host: ordering.digikey.com\r\n"^
		"accept: text/html,application/xhtml+xml,application/xml\r\n"^
		"accept-language: en-us,en;q=0.5\r\n"^
		"Connection: keep-alive\r\n") in
		
	let access = "?web_id="^web_id^"&access_id="^access_id^" " in
	(* 1. get a cookie & present the order we want to work on *)
	sock_send ( "GET /Ordering/OrderStatus.aspx"^access^httphdr^"\r\n" ) ;
	let page1 =  sock_recv () in
	printf "\n page1: \n%s%!" page1 ; 
	check page1 ; 
	let cookie= (Pcre.extract ~pat:"Set-Cookie: ([^;]+);" page1).(1) in
	(* add some local stuff to the cookie? *)
	let cookie = cookie ^ "; WT_FPC=id=152.16.229.48-2070911184.2998612" in

	(* 2. 'click' on the ResumeOrder link *)
	sock_send ("GET /Ordering/ResumeOrder.aspx"^access^httphdr^
		"Cookie: " ^ cookie ^ "\r\n\r\n");  (* we have accepted a cookie! *)
	(* this should redirect *)
	let redirect = sock_recv () in
	printf "\n redirect: \n%s%!" redirect ; 
	check redirect ; 
	(* now go to the actual order (we ignore the content of the redirect)*)
	sock_send (
		"GET /Ordering/AddPart.aspx "^httphdr ^
		"Cookie: " ^ cookie ^ "\r\n\r\n" ); 
	(* this should get the order *)
	let order = sock_recv () in
	printf "\n order: \n%s%!" order ; 
	check order ; 
	let viewstate = (Pcre.extract ~pat:"__VIEWSTATE\" value=\"([^\"]+)\"" order).(1) in
	let eventvalid = (Pcre.extract ~pat:"__EVENTVALIDATION\" value=\"([^\"]+)\"" order).(1) in
	
	let aspctl = "&ctl00%24ctl00%24" in
	let aspjnk = aspctl ^ "mainContentPlaceHolder%24mainContentPlaceHolder%24" in
	
	(* set up a handler in case the minimum qty is not met *)
	let rec checkMin ?(suggest=0) part count = 
		(* assumes that we already got a 302 redirect to LineItem.aspx *)
		sock_send (
			"GET /Ordering/LineItem.aspx?"^
			"part="^part^
			"&qty="^count^
			"&cref=&ErrorCount=1&ErrorType1=Text&ErrorKey1=Minumum%20Quantity%20Not%20Met "^
			httphdr ^ 
			"Cookie: " ^ cookie ^ ";\r\n\r\n" ); 
		let avail = sock_recv () in 
		printf "\n avail: \n%s" avail ; 
		check avail; 
		(* have to figure out what to do with these .. 
		right now we ignore the content, too much asp junk *)
		printf "part %s minimum order not met; present qty %s\n%!" part count ; 
		printf "please enter a new quantity: " ; 
		let newcount = if suggest <> 0 then soi suggest else  read_line () in
		let content = (
			"__LASTFOCUS=&__EVENTTARGET=&__EVENTARGUMENT=&__VIEWSTATE="^
			(Netencoding.Url.encode viewstate) ^
			aspctl ^ "txtHeaderPartSearch="^ 
			aspjnk^"txtQuantity="^newcount^
			aspjnk^"txtCustomerReference="^
			aspjnk^"btnAddToOrder=Add+to+Order"^
			"&__SCROLLPOSITIONX=0&__SCROLLPOSITIONY=0" ^
			"&__EVENTVALIDATION=" ^ (Netencoding.Url.encode eventvalid)
			) in
		sock_send (
			"POST /Ordering/LineItem.aspx?"^
			"part="^(Netencoding.Url.encode part)^
			"&qty="^count^ (* this is not updated *)
			"&cref=&ErrorCount=1&ErrorType1=Text&ErrorKey1=Minumum%20Quantity%20Not%20Met "^
			httphdr ^ 
			"Cookie: " ^ cookie ^ ";\r\n" ^
			"Content-Type: application/x-www-form-urlencoded\r\n" ^
			"Content-Length: " ^ (string_of_int (String.length content)) ^
			"\r\n\r\n" ^
			content ) ;
		let redir2 = sock_recv () in
		printf "\n redir2: \n%s" redir2 ; 
		check redir2 ; 
		(* this will redirect back to AddPart.aspx *)
		sock_send (
			"GET /Ordering/AddPart.aspx?"^
			"part="^(Netencoding.Url.encode part)^
			"&qty="^newcount^
			"&cref= "^
			httphdr ^ 
			"Cookie: " ^ cookie ^ ";\r\n\r\n" ); 
		let res = sock_recv () in
		printf "\n res: \n%s" res ; 
		check res; 
		if Pcre.pmatch ~pat:"302 Moved" res then ( 
			checkMin part newcount ; 
		) ; 
	in
	
	List.iter (fun (pn,pc) -> 
		let count = soi pc in
		let part1,fnd = try (Pcre.extract ~pat:"([^\"]+)" pn).(1),true with _ -> "",false in
		if fnd then (
			let part = Pcre.replace ~pat:" " ~templ:"" part1 in
			let content = (
				"__LASTFOCUS=&__EVENTTARGET=&__EVENTARGUMENT=&__VIEWSTATE="^
				(Netencoding.Url.encode viewstate) ^
				aspctl ^ "txtHeaderPartSearch="^ 
				aspjnk^"txtQuantity="^count^
				aspjnk^"txtPartNumber="^(Netencoding.Url.encode part)^
				aspjnk^"txtCustomerReference="^
				aspjnk^"btnAddToOrder=Add+to+Order"^
				"&__SCROLLPOSITIONX=0&__SCROLLPOSITIONY=0" ^
				"&__EVENTVALIDATION=" ^ (Netencoding.Url.encode eventvalid)
				) in
			printf "\n content: \n%s\n%!" content ; 
			check ""; 
			sock_send (
				"POST " ^
				"/Ordering/AddPart.aspx?site=US&source=search "^
				httphdr ^
				"Cookie: " ^ cookie ^ ";\r\n" ^
				"Content-Type: application/x-www-form-urlencoded\r\n" ^
				"Content-Length: " ^ (soi (String.length content)) ^
				"\r\n\r\n" ^
				content ) ; 
			let res = sock_recv () in
			printf "\n res: \n%s" res ; 
			check res; 
			if Pcre.pmatch ~pat:"302 Moved" res then (
				(* start out by suggesting the next increment of 10 *)
				(* this may work frequently, and will save time *)
				let sug = ((ios count)/10 +1)*10 in
				checkMin ~suggest:sug part count ; 
			) else (
				if Pcre.pmatch ~pat:"Invalid Part Number" res then (
					printf "!! invalid part number: %s\n%!" part; 
					raise  Not_found "Invalid Part Number"
				) else (
					printf "added part %s count %s to order!\n%!" part count ; 
				) ; 
			) ;
		) ; 
	) (List.rev !pl) ; 
	close !gsock ; 
);;
	