(* program for reading in a CSV & creating a digikey weborder from it *)
open Printf
open Unix

let debug = false
let ios a = int_of_string a 
let soi a = string_of_int a 
let gsock = ref (socket PF_INET SOCK_STREAM 0) 


let gitline ic = try input_line ic, false 
		with | End_of_file -> "", true ;;

let sock_open () = 
	let hentry = gethostbyname "ordering.digikey.com" in
	gsock := socket PF_INET SOCK_STREAM 0 ;  (* must make a new one here! *)
	connect !gsock (ADDR_INET (hentry.h_addr_list.(0), 80))
	;;

let rec sock_send str =
	let len = String.length str in
	printf "sock_send{\n%s\n}%!" str ; 
	let rec ssend pos iter = 
		if iter > 10 then (
			printf "error! bytes to be sent %d actual %d\n%!" len pos ; 
		) ; 
		let sent = send !gsock str pos (len-pos) [] in (* function returns bytes sent *)
		if pos + sent < len then (
			ssend (pos+sent) (iter + 1)
		) else (
			 printf "success!\n%!";
		) ; 
	in
	ssend 0 0 ;
	;;
	
let check page = 
	(* if the server requested that we close the connection, then close it *)
	let rex = Pcre.regexp ~flags:[`CASELESS] "connection:\s+close" in
	if Pcre.pmatch ~rex page then (
		(try close !gsock with _ -> () ); 
		printf "reopening TCP connection... \n" ; 
		sock_open () ; 
	) ; 
	let webid = try (Pcre.extract ~pat:"WebID\">(\d+)<" page).(1) with _ -> "" in
	let accessid = try (Pcre.extract ~pat:"AccessID\">(\d+)<" page).(1) with _ -> "" in
	if (webid^accessid) <> "" then (
		printf "WebID: %s AccessID:%s\n%!" webid accessid ; 
		printf "go to http://ordering.digikey.com/Ordering/OrderStatus.aspx?web_id=%s&access_id=%s&site=US to view/edit this order\n%!" 
			webid accessid ; 
	) ; 
	(* printf "rxed page{ %s }\n" page; *)
	Unix.sleep 1 ; 
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
	let ret = List.fold_left (fun a b -> a ^ b) "" (rx()) in
	if debug then ( printf "sock_recv2{\n%s\n}\n%!" ret ); 
	ret;;
	
let sock_recv () = 
	(* this one waits until we get the end tag! *)
	let s = ref "" in
	let cnt = ref 0 in
	if debug then ( printf "sock_recv{ \n%!" ); 
	while not (Pcre.pmatch ~pat:"</html>" !s) && !cnt < 5000 do (
		s := !s ^ sock_recv2 () ; 
		for k = 1 to 1200 do printf "" done ; 
		incr cnt ; 
	) done ; 
	if !cnt > 5000 then (
		printf "error : timeout!\n" ; 
		raise Not_found ; 
	) ; 
	if debug then (printf "\n}\n%!" ); 
	!s ;;

let _ = 
	if Array.length (Sys.argv) != 4 then (
		printf "please specify the CSV file to be read, \n"; 
		printf " followed by the column number of the digikey part number\n"; 
		printf " followed by the column number of the part count (quantity)\n"; 
		printf " (note: columns start at 0)\n"; 
		printf "e.g: ./digikeyweborder bom_digikey.csv 2 5\n"; 
		printf "will return a WebID and AccessID to use.\n%!"; 
	) else (
	let source = Sys.argv.(1) in
	let pn_col = ios ( Sys.argv.(2)) in
	let pc_col = ios ( Sys.argv.(3)) in
	(* read in the source file *)
	let ic = open_in source in
	let eof = ref false in 
	let pl = ref [] in (* part list *)
	while not !eof do (
		let line, eoff = gitline ic in
		let a = Array.of_list (Pcre.split ~pat:"," line) in
		let cnt = try ios a.(pc_col) with _ -> 0 in
		let pn = try a.(pn_col) with _ -> "" in
		let pnok = Pcre.pmatch ~pat:"-ND" pn in
		if cnt > 0  && pnok then (
			pl := ( a.(pn_col) , cnt ) :: !pl ; 
		) else (
			printf "ignoring: pn:%s qty:%d \n\t(line:%s)\n%!" pn cnt line ; 
		); 
		eof := eoff ; 
	) done ; 
	printf "everything look ok? (y/n)\n%!"; 
	if read_line () <> "y" then ( exit 0 ); 
	(* echo the contents of the file *)
	List.iter (fun (pn,pc) -> 
		printf " %s , %d\n%!" pn pc ;  
	) (List.rev !pl) ; 
	
	(* ok, now open a connection *)
	printf "opening a connection.. \n%!"; 
	sock_open () ; 
	
	let httphdr = ("HTTP/1.1\r\n"^
		"Host: ordering.digikey.com\r\n"^
		"Accept: text/html,application/xhtml+xml,application/xml\r\n"^
		"Accept-language: en-us,en;q=0.5\r\n"^
		"Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7"^
		"Keep-Alive: 300\r\n"^
		"Connection: keep-alive\r\n"^
		"Referer: http://ordering.digikey.com/Ordering/AddPart.aspx"^
		"Content-Type: application/x-www-form-urlencoded\r\n") in
		
	(* get a cookie! *)
	sock_send ("GET http://ordering.digikey.com/"^
		"Ordering/AddPart.aspx?site=us&lang=en&WT.z_header=link "^httphdr^"\r\n");  
	let page1 = sock_recv () in
	(* server ID seems to be critical thing here. *)
	let cookie= (Pcre.extract ~pat:"sid=([^;]+);" page1).(1) in
	printf "Cookie %s\n" cookie; 
	(* add some local stuff to the cookie? *)
	let cookie = "WT_FPC=id=" ^ (string_of_float (Random.float 1000000.0))^
		"; sid="^cookie^"; cur=USD" in 
	
	List.iter (fun (pn,pc) -> 
		let count = soi pc in
		let part1,fnd = try (Pcre.extract ~pat:"([^\"]+)" pn).(1),true with _ -> "",false in
		if fnd then (
			let part = Pcre.replace ~pat:" " ~templ:"" part1 in
			sock_send (
                                "GET " ^
                                "/Ordering/AddPart.aspx?part="^(Netencoding.Url.encode part)^
                                "&qty="^count^"&cref= "^
                                httphdr ^
                                "Cookie: " ^ cookie ^ ";\r\n\r\n" );
			let res = sock_recv () in 
			check res ; 
			 if Pcre.pmatch ~pat:"302 Moved" res then (
                                (* start out by suggesting the next increment of 10 *)
                                (* this may work frequently, and will save time *)
				(* this willredirect to the following page.. *)
				sock_send (
					"GET /Ordering/LineItem.aspx?"^
					"part="^(Netencoding.Url.encode part)^
					"&qty="^count^
					"&cref=&ErrorCount=1&ErrorType1=Text&ErrorKey1=Minumum+Quantity+Not+Met "^
					httphdr ^ 
					"Cookie: " ^ cookie ^ ";\r\n\r\n" ); 
				let res = sock_recv () in 
				check res ;
                                let sug = ((ios count)/10 +1)*10 in
				sock_send (
					"GET " ^
					"/Ordering/AddPart.aspx?part="^(Netencoding.Url.encode part)^
					"&qty="^(soi sug)^"&cref= "^
					httphdr ^
					"Cookie: " ^ cookie ^ ";\r\n\r\n" );
				let res = sock_recv () in (* and this, to the old addpart page. probably. *)
				check res ; 
				 if Pcre.pmatch ~pat:"302 Moved" res then (
					printf "could not figure out appropriate qty for part %s count %s\n%!"
						part count ; 
					raise  Not_found "Invalid quantity"
				 ); 
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
	