(* 
	This is just a test file. 
	I wrote it while trying to figure out how to interact with 
	Digi-key's IIS webservers. 
	It probably no longer works, but may be a source of ideas?
*)

open Unix ;;
open Printf ;;

let sock_send sock str =
	let len = String.length str in
	send sock str 0 len [] ;;

let sock_recv sock =
	(* note!!  Unix.recv can only grab 16k at a time! *)
	let rec rx () = 
		let str = String.create (1024*16) in
		let recvlen = recv sock str 0 (1024*16) [] in
		printf "recieved %d\n" recvlen ;
		if recvlen = (1024*16) then str :: (rx ())
		else [ ( String.sub str 0 recvlen) ]
	in
    List.fold_left (fun a b -> a ^ b) "" (rx()) ;;

let hentry = gethostbyname "ordering.digikey.com" ;;

let client_sock = socket PF_INET SOCK_STREAM 0 ;;
connect client_sock (ADDR_INET (hentry.h_addr_list.(0), 80)) ;;

(* sock_recv client_sock 1024 ;;  clear the pipe? *)
(*
sock_send client_sock (
	"GET " ^ 
	"/ordering/addpart.aspx?site=US&source=search "^
	"HTTP/1.1\r\n"^
	"Host: ordering.digikey.com\r\n"^
	"accept: text/html,application/xhtml+xml,application/xml\r\n"^
	"accept-language: en-us,en;q=0.5\r\n" ^
	"\r\n");; *)

let httphdr = ("HTTP/1.1\r\n"^
	"Host: ordering.digikey.com\r\n"^
	"accept: text/html,application/xhtml+xml,application/xml\r\n"^
	"accept-language: en-us,en;q=0.5\r\n" ) ;;

(* 1. get a cookie & present the order we want to work on *)
sock_send client_sock (
	"GET /Ordering/OrderStatus.aspx?web_id=26983993&access_id=41138 "^
	httphdr ^
	"\r\n") ;;
let page1 =  sock_recv client_sock ;;
let cookie= (Pcre.extract ~pat:"Set-Cookie: ([^;]+);" page1).(1) ;; 
(* add some local stuff to the cookie? *)
let cookie = cookie ^ "; WT_FPC=id=152.16.229.48-2070911184.2998612" ;; 

(* 2. 'click' on the ResumeOrder link *)
sock_send client_sock (
	"GET /Ordering/ResumeOrder.aspx?web_id=26983993&access_id=41138 "^
	httphdr ^
	"Cookie: " ^ cookie ^ "\r\n" ^ (* we have accepted a cookie *)
	"\r\n");;	
	
(* this should redirect *)
let redirect = sock_recv client_sock  ;; 

(* now go to the actual order*)
sock_send client_sock (
	"GET /Ordering/AddPart.aspx "^
	httphdr ^
	"Cookie: " ^ cookie ^ "\r\n" ^
	"\r\n");;	
	
(* this should get the order *)
let order = sock_recv client_sock ;; 

(* need to extract the ajax / javascript feilds. *)
let viewstate = (Pcre.extract ~pat:"__VIEWSTATE\" value=\"([^\"]+)\"" order).(1) ;; 
let eventvalid = (Pcre.extract ~pat:"__EVENTVALIDATION\" value=\"([^\"]+)\"" order).(1) ;; 

(* uh .. try? *)
let aspctl = "&ctl00%24ctl00%24" ;;
let aspjnk = aspctl ^ "mainContentPlaceHolder%24mainContentPlaceHolder%24" ;;
let part = "399-3520-1-ND" ;;
let count = (string_of_int 10);;
let content = (
	"__LASTFOCUS=&__EVENTTARGET=&__EVENTARGUMENT=&__VIEWSTATE="^
	(Netencoding.Url.encode viewstate) ^
	aspctl ^ "txtHeaderPartSearch="^ 
	aspjnk^"txtQuantity="^count^
	aspjnk^"txtPartNumber="^part^
	aspjnk^"txtCustomerReference="^
	aspjnk^"btnAddToOrder=Add+to+Order"^
	"&__SCROLLPOSITIONX=0&__SCROLLPOSITIONY=0" ^
	"&__EVENTVALIDATION=" ^ (Netencoding.Url.encode eventvalid)
	) ;;
sock_send client_sock (
	"POST " ^
	"/Ordering/AddPart.aspx?site=US&source=search "^
	httphdr ^
	"Cookie: " ^ cookie ^ ";\r\n" ^
	"Content-Type: application/x-www-form-urlencoded\r\n" ^
	"Content-Length: " ^ (string_of_int (String.length content)) ^
	"\r\n\r\n" ^
	content ) ;;

let res = sock_recv client_sock ;;

(* now need to see if the result indicates that we need to order more *)
if Pcre.pmatch ~pat:"302 Moved" res then (
	sock_send client_sock (
		"GET /Ordering/LineItem.aspx?"^
		"part="^part^
		"&qty="^count^
		"&cref=&ErrorCount=1&ErrorType1=Text&ErrorKey1=Minumum%20Quantity%20Not%20Met "^
		httphdr ^ 
		"Cookie: " ^ cookie ^ ";\r\n" ^
		"\r\n" ) ;;
	let avail =  sock_recv client_sock ;; (* have to figure out what to do with these .. *)
	let newcount = "10"; 
	let content = (
		"__LASTFOCUS=&__EVENTTARGET=&__EVENTARGUMENT=&__VIEWSTATE="^
		(Netencoding.Url.encode viewstate) ^
		aspctl ^ "txtHeaderPartSearch="^ 
		aspjnk^"txtQuantity="^newcount^
		aspjnk^"txtCustomerReference="^
		aspjnk^"btnAddToOrder=Add+to+Order"^
		"&__SCROLLPOSITIONX=0&__SCROLLPOSITIONY=0" ^
		"&__EVENTVALIDATION=" ^ (Netencoding.Url.encode eventvalid)
		) ;;
	sock_send client_sock (
		"POST /Ordering/LineItem.aspx?"^
		"part="^part^
		"&qty="^count^
		"&cref=&ErrorCount=1&ErrorType1=Text&ErrorKey1=Minumum%20Quantity%20Not%20Met "^
		httphdr ^ 
		"Cookie: " ^ cookie ^ ";\r\n" ^
		"Content-Type: application/x-www-form-urlencoded\r\n" ^
		"Content-Length: " ^ (string_of_int (String.length content)) ^
		"\r\n\r\n" ^
		content ) ;;
	let redir2 = sock_recv client_sock ;; 
	(* this will redirect back to AddPart.aspx *)
	sock_send client_sock (
		"GET /Ordering/AddPart.aspx?"^
		"part="^part^
		"&qty="^count^
		"&cref= "^
		httphdr ^ 
		"Cookie: " ^ cookie ^ ";\r\n" ^
		"\r\n" ) ;;
	let uporder = sock_recv client_sock
	if Pcre.pmatch ~pat:"302 Moved" uporder then   
) ;;

close client_sock ;;