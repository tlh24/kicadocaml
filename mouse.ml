open Comm
open Tk
open Printf

let mouseMoveQ = Stack2.create () ;;
let mousePressQ = Stack2.create () ;;
let mouseReleaseQ = Stack2.create () ;;
let gbutton1pressed = ref false

let bindMove top ~action = 
	(* keep these things on a stack, so that we can push / pop them 
	e.g. when the user presses 'shift', we start a selection box
	and if the user then presses the right mouse button, we pan. 
	 this is only for actions that require listening to the cursor position 
	the default is to simply update the cursor position, of course. *)
	Stack2.push action mouseMoveQ ; 
	bind ~events:[`Motion] ~fields:[`MouseX; `MouseY] ~action:action top;
	;;

let releaseMove top = 
	let action = Stack2.pop_nxt mouseMoveQ in (* pop's then returns the next entry *)
	bind ~events:[`Motion] ~fields:[`MouseX; `MouseY] ~action:action top;
	;;

let rec bindPress top ~onPress ~onRelease = 
	Stack2.push onPress mousePressQ ;	
	Stack2.push onRelease mouseReleaseQ ;	
	
	bind ~events:[`ButtonPressDetail(1)] ~fields:[`MouseX; `MouseY] ~action:
		(fun ev -> 
			(* let x = ev.ev_MouseX in *)
			let y = ev.ev_MouseY in
			(* printf "button press @ x:%d, y:%d\n%!" x y ; *)
			let _, h = !gwindowsize in
			(* clear any lingering presses *)
			if !gbutton1pressed then (
				gbutton1pressed := false ; 
				onRelease ev; 
			) ; 
			(* don't accept presses in the title bar *)
			if y > 32 && y < h - 32 then ( 
				gbutton1pressed := true; 
				onPress ev; 
			) ; 
		) top ; 
	bind ~events:[`ButtonReleaseDetail(1)] ~fields:[`MouseX; `MouseY] ~action:
		(fun ev -> 
			(* printf "button release @ x:%d, y:%d\n%!" (ev.ev_MouseX) (ev.ev_MouseY) ; *)
			if !gbutton1pressed then (
				gbutton1pressed := false ; 
				onRelease ev; 
			); 
		) top ;
	;;

let rec releasePress top = 
	(* if there is nothing on the stack, then just bind it to nothing. *)
	let onPress = try 
		Stack2.pop_nxt mousePressQ 
		with Stack2.Empty -> (fun _ -> ())
	in
	let onRelease= try 
		Stack2.pop_nxt mouseReleaseQ 
		with Stack2.Empty -> (fun _ -> ())
	in
	
	bind ~events:[`ButtonPressDetail(1)] ~fields:[`MouseX; `MouseY] ~action:onPress top ; 
	bind ~events:[`ButtonReleaseDetail(1)] ~fields:[`MouseX; `MouseY] ~action:onRelease top ; 
	;;
