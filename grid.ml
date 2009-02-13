open Comm
open Tk

let draw (xl,yl,xh,yh) gridsize alphain=
	(* draw a grid on the screen*)
	let gridrange lo hi =
		let sta = iof (lo /. gridsize) in
		let fin = iof (hi /. gridsize) +1 in
		(* don't draw too many lines on the screen *)
		if fin - sta < 400 then (
			range_float ((foi sta) *. gridsize) gridsize ((foi fin) *. gridsize)
		) else []
	in
	let vgrid =  gridrange xl xh in
	let hgrid =  gridrange yl yh in
	if List.length hgrid > 0 && List.length vgrid > 0 then (
		let numverts = ((List.length vgrid) + (List.length hgrid)) * 2 in
		let raw = Raw.create `float (numverts * 2) in
		let cnt = ref 0 in
		List.iter (fun x -> 
			Raw.set_float raw ~pos:(!cnt*4+0) x ;  
			Raw.set_float raw ~pos:(!cnt*4+1) yl ;  
			Raw.set_float raw ~pos:(!cnt*4+2) x ;  
			Raw.set_float raw ~pos:(!cnt*4+3) yh ;  
			incr cnt ; 
		) vgrid; 
		List.iter (fun y -> 
			Raw.set_float raw ~pos:(!cnt*4+0) xl ;  
			Raw.set_float raw ~pos:(!cnt*4+1) y ;  
			Raw.set_float raw ~pos:(!cnt*4+2) xh ;  
			Raw.set_float raw ~pos:(!cnt*4+3) y ;  
			incr cnt ; 
		) hgrid; 
		let alpha = alphain -. 0.2 *. ((foi (List.length hgrid)) /. 330.0) in
		let a2 = if alpha < 0.0 then 0.0 else alpha in
		(* Printf.printf "alpha: %f\n%!" a2 ; *)
		GlDraw.color ~alpha:a2 (0.5 , 0.5 , 1.); 
		GlArray.vertex `two raw ; 
		GlArray.draw_arrays `lines 0 numverts ; 
	); 
	;;
	
let dialog top rendercb = 
	let dlog = Toplevel.create top in
	Wm.title_set dlog "Grid sizes" ; 
	let makeFrames pack unpack label = 
		let cnt = ref 1 in
		let frame3 = Frame.create dlog in
		let frames = List.map (fun g -> 
			let frame = Frame.create frame3 in
			let msg = Message.create ~width:40 ~text:("grid " ^ (soi !cnt) ^":") frame in
			let entry = Entry.create ~width:10 frame in
			Entry.insert ~index:(`Num 0) ~text:(pack g) entry; 
			Tk.pack ~fill:`Y ~expand:true ~side:`Left [Tk.coe msg; Tk.coe entry]; 
			incr cnt; 
			(frame, (fun () -> unpack (Entry.get entry)))
		) (Array.to_list ggrid) in
		let frame2 = Frame.create frame3 in
		let button = Button.create ~text:("set " ^ label ^ "!")
		~command:( fun () -> 
				let c = ref 0 in
				List.iter (fun (_,cb) -> 
					ggrid.(!c) <- cb () ; 
					incr c; 
				) frames ; 
				rendercb(); 
			) frame2 
		in
		(* button is in frame2 *)
		Tk.pack ~side:`Left ~fill:`Both ~expand:true [button] ;
		let all = frame2 :: (List.map (fun (f,_) -> f) frames) in
		Tk.pack ~side:`Top ~fill:`Both ~expand:true all ;
		frame3
	in
	let inchframe = makeFrames sof fos "inches"  in
	let mmframe = makeFrames 
		(fun x -> sof (x *. 25.4)) 
		(fun s -> (fos s) /. 25.4) "mm" in
	Tk.pack ~side:`Left ~fill:`Both ~expand:true [inchframe; mmframe] ; 
	;;
	