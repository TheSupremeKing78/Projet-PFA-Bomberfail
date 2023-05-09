open System_defs

class game_state =
  object
    val mutable occupied = Array.make_matrix 15 13 false
    method get_occupied = occupied
  end

let bombs = ref []
let time_elapsed = ref 0.0
let explosable = ref []
let portee = 2

let game = new game_state
let occupied = game#get_occupied 

let fill_borders win block_width mass =
  let width, height = Gfx.get_window_size win in
  let border_blocks = [
    Block.make 0. 0. block_width height (Gfx.color 64 64 64 255) mass;
    Block.make (float_of_int (width - block_width)) 0. block_width height (Gfx.color 64 64 64 255) mass;
    Block.make 0. 0. width block_width (Gfx.color 64 64 64 255) mass;
    Block.make 0. (float_of_int (height - block_width)) width block_width (Gfx.color 64 64 64 255) mass;
  ] in
  List.iter (fun block -> Draw_system.register (block :> Draw_system.t)) border_blocks 
  let a = Array.iteri (fun i _ -> game#get_occupied.(i).(0) <- true) (Array.make 15 false)  
  let b = Array.iteri (fun i _ -> game#get_occupied.(i).(12) <- true) (Array.make 15 false) 
  let c = Array.iteri (fun j _ -> if not game#get_occupied.(0).(j) then game#get_occupied.(0).(j) <- true) (Array.make 13 false)
  let d = Array.iteri (fun j _ -> if not game#get_occupied.(14).(j) then game#get_occupied.(14).(j) <- true) (Array.make 13 false) 

  
  


    let create_patterned_blocks win mass =
      let width, height = Gfx.get_window_size win in
      let num_blocks_x = 7 in
      let num_blocks_y = 6 in
      let block_size = 60 in
      let gap_size = 60 in
      let x_of_col col = width - (gap_size + col * (block_size + gap_size)) in
      let y_of_row row = height - (gap_size + row * (block_size + gap_size)) in
      let create_block x y size =
        let border_size = 60 in
        if x > border_size && y > border_size && (float_of_int x) +. (float_of_int size) < (float_of_int (width - border_size)) && (float_of_int y) +. (float_of_int size) < (float_of_int (height - border_size)) then
          let j = y_of_row (num_blocks_y - 1 - (y - border_size - gap_size) / (block_size + gap_size)) / (block_size + gap_size) in
          let i = x_of_col ((width - x - size - gap_size) / (block_size + gap_size)) / (block_size + gap_size) in
          game#get_occupied.(i).(j) <- true;
          Block.make (float_of_int x) (float_of_int y) size size (Gfx.color 160 160 160 255) mass 
        else
          Block.make 0. 0. 0 0 (Gfx.color 0 0 0 0) mass
      in
      let rec create_row row col acc =
        if col >= num_blocks_x then acc
        else
          let x = x_of_col col in
          let y = y_of_row row in
          let block = create_block x y block_size in
          create_row row (col+1) (block :: acc)
      in
      let rec create_all_rows row acc =
        if row >= num_blocks_y then acc
        else create_all_rows (row+1) (create_row row 0 acc)
      in
      let blocks = create_all_rows 0 [] in
      List.iter (fun block -> Draw_system.register (block :> Draw_system.t)) blocks
    

      let create_random_blocks win mass =
        let width, height = Gfx.get_window_size win in
        let num_blocks_x = 15 in
        let num_blocks_y = 13 in
        let block_size = 60 in
        let occupied = game#get_occupied in
        let rec create_block acc count =
          if count >= 30 then acc
          else
            let i = Random.int num_blocks_x in
            let j = Random.int num_blocks_y in
            if not occupied.(i).(j) then
              let x = (i * block_size) in
              let y = (j * block_size) in
              occupied.(i).(j) <- true;
              let block = Block.make (float_of_int x) (float_of_int y) block_size block_size (Gfx.color 165 42 42 255) mass in 
              block#expl :=true ;
              explosable := block :: !explosable;
              create_block (block::acc) (count+1)
            else
              create_block acc count
        in
        let blocks = create_block [] 0 in
        List.iter (fun block -> Draw_system.register (block :> Draw_system.t)) blocks


        let round_to_60 x =
          let y = x /. 60.0 in
          let rounded_y = float_of_int (int_of_float (y +. (if y < 0.0 then -.0.5 else 0.5))) in
          int_of_float (rounded_y *. 60.0)
      
  
      
  
    

let init () =
  let win = Gfx.create (Format.sprintf "game_canvas:%dx%d:" 900 780) in
  fill_borders win 60 infinity;
  create_patterned_blocks win infinity;
  create_random_blocks win infinity;
  Game_state.set_window win

let player =
   Joueur.make 60. 60. 60 60 (Gfx.color 255 0 0 255) 20. 
  let a = occupied.(1).(1) <- true

let ia =
  IA.make 780. 60. 60 60 (Gfx.color 255  255 0 255) 20.
  let a = occupied.(12).(1) <- true


  
  let update dt =
    (*player#velocity#set Vector.{x=0.25; y=0.25};*)
    player#sum_forces#set Vector.{x=0.00000001; y=0.00000001};
    ia#sum_forces#set Vector.{x=0.01; y=0.01};
    (*ia#velocity#set Vector.{x=0.01; y=0.01};*)
    (*ia#sum_forces#set Vector.{x=5.; y=5.};*)
    let () =
    match Gfx.poll_event () with
    | Gfx.NoEvent -> ()
    |Gfx.KeyDown " " ->
      let Vector.{ x; y } = player#position#get in

      let bombe = Bombe.make (float_of_int (round_to_60 x)) (float_of_int (round_to_60 y)) 50 50 (Gfx.color 0 0 0 255) infinity portee in
      bombs := bombe :: !bombs;
      player#position#set Vector.{ x; y }
    | Gfx.KeyDown s ->
        (*Gfx.debug "%s@\n%!" s;*)
        let Vector.{ x; y } = player#position#get in
        let x = if s = "ArrowLeft" then x -. 5.0 else x in
        let x = if s = "ArrowRight" then x +. 5.0 else x in
        let y = if s = "ArrowUp" then y -. 5.0 else y in
        let y = if s = "ArrowDown" then y +. 5.0 else y in
        player#position#set Vector.{ x; y }
    | Gfx.KeyUp _ -> ()
    in
    let Vector.{ x; y } = player#position#get in
    List.iter (fun bomb -> bomb#update x y 1.) !bombs;
    System_defs.Draw_system.update dt;
    Collision_system.update dt;
    Force_system.update dt;
    Move_system.update dt;
    Draw_system.update dt;
    true
  
  
  

let run () = init () ; Gfx.main_loop update
