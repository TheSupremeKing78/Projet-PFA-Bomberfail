open Component_defs
open System_defs
open Block

class explosion =
  object (self)
    inherit position
    inherit box
    inherit color
    inherit mass
    inherit velocity
    inherit sum_forces

    val mutable time = 0.0
    

    method time = time
    (*method priority = priority
    method set_priority p = priority <- p*)

    method private remove_from_system =
      Draw_system.unregister (self :> Draw_system.t);
      Collision_system.unregister (self :> Collision_system.t)

    method private remove_explosion =
      self#remove_from_system;
      self#mass#set 0.0;

    method update dt =
      time <- time +. dt;
      if time >= 100.0 then self#remove_explosion
  end


class bombe =
  object (self)
    inherit position
    inherit box
    inherit color
    inherit mass
    inherit velocity
    inherit sum_forces

    

    

    val mutable time = 0.0
    val mutable explosionRange = 2

    (*method explosable (e : block) : block list =
      explosable := e :: !explosable;
      !explosable
    

      val mutable explosable : Component_defs.block list = []

      method set_explosable sable : unit =
        explosable <- sable*)
      


    val bombe_1 = ref false
    val  explosion = ref []
    method get_explosion = !explosion


    method bombe_1 = !bombe_1
    method time = time

    method explosionRange = explosionRange
    method set_explosionRange range =
      explosionRange <- range


    method set_bombe_1 b =
      if not b && !bombe_1 then self#remove_bombe;
      bombe_1 := b

    method private remove_from_system =
      Draw_system.unregister (self :> Draw_system.t);
      Collision_system.unregister (self :> Collision_system.t)

      method private remove_from_systemE a =
        Draw_system.unregister (a :> Draw_system.t);
        Collision_system.unregister (a :> Collision_system.t)

    method remove_bombe =
      self#remove_from_system;

    method private createExplosionObjects =
      let bomb_x = self#position#get.x in
      let bomb_y = self#position#get.y in
      let bomb_width = 50 in
      let bomb_height = 50 in
      let explosion_color = Gfx.color 255 0 0 255 in
      let coefficient = explosionRange in
    
      let createObject x y is_horizontal centre=
        let obj = new explosion in
        obj#mass#set infinity ;
        obj#position#set Vector.{ x; y };
        let width_coefficient = if is_horizontal then if centre then coefficient+1 else coefficient else 1 in
        let height_coefficient = if is_horizontal then 1 else coefficient in
        obj#box#set Rect.{ width = width_coefficient * bomb_width; height = height_coefficient * bomb_height; x; y };
        obj#color#set explosion_color;
        Draw_system.register (obj :> Draw_system.t);
        obj
      in
    
      let left_object = createObject (bomb_x -. 2. *. float_of_int bomb_width) bomb_y true true in
      let right_object = createObject (bomb_x +. 1. *. float_of_int bomb_width) bomb_y true false in
      let top_object = createObject bomb_x (bomb_y -. float_of_int coefficient *. float_of_int bomb_height) false false in
      let bottom_object = createObject bomb_x (bomb_y +. 1. *. float_of_int bomb_height) false false in
  
    
      [left_object; right_object; top_object; bottom_object]

    method update x y dt =
      time <- time +. dt;
      
      let Vector.{ x = bomb_x; y = bomb_y } = self#position#get in
      let Rect.{ width = bomb_width; height = bomb_height; x = bomb_box_x; y = bomb_box_y } =self#box#get in
      let bomb_box_left = bomb_box_x -. (float_of_int bomb_width /. 2.0) in
      let bomb_box_right = bomb_box_x +. (float_of_int bomb_width /. 2.0) in
      let bomb_box_top = bomb_box_y -. (float_of_int bomb_height /. 2.0) in
      let bomb_box_bottom = bomb_box_y +. (float_of_int bomb_height /. 2.0) in
      let is_colliding = x >= bomb_box_left && x <= bomb_box_right && y >= bomb_box_top && y <= bomb_box_bottom in
      if not is_colliding then
        Collision_system.register (self :> Collision_system.t);
      if time >= 600. then self#remove_bombe;
      List.iter (fun e -> e#update dt) !explosion;
      


      if explosionRange > 0 && time >= 600.0 then begin
        let explos = self#createExplosionObjects in 
        explosion := explos ;
        (*List.iter (fun obj -> Collision_system.register (obj :> Collision_system.t)) !explosion;*)
        explosionRange <- explosionRange - explosionRange ; 
      end
        
      
  end

let make x y width height c mass portee =
  let b = new bombe in
  b#position#set Vector.{ x; y };
  b#box#set Rect.{ width; height;x;y};
  b#color#set c;
  Draw_system.register (b :> Draw_system.t);
  b#mass#set mass;

  b#set_bombe_1 true;
  b#set_explosionRange portee; 
  b
