open Component_defs
open System_defs

let make x y width height c mass =
  let b =
    object
      inherit position
      inherit box
      inherit color
 

      inherit mass
      inherit velocity
      inherit sum_forces
    end
  in
  b#position#set Vector.{ x; y };
  b#box#set Rect.{ width; height; x;y };
  b#color#set c;
  Draw_system.register (b :> Draw_system.t);

 
  b#mass#set mass;

  Move_system.register (b :> Move_system.t);
  
  Force_system.register (b :> Force_system.t);
  
  Collision_system.register (b :> Collision_system.t);
  b