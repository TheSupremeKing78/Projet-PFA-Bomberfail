open Ecs

module Draw_system = System.Make(Draw)
let () = System.register (module Draw_system)

module Collision_system = System.Make(Collisions)
let () = System.register (module Collision_system)



module Force_system = System.Make (Forces)
let () = System.register (module Force_system)


module Move_system = System.Make (Move)
let () = System.register (module Move_system)



