open Vector

type t = { width : int; height : int; x:float; y:float }


let mdiff v1 b1 v2 b2 =
  (* We use the Minkowski difference of Box1 and Box2 *)
  let x = v1.x -. v2.x -. float b2.width in
  let y = v1.y -. v2.y -. float b2.height in
  let h = b1.height + b2.height in
  let w = b1.width + b2.width in
  let rect = { width = w; height = h ; x=v1.x;y=v1.y } in
  let vec = { x; y } in
  (vec, rect)


let has_origin v b =
  v.x <= 0.0
  && v.x +. float b.width >= 0.0
  && v.y <= 0.0
  && v.y +. float b.height >= 0.0

(*let make_from_block block =
    let position = block#position#get in
    let size = block#size#get in
    Rect.create (Vector.sub position (Vector.div size 2.0)) size*)
  