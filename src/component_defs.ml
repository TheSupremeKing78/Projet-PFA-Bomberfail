open Ecs


class position =
  object
    val position = Component.def Vector.zero
    method position = position
  end

class color =
  object
    val color = Component.def (Gfx.color 0 0 0 255)
    method color = color
  end

class box =
  object
    val box = Component.def Rect.{ width = 0; height = 0;x=0.0;y=0.0}
    method box = box
  end

class mass =
  object
    val mass = Component.def 0.0
    method mass = mass
  end

class velocity =
  object
    val velocity = Component.def Vector.zero
    method velocity = velocity
  end

class sum_forces =
  object
    val sum_forces = Component.def Vector.zero
    method sum_forces = sum_forces
  end

class bonus_1 = 
  object
    val bonus_1 = true
    method bonus_1 = bonus_1
  end 

class time = 
  object
    val time = 0.0
    method time = time 
  end 

class expl =
  object
    val expl = ref false
    method expl = expl
end







