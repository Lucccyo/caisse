type t = cell Grid.t
and cell = Wall | NotWall

val pp_maze: t -> unit

val v: width:int -> height:int -> t
