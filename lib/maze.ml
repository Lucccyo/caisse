type cell = Wall | NotWall

type t = cell Grid.t

let pp_cell fmt _pos cell =
  Format.fprintf fmt "%s" (
    match cell with
    | NotWall -> "-"
    | Wall  -> "@"
  )

let pp_maze m =
  Grid.print pp_cell Format.std_formatter m

let is_NotWall x y = x mod 2 = 0 && y mod 2 = 0

let init_maze ~width ~height =
  let g = Grid.make (height * 2 - 1) (width * 2 - 1) Wall in
  for x = 0 to (Grid.height g - 1) do
    if x mod 2 = 0 then
    for y = 0 to (Grid.width g - 1) do
      if y mod 2 = 0 then (
        Grid.set g (x, y) NotWall
      )
      else ()
    done
    else ()
  done; g

let v ~width ~height =
  let maze = init_maze ~width ~height in
  Grid.iter (fun (x, y) _ ->
(* Chaque coordonnés pairs sont des trous *)
    if is_NotWall x y
    then (
      if (Random.int 2) mod 2 = 0
      then (* creuser le mur au nord *)
        if y > 0 then Grid.set maze (x, (y - 1)) NotWall else ()
      else (* creuser le mur de l'ouest *)
        if x > 0 then Grid.set maze ((x - 1), y) NotWall else ()
    )
    else ()
  ) maze;
  maze
