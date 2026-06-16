open Gamelle

type state = {
  maze: Maze.cell Grid.t;
  caisse_visual: float * float;
  caisse_pos: Grid.position;
  victory_pos: Grid.position;
  last_frame: float;
}

let framespeed = 0.09 (* s *)

let step = 0.2

let csize = 40.

let rec get_not_wall_position maze =
  let x =  Random.int (Grid.width maze) in
  let y =  Random.int (Grid.height maze) in
  match Grid.get maze (x, y) with
  | Maze.NotWall -> (x, y)
  | Wall -> get_not_wall_position maze

let next_grid () =
  let maze = Maze.v ~width:7 ~height:7 in
  let caisse_pos = get_not_wall_position maze in
  let rec victory () =
    let pos = get_not_wall_position maze in
    if pos <> caisse_pos then pos
    else victory ()
  in
  let caisse_visual =
    (float_of_int (Pair.fst caisse_pos),
     float_of_int (Pair.snd caisse_pos)) in
  let victory_pos = victory () in
  {maze; caisse_pos; caisse_visual; victory_pos; last_frame = 0.}

let draw_maze maze ~io =
  Grid.iter (fun (x, y) cell ->
    let x = float_of_int x in
    let y = float_of_int y in
    match cell with
    | Maze.NotWall ->
      let box = Box.v (Point.v (x *. csize) (y *. csize)) (Size.v csize csize) in
      Box.fill ~io ~color:Color.white box
    | Wall ->
      let box = Box.v (Point.v (x *. csize) (y *. csize)) (Size.v csize csize) in
      Box.fill ~io ~color:Color.gray box
  ) maze

let draw_caisse caisse_visual ~io =
  let x, y = (Pair.fst caisse_visual, Pair.snd caisse_visual) in
  let box = Box.v (Point.v (x *. csize +. 10.) (y *. csize +. 10.)) (Size.v 20. 20.) in
  Box.fill ~io ~color:Color.red box

let draw_victory victory_pos ~io =
  let x = float_of_int (Pair.fst victory_pos) in
  let y = float_of_int (Pair.snd victory_pos) in
  let box = Box.v (Point.v (x *. csize +. 10.) (y *. csize +. 10.)) (Size.v 20. 20.) in
  Box.fill ~io ~color:Color.blue box

let draw_state state io =
  draw_maze state.maze ~io;
  draw_victory state.victory_pos ~io;
  draw_caisse state.caisse_visual ~io;
  ()

let check_border target maze =
  Pair.fst target < Grid.height maze  &&
  Pair.fst target >= 0 &&
  Pair.snd target < Grid.width maze &&
  Pair.snd target >= 0

let move state dir =
  let target = Grid.move dir state.caisse_pos in
  if check_border target state.maze then (
    match Grid.get state.maze target with
    | NotWall ->
      if target = state.victory_pos then next_grid ()
      else {state with caisse_pos = target}
    | Wall -> state
  ) else
  state

let calc_next_visual_pos state =
  let caisse_pos =
    (float_of_int (Pair.fst state.caisse_pos),
     float_of_int (Pair.snd state.caisse_pos)) in
  let caisse_visual = state.caisse_visual in
  let x, y =
    if caisse_pos = caisse_visual then (Pair.fst caisse_pos, Pair.snd caisse_pos)
    else (
      let visu_x, visu_y = (Pair.fst caisse_visual, Pair.snd caisse_visual) in
      let to_x, to_y = (Pair.fst caisse_pos, Pair.snd caisse_pos) in
      assert(visu_x != to_x || visu_y != to_y);
      assert( (visu_x >= to_x -. 1. && visu_y = to_y) (* -> *)
           || (visu_x <= to_x +. 1. && visu_y = to_y) (* <- *)
           || (visu_y >= to_y -. 1. && visu_x = to_x) (* V  *)
           || (visu_y <= to_y +. 1. && visu_x = to_x) (* A  *) );
      if      visu_x < to_x then (min (visu_x +. step) to_x, visu_y) (*->*)
      else if visu_x > to_x then (max (visu_x -. step) to_x, visu_y) (* <-*)
      else if visu_y < to_y then (visu_x, min (visu_y +. step) to_y) (* V *)
      else if visu_y > to_y then (visu_x, max (visu_y -. step) to_y) (* A *)
      else caisse_visual )
    in
  (x, y)

let () =
  Random.init 18;
  Gamelle.run (next_grid ()) @@ fun ~io state ->
    draw_state state io;
    let state = {state with caisse_visual = calc_next_visual_pos state} in
    let dir =
      if      Input.is_pressed ~io `arrow_right then Some(Grid.S)
      else if Input.is_pressed ~io `arrow_left  then Some(Grid.N)
      else if Input.is_pressed ~io `arrow_up    then Some(Grid.W)
      else if Input.is_pressed ~io `arrow_down  then Some(Grid.E)
      else None in
    if state.last_frame +.framespeed <= clock ~io then
      let state = match dir with
        | Some d -> move state d
        | None -> state in
      {state with last_frame = state.last_frame +. framespeed}
    else
      state
