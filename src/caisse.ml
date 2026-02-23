open Gamelle

type state = { t: int; maze: Maze.cell Grid.t; caisse: Grid.position; victory: Grid.position }

let csize = 40.

let rec get_not_wall_position maze =
  let x =  Random.int (Grid.width maze) in
  let y =  Random.int (Grid.height maze) in
  match Grid.get maze (x, y) with
  | Maze.NotWall -> (x, y)
  | Wall -> get_not_wall_position maze

let next_state () =
  let maze = Maze.v ~width:7 ~height:7 in
  let caisse = get_not_wall_position maze in
  let rec victory () =
    let pos = get_not_wall_position maze in
    if pos <> caisse then pos
    else victory ()
  in
  let victory = victory () in
  { t = 0; maze; caisse; victory }

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

let draw_caisse caisse ~io =
  let x = float_of_int (Pair.fst caisse) in
  let y = float_of_int (Pair.snd caisse) in
  let box = Box.v (Point.v (x *. csize +. 10.) (y *. csize +. 10.)) (Size.v 20. 20.) in
  Box.fill ~io ~color:Color.red box

let draw_victory victory ~io =
  let x = float_of_int (Pair.fst victory) in
  let y = float_of_int (Pair.snd victory) in
  let box = Box.v (Point.v (x *. csize +. 10.) (y *. csize +. 10.)) (Size.v 20. 20.) in
  Box.fill ~io ~color:Color.blue box

let draw_state state io =
  draw_maze state.maze ~io;
  draw_victory state.victory ~io;
  draw_caisse state.caisse ~io;
  ()

let check_border target maze =
  Pair.fst target < Grid.height maze  &&
  Pair.fst target >= 0 &&
  Pair.snd target < Grid.width maze &&
  Pair.snd target >= 0

let move state dir =
  let target = Grid.move dir state.caisse in
  if check_border target state.maze then (
    match Grid.get state.maze target with
    | NotWall ->
      if target = state.victory then next_state ()
      else { t = state.t + 1; maze = state.maze; caisse = target; victory = state.victory }
    | Wall -> {state with t = state.t + 1}
  ) else
  state

let () =
  Random.init 18;
  Gamelle.run (next_state ()) @@ fun ~io state ->
    draw_state state io;
    if state.t mod 7 <> 0 then { state with t = state.t + 1} else (
      if      Input.is_pressed ~io `arrow_right then move state Grid.S
      else if Input.is_pressed ~io `arrow_left  then move state Grid.N
      else if Input.is_pressed ~io `arrow_up    then move state Grid.W
      else if Input.is_pressed ~io `arrow_down  then move state Grid.E
      else if Input.is_pressed ~io `space  then next_state ()
      else { state with t = state.t + 1}
    )
