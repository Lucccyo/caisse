open Gamelle

type elem = Empty | Wall | Caisse | End

type state = { t: int; grid: elem Grid.t; caisse: Grid.position }

let s = 40.

let pp_grid grid =
  Grid.iter (fun (x, y) e ->
    Format.printf "(%d;%d) -> %s@." x y
      (match e with Empty -> "empty" | Wall -> "wall" | Caisse -> "caisse" | End -> "end")
  ) grid

let rec choose_different width height randx randy =
  let x = Random.int height in
  let y = Random.int width in
  if x = randx && y = randy then choose_different width height randx randy else x, y

let init_grid width height randx randy =
  let grid = Grid.init width height (fun _ -> Empty) in
  Format.printf "caisse pos : %d;%d@." randx randy;
  Grid.set grid (randx, randy) Caisse;
  let endx, endy = choose_different (Grid.width grid) (Grid.height grid) randx randy in
  Format.printf "vic pos : %d;%d@." endx endy;
  Grid.set grid (endx, endy) End;
  Format.printf "grid size : h: %d; w: %d@.@." (Grid.height grid) (Grid.width grid);
  grid

let initial_state () =
  let width = 10 in
  let height = 8 in
  let randx =  Random.int width in
  let randy =  Random.int height in
  { t = 0; grid = init_grid width height randx randy; caisse= (randx, randy) }

let draw_grid grid io =
  Grid.iter (fun (x, y) e ->
    let x = float_of_int x in
    let y = float_of_int y in
    let b = Box.v (Point.v (x *. s) (y *. s)) (Size.v s s) in
    let color = match e with
      | Empty  -> Color.white
      | Wall   -> Color.gray
      | Caisse -> Color.blue
      | End -> Color.red
    in
    Box.fill ~io ~color b;
  ) grid

let check_border target grid =
  Pair.fst target < Grid.height grid  &&
  Pair.fst target >= 0 &&
  Pair.snd target < Grid.width grid &&
  Pair.snd target >= 0

let move state dir =
  let target = Grid.move dir state.caisse in
  if check_border target state.grid then (
    match Grid.get state.grid target with
    | Empty ->
      Grid.set state.grid state.caisse Empty;
      Grid.set state.grid target Caisse;
      { t = state.t + 1; grid = state.grid; caisse = target }
    | Wall -> {state with t = state.t + 1}
    | End  -> Format.printf "Victory@."; initial_state ()
    | Caisse -> assert false
  ) else state

let () =
  Random.init 42;
  Gamelle.run (initial_state ()) @@ fun ~io state ->
    draw_grid state.grid io;
    if state.t mod 7 <> 0 then { state with t = state.t + 1} else (
      if      Input.is_pressed ~io `arrow_right then move state Grid.S
      else if Input.is_pressed ~io `arrow_left  then move state Grid.N
      else if Input.is_pressed ~io `arrow_up    then move state Grid.W
      else if Input.is_pressed ~io `arrow_down  then move state Grid.E
      else { state with t = state.t + 1}
    )