module G = Graphics
module D = Draw
open Printf

(* max width of the grid printed *)
let max_x = 15

(* max height of the grid printed *)
let max_y = 15

(* game is a reference to the initial game. *)
let game = ref (Rules.new_game [])

(* removes empty strings from a string list *)
let rec clean strlist=
  match strlist with
  |[] -> []
  |h::t -> if h <> "" then h::(clean t)
           else clean t

(* save a game into a file named [filename] (See README for the format)*)
let save filename game = 
  let bl = Rules.get_balls game in
  let oc = open_out filename in
  try
    Printf.fprintf oc "%d\n" (List.length bl);
    let rec aux l =
      match l with
      |[] -> ()
      |h::t ->  let p = Rules.position_of_ball h in
                let (x,y) = Position.proj_x p, Position.proj_y p in
                Printf.fprintf oc "%d %d\n" x y;
                aux t
    in aux bl;
    close_out oc
  with _ -> (close_out_noerr oc;            (* if there is an exception we close the channel but its content is not flushed *)
            print_string "An error occured while saving"; flush stdout)

(* load a game from a config file. Returns None if the file is invalid else it returns Some game *)
let load filename =
  D.ready false;
  let g = Rules.new_game [] in
  D.draw_game max_x max_y g;
  let ic = open_in filename in
  try begin
    let n = int_of_string (List.hd (clean (String.split_on_char ' ' (input_line ic)))) in
    let rec aux k =
      if k = 0 then []
      else begin
        (* parse a line from the file into x and y coordinates *)
        let splited_line = clean (String.split_on_char ' ' (input_line ic)) in
        let x = int_of_string (List.hd splited_line) and y = int_of_string (List.nth splited_line 1) in
        let p = Position.from_int x y in 
        if not( Rules.is_ball g p) then begin  (* add a ball to the game *)
          let b = Rules.make_ball p in
          Rules.add_ball g b;
          D.draw_ball b;
          b::(aux (k-1)) end
        else
          aux (k-1) 
      end
    in let g = Rules.new_game (aux n) in
    close_in ic; D.ready true;
    Some g
  end
  with e -> (close_in_noerr ic;
            print_string "An error occured while loading (maybe the config file is invalid)"; 
            flush stdout;D.ready true; None)

(* return the ball that the player wants to move *)
let rec get_ball game =
  let status = G.wait_next_event [G.Button_down] in
  let (x,y) = (status.G.mouse_x,status.G.mouse_y) in
  let p = D.position_of_coord x y in
  if Rules.is_ball game p then
    begin
      let ball = Rules.ball_of_position game p in
      D.draw_ball ~select:true ball; (* to show which ball has been selected *)
      ball
    end
  else
    get_ball game (* the player has selected an empty cell *)

(* convert the key pressed into a char and call the continuation k on it *)
let get_key_pressed k =
  let status = G.wait_next_event [G.Key_pressed] in
  let key = Char.code status.G.key in
  k (Char.chr key)

(* return the direction choosen by the player *)
let rec get_ball_direction () =
  let dir_of_char c =
    Rules.(
      match c with
      | 'z' -> Some Up
      | 's' -> Some Down
      | 'd' -> Some Right
      | 'q' -> Some Left
      | _ -> None
    )
  in
  get_key_pressed (fun c -> match dir_of_char c with
      | Some (x) -> x

      | None -> get_ball_direction () (* wrong key pressed by the player *)
    )

(* get the next move of the player *)
let get_next_move game =
  let p = get_ball game in
  let d = get_ball_direction () in
  Rules.make_move p d

let rec generate_game n =
  D.ready false;
  let g = Rules.new_game [] in
  D.draw_game max_x max_y g;
  D.draw_string "Generating ...";
  let rec aux k =
    if k<n then
      let p = Position.from_int (Random.int 15) (Random.int 15) in
      if not( Rules.is_ball g p) then begin
        let b = Rules.make_ball p in
        Rules.add_ball g b;
        D.draw_ball b;
        aux (k+1) end
      else
        aux k
    else ()
  in aux 0;
  D.ready true;
  match Solver.solve g with
  |None -> generate_game n
  |Some x -> g

(* create_game allows the player to create its own game by putting balls over the grid *)
let create_game () =
  D.ready false;
  D.draw_game max_x max_y (Rules.new_game []);
  let rec add_balls l =
    let status = G.wait_next_event [G.Button_down;G.Key_pressed] in
    if status.G.keypressed = true &&  Char.chr (Char.code status.G.key) = 'e' then
      begin Draw.ready true; l end
    else
      let (x,y) = (status.G.mouse_x, status.G.mouse_y) in
      let p = D.position_of_coord x y in
      let (x',y') = Position.proj_x p, Position.proj_y p in
      (* balls can not be outside the grid *)
      if 0 <= x' && x' < max_x && 0 <= y' && y' < max_y then
        let ball = Rules.make_ball p in
        D.draw_ball ball;
        add_balls (ball::l)
      else
        add_balls l
  in
  let balls = add_balls [] in
  Rules.new_game balls

(* A menu is a pair of string * f where f is a function of type unit -> unit.
   If the player choose on the menu which function should be called *)
let rec menu = [("solve", solve);("play", play);("generate and play", generate_and_play);("load config file", load_config);("exit", leave)]

(* play allows the player to create a new game, and then try to solve it *)
and play () =
  game := create_game ();
  loop !game

(* solve allows the player to create a new game and then see if the game can be solved *)
and solve () =
  game := create_game ();
  solver !game

(* loop game loops on the game while their is still moves possible for the player *)
and loop game =
  D.draw_game max_x max_y game;
  let valid_moves = Rules.moves game in
  if List.length valid_moves == 0 then begin
    if List.length (Rules.get_balls game) == 1 then 
       D.draw_string "Good job!"
    else  
      D.draw_string "There is no possible action";
    get_key_pressed (fun c -> main (("replay", replay)::("get solution (last game)", resolve)::("save previous game", save_config)::menu))
  end
  else begin
    let wanted_move = get_next_move game in
    let g1 = if List.mem wanted_move valid_moves then Rules.apply_move game wanted_move
              else game
    in loop g1
  end

(* solver game solve the game if it is possible *)
and solver game  =
  D.draw_game max_x max_y game;
  D.draw_string "Solving ...";
  let moves = Solver.solve game in
  D.draw_game max_x max_y game;
  match moves with
  | None ->   D.draw_string "No solution!"; get_key_pressed (fun c -> main (("resolve", resolve)::("save previous game", save_config)::menu))
  | Some moves -> D.draw_string "Solved!";
    let g = List.fold_left (fun g m -> D.draw_game max_x max_y g ;
                             D.draw_string "Solved!";
                             get_key_pressed (fun c -> ());
                             Rules.apply_move g m) game moves
    in
    D.draw_game max_x max_y g;
    get_key_pressed (fun c -> main (("resolve", resolve)::("save previous game", save_config)::menu))

(* replay the previous game *)
and replay () =
  loop !game
(* resolve the previous game *)
and resolve () =
  solver !game
(* generate a random game and play it *)
and generate_and_play () =
  let f n = fun () -> game := generate_game n in
  main [("5 balls", f 5);("7 balls", f 7);("10 balls", f 10);
        ("15 balls", f 15);("back", fun x -> main menu)];
  loop !game
(* load a game from a config file *)
and load_config () =
  let g filename= fun () -> match load filename with
                            |None -> main menu
                            |Some g -> (game:= g;
                                        main [("play loaded game", replay);("solve loaded game", resolve);("back", fun x -> main menu)])
  in main [("load config 1", g "config1.flg");("load config 2", g "config2.flg");
            ("load config 3", g "config3.flg");("back", fun x -> main menu)]
(* save previous game in a file *)
and save_config () =
  let h filename= fun () -> save filename !game
  in main [("save as config 1", h "config1.flg");("save as config 2", h "config2.flg");
            ("save as config 3", h "config3.flg");("back", fun x -> main menu)];
  main (("replay", replay)::("resolve", resolve)::menu)
(* leave the application *)
and leave () =
  D.close_window()

(* get the choice of the player *)
and main l =
  let choice c =
    let i = (int_of_char c) - (int_of_char '0') in
    if 0 <= i && i < List.length l then
      snd (List.nth l i) ()
    else
      main l
  in
  Random.self_init();
  D.init_window();
  D.draw_menu l;
  get_key_pressed choice

let _ = main menu
