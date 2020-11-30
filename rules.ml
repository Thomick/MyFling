type direction = Up | Right | Down | Left

type ball = {pos : Position.t; id : int}

type move = { b: ball ; dir: direction}

type game = {x_axis : (int, ball list) Hashtbl.t; y_axis : (int, ball list) Hashtbl.t}

(* Counter incremented each time a new ball is created. Allows to give unique id for each ball *)
let counter = ref 0

let make_ball ?bid:(bid=0) p = 
    if bid <> 0 then 
        {pos = p; id = bid}
    else
        (incr counter;
        {pos = p; id = !counter})

let position_of_ball b = b.pos

let eq_ball b b' = b.id==b'.id

let hash_find hashtbl key = 
    match (Hashtbl.find_opt hashtbl key) with
        |None -> []
        |Some (x) -> x

let is_ball g p =
    let rec aux l = 
        match l with
        |[] -> false
        |h::t -> Position.eq (position_of_ball h) p || aux t
    in aux (hash_find g.x_axis (Position.proj_x p))

let add_ball game ball=
    let (x,y) = Position.proj_x ball.pos, Position.proj_y ball.pos in
    let l = hash_find game.x_axis x in
    Hashtbl.replace game.x_axis x (List.sort (fun b1 b2 -> compare (Position.proj_y b1.pos) (Position.proj_y b2.pos)) (ball::l));
    let l = hash_find game.y_axis y in
    Hashtbl.replace game.y_axis y (List.sort (fun b1 b2 -> compare (Position.proj_x b1.pos) (Position.proj_x b2.pos)) (ball::l))

let rm_ball game ball=
    let (x,y) = Position.proj_x ball.pos, Position.proj_y ball.pos in
    let l = hash_find game.x_axis x in
    Hashtbl.replace game.x_axis x (List.filter (fun b -> not(eq_ball ball b)) l);
    let l = hash_find game.y_axis y in
    Hashtbl.replace game.y_axis y (List.filter (fun b -> not(eq_ball ball b)) l)

let new_game ps = 
    let game = {x_axis = Hashtbl.create (List.length ps); y_axis= Hashtbl.create (List.length ps)} in
    let rec aux l = 
        match l with
        |[] -> ()
        |b::t -> if not(is_ball game (position_of_ball b)) then (add_ball game b; aux t)
                else aux t
    in aux ps; game

let make_move p d = {b=p; dir = d}

let apply_move game move = 
    let g = {x_axis = Hashtbl.copy game.x_axis; y_axis = Hashtbl.copy game.y_axis} in
    let (x,y) = Position.proj_x move.b.pos, Position.proj_y move.b.pos in
    (* Find the relevant list for the move in one of the hashtables *)
    let l=match move.dir with
        | Up -> hash_find g.x_axis x
        | Right -> hash_find g.y_axis y
        | Down -> List.rev (hash_find g.x_axis x)
        | Left -> List.rev (hash_find g.y_axis y)
    in 
    let rec do_move bl =  
        (* the balls that will collide are consecutive in the ball list bl. 
        The move is relayed from ball to ball until the last one which is expulsed from the board *)
        match bl with
        |b1::b2::t ->begin 
                    match move.dir with
                    | Up -> (rm_ball g b1; 
                            let b1' = make_ball ~bid:b1.id (Position.move b2.pos (Position.from_int 0 (-1))) in
                            add_ball g b1')
                    | Right -> (rm_ball g b1; 
                            let b1' = make_ball ~bid:b1.id (Position.move b2.pos (Position.from_int (-1) 0)) in
                            add_ball g b1')
                    | Down -> (rm_ball g b1; 
                            let b1' = make_ball ~bid:b1.id (Position.move b2.pos (Position.from_int 0 1)) in
                            add_ball g b1')
                    | Left -> (rm_ball g b1; 
                            let b1' = make_ball ~bid:b1.id (Position.move b2.pos (Position.from_int 1 0)) in
                            add_ball g b1')
                    end;do_move (b2::t)
        |b::[] -> rm_ball g b
        |[] -> ()
    in let rec from_ball bl =   (* returns the sublist of the balls which will really move (from the first ball which is moved onward) *)
        match bl with
        |[] -> []
        |b::t when eq_ball b move.b -> b::t
        |b::t -> from_ball t
    in do_move (from_ball l);
    g


let moves g = 
    (* each list represent the balls aligned along one direction.
    We consider each pair of consecutive balls in the list, if there is an empty space between them the move is valid, else we continue *)
    let rec search_moves d1 d2 k bl ml =    (* d1 is the direction of a ball going to the next in the list. d2 is the opposite direction
                                            k is an unused but obligatory argument so search_move can be used with Hashtable.fold (it's the key associated with the ball list)
                                            bl is a list of balls sharing a coordinate and ordered by the other one.
                                            ml is a list of moves that accumulate the moves already found*)
        match bl with
        |b1::b2::t -> if abs ((Position.proj_x b1.pos) - (Position.proj_x b2.pos)) > 1
                        || abs ((Position.proj_y b1.pos) - (Position.proj_y b2.pos)) > 1 then
                        search_moves d1 d2 k (b2::t) ({b=b1;dir=d1}::{b=b2;dir=d2}::ml)
                    else search_moves d1 d2 k (b2::t) ml
        |_ -> ml
    in let rlm = Hashtbl.fold (search_moves Right Left) g.y_axis []     (* apply the fonction search_moves to each line to detect valid Right Left moves *)
    in Hashtbl.fold (search_moves Up Down) g.x_axis rlm                 (* apply the fonction search_moves to each column to detect valid Up Down moves and add them to the Right Left moves*)

let get_balls g = Hashtbl.fold (fun k v c -> v@c) g.x_axis []

let ball_of_position game p = 
    let rec find bl =
        match bl with
        |[] -> failwith "No ball here"
        |h::t -> if Position.eq (position_of_ball h) p then h
                else find t
    in find (hash_find game.x_axis (Position.proj_x p))

