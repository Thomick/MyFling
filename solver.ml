(* Un critere pour qu'une solution soit possible. Raccourci la profondeur du parcours en détectant les boules isolées.
Prend en entrée les boules à surveiller [watchList] (sur lesquelles on vérifie le critère) et la liste de toutes les boules [ballList] (voir README)*)
let rec check watchList ballList = 
    match watchList with
    |[] -> false
    |b::t -> let p = Rules.position_of_ball b in
              let x,y = Position.proj_x p, Position.proj_y p in
              (* Are there balls up, down, left, right by comparaison to the ball b coordinates *)
              let u,d,l,r = not(List.exists (fun c -> Position.proj_y (Rules.position_of_ball c) >= y && not(Rules.eq_ball c b)) ballList),
                            not(List.exists (fun c -> Position.proj_y (Rules.position_of_ball c) <= y && not(Rules.eq_ball c b)) ballList),
                            not(List.exists (fun c -> Position.proj_x (Rules.position_of_ball c) <= x && not(Rules.eq_ball c b)) ballList),
                            not(List.exists (fun c -> Position.proj_x (Rules.position_of_ball c) >= x && not(Rules.eq_ball c b)) ballList)
              in (u&&l)||(u&&r)||(d&&l)||(d&&r) || check t ballList  (* Vérifie le critère "boule isolée" (cf README) *)

let solve game = 
  let rec next_move g ml=
    let balls = Rules.get_balls g in
    if List.length balls == 1 then Some []
    else if check balls balls then      (* S'il y a une boule isolées on arrête le parcours de cette branche *)
      None
    else begin
      match ml with
      |[] -> None
      |m::t -> let g' = Rules.apply_move g m in
        let res = next_move g' (Rules.moves g') in
        match res with
        |None -> next_move g t
        |Some res -> Some (m::res)
    end
  in next_move game (Rules.moves game)

