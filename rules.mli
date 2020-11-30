type direction = Up | Right | Down | Left


type ball

type move

type game

(** [make_ball ~bid:i pos] returns a new ball at position [pos] with the ball id [bid] (optional) *)
val make_ball : ?bid:int -> Position.t -> ball

(** [new_game ball_list] returns a new game from a list of balls [ball_list] *)
val new_game : ball list -> game

(** [eq_ball ball ball'] returns true if and only if ball and ball' are equals
    indenpendetly from their position since balls can move *)
val eq_ball : ball -> ball -> bool

(** [make_move b d] returns a new move from a ball [b] and a direction [d] *)
val make_move : ball -> direction -> move

(** [apply_move game move] returns a new game where [move] has been applied to [game] *)
val apply_move : game -> move -> game

(** [moves game] returns all the valid moves possible for [game] *)
val moves : game -> move list

(** [get_balls game] returns the current list of ball on the [game] *)
val get_balls : game -> ball list

(** [is_ball pos] returns true if and only if their is a ball on the position [pos] *)
val is_ball : game -> Position.t -> bool

(** [ball_of_position game pos] returns the ball that is on the position [pos]. Fail if their is none *)
val ball_of_position : game -> Position.t -> ball

(** [position_of_ball ball] returns the position of the ball [ball] *)
val position_of_ball : ball -> Position.t

(** [add_ball game ball on_x on_y] link the ball to the game *)
val add_ball : game -> ball -> unit

(** [add_ball game ball on_x on_y] unlink the ball to the game *)
val rm_ball : game -> ball -> unit

(** [hashtbl key] return the binding of key in hashtbl without raising Missing_value and by remplacing missing values by [] *)
val hash_find : ('a, 'b list) Hashtbl.t -> 'a -> 'b list
