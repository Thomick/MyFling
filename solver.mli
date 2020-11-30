(** [game] returns some list of move (solution of the game) if there is a solution else returns None *)
val solve : Rules.game -> Rules.move list option

(** [watchList ballList] check if there are balls in [watchList] that are isolated from the balls in [ballList]. Returns true if there is an isolated ball*)
val check : Rules.ball list -> Rules.ball list -> bool