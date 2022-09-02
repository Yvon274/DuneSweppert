type t
(** The abstract type of values representing grid maps. *)

type state =
  | Flagged
  | Discovered of bool  (** The type of tile state *)

type tile =
  | Bomb of state
  | Numtile of state * int  (** The type of grid tiles *)

type config = {
  cols : int;
  rows : int;
  bombs : int;
}

(** The type of initial game configuration *)

val grid_gen : int -> int -> t
(** [grid_gen rows cols] creates a matrix filled with undiscovered
    Numtiles that contain value 0. *)

val place_bombs : config -> t -> t
(** [place_bombs fig array] randomly distrubutes bomb throughout the
    array. *)

val grid_length : t -> int
(** [grid_length t] is the length of the grid*)

val give_num : int -> int -> t -> config -> unit
(** [give_num x y array fig] updates the arrays NumTiles to the correct
    int value based off nerighboring bombs. *)

val generate_map : config -> t
(** [generate_map fig array] generates map using give_num. *)

(*val giveNum : int -> int -> tile array array -> t -> unit*)

val get_tile : t -> int -> int -> tile
(** [get_tile] returns the tile at the corresponding coordinates. **)

val set_tile : t -> int -> int -> tile -> unit
(** [set_tile] replaces the tile at the corresponding coordinate to the
    given tile. **)

val num_rows : t -> int
(** [set_tile] returns the number of rows in the given grid. **)

val num_cols : t -> int
(** [set_tile] returns the number of columns in the given grid. **)

val print_tile : tile -> int -> int -> config -> unit

val print_grid : config -> t -> unit

val num_lives : t -> config -> int
