type t = 
    Variable of string
  | LogicalValue of bool
  | And of t * t
  | Or of t * t
  | Not of t
[@@deriving show]

val pretty_print : t -> unit

val df_satisfied : t -> bool

val optimize : t -> t
