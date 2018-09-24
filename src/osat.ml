type boolean_expression =
    Variable of string
  | LogicalValue of bool
  | And of boolean_expression * boolean_expression
  | Or of boolean_expression * boolean_expression
  | Not of boolean_expression
[@@deriving show]

let rec find_free_variable expr =
  let first e1 e2 = match (e1, e2) with
      Some e, _ -> Some e
    | _, Some e -> Some e
    | _ -> None
  in match expr with
    Variable s -> Some s
  | And (e1, e2) -> first (find_free_variable e1) (find_free_variable e2)
  | Or (e1, e2) -> first (find_free_variable e1) (find_free_variable e2)
  | Not (e) -> find_free_variable e
  | _ -> None 

let rec assign_variable name value = function
    Variable s when s = name -> LogicalValue value
  | And (e1, e2) -> And(assign_variable name value e1, assign_variable name value e2)
  | Or (e1, e2) -> Or(assign_variable name value e1, assign_variable name value e2)
  | Not (e1) -> Not(assign_variable name value e1)
  | x -> x
