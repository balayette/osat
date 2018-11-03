open Sexplib.Std

type t =
  | Variable of string
  | LogicalValue of bool
  | And of t * t
  | Or of t * t
  | Not of t
[@@deriving show, sexp]

let pretty_print e =
  let rec print = function
    | And (e1, e2) ->
      print_string "(" ;
      print e1 ;
      print_string " ∧ " ;
      print e2 ;
      print_string ")"
    | Or (e1, e2) ->
      print_string "(" ;
      print e1 ;
      print_string " ∨ " ;
      print e2 ;
      print_string ")"
    | Variable s -> Printf.printf "'%s'" s
    | LogicalValue b ->
      Printf.printf "%s" (match b with true -> "true" | _ -> "false")
    | Not e ->
      print_string "¬" ; print_string "(" ; print e ; print_string ")"
  in
  print e ; print_endline ""

let rec find_free_variable expr =
  let first e1 e2 =
    match (e1, e2) with Some e, _ | _, Some e -> Some e | _ -> None
  in
  match expr with
  | Variable s -> Some s
  | And (e1, e2) -> first (find_free_variable e1) (find_free_variable e2)
  | Or (e1, e2) -> first (find_free_variable e1) (find_free_variable e2)
  | Not e -> find_free_variable e
  | _ -> None

let rec assign_variable name value e =
  let aux = assign_variable name value in
  match e with
  | Variable s when s = name -> LogicalValue value
  | And (e1, e2) -> And (aux e1, aux e2)
  | Or (e1, e2) -> Or (aux e1, aux e2)
  | Not e1 -> Not (aux e1)
  | x -> x

let rec optimize = function
  | And _ as a -> optimize_and a
  | Or _ as o -> optimize_or o
  | Not _ as n -> optimize_not n
  | x -> x

and optimize_and = function
  | And (LogicalValue true, e) | And (e, LogicalValue true) -> e
  | And (LogicalValue false, _) | And (_, LogicalValue false) ->
    LogicalValue false
  | And (e1, e2) -> (
      let oe1 = optimize e1 and oe2 = optimize e2 in
      match (oe1, oe2) with
      | LogicalValue _, _ | _, LogicalValue _ -> optimize_and (And (oe1, oe2))
      | _ -> And (oe1, oe2) )
  | _ -> failwith "Can't happen"

and optimize_or = function
  | Or (LogicalValue true, _) | Or (_, LogicalValue true) -> LogicalValue true
  | Or (LogicalValue false, e) | Or (e, LogicalValue false) -> optimize e
  | Or (e1, e2) -> (
      let oe1 = optimize e1 and oe2 = optimize e2 in
      match (oe1, oe2) with
      | LogicalValue _, _ | _, LogicalValue _ -> optimize_or (Or (oe1, oe2))
      | _ -> Or (oe1, oe2) )
  | _ -> failwith "Can't happen"

and optimize_not = function
  | Not (LogicalValue x) -> LogicalValue (not x)
  | Not e -> (
      let oe = optimize e in
      match oe with LogicalValue _ -> optimize_not (Not oe) | _ -> Not oe )
  | _ -> failwith "Can't happen"

let df_satisfied e =
  let rec aux e =
    match find_free_variable e with
    | None -> (
        match e with LogicalValue b -> b | _ -> failwith "Impossible" )
    | Some s -> (
        match assign_variable s true e |> optimize |> aux with
          true -> Printf.printf "%s = true\n" s; true
        | _ -> (
            match assign_variable s false e |> optimize |> aux with
            | true ->
              Printf.printf "%s = false\n" s ;
              true
            | _ -> false ))
  in
  aux (optimize e)
