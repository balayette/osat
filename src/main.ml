open Osat

let () =
  let expr =
    And
      (
        And
          (
            Not (Variable "d"),
            (Variable "a")
          ),
        Or
          (
            LogicalValue false,
            And
            (
                Variable "c",
                Variable "a"
            )
          )
      )
  in
  expr |> pretty_print;
  optimize expr |> pretty_print;
  match  df_satisfied (optimize expr) with true -> Printf.printf "SAT\n" | _ -> Printf.printf "UNSAT\n"
