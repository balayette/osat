open Osat

let () = 
  let expr = 
    And
      (
        And 
          (
            Not (LogicalValue false),
            Variable "a"
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
  match  df_satisfied expr with true -> Printf.printf "SAT\n" | _ -> Printf.printf "UNSAT\n"
