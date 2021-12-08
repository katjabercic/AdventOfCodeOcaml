open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let increment1 (counter, previous) next =
    let updated_counter = if next > previous then counter + 1 else counter in
    (updated_counter, next)

  let increment2 (counter, a, b, c) d =
    let updated_counter = if a < d then counter + 1 else counter in
    (updated_counter, b, c, d)

  let get_counter1 (counter, _) = counter

  let get_counter2 (counter, _, _, _) = counter

  let naloga1 data =
    let lines = List.int_list (List.lines data) in
    match lines with
    | [] -> failwith "The list is too short"
    | head :: tail ->
        tail
        |> List.fold_left increment1 (0, head)
        |> get_counter1 |> string_of_int

  let naloga2 data _part1 =
    let lines = List.int_list (List.lines data) in
    match lines with
    | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] -> failwith "The list is too short"
    | a :: b :: c :: (_ :: _ as tail) ->
        tail
        |> List.fold_left increment2 (0, a, b, c)
        |> get_counter2 |> string_of_int
end
