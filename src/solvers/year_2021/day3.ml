open Solvers.Signature
open Utils.List_utils
open Utils.String_utils

module Solver : Solver = struct
  let bit_value = function '1' -> 1 | '0' -> -1 | _ -> failwith "Invalid bit."

  let gamma_epsilon counter =
    let rec aux i gamma epsilon = function
      | [] -> (gamma, epsilon)
      | x :: xs ->
          if x > 0 then aux (i * 2) (gamma + i) epsilon xs
          else aux (i * 2) gamma (epsilon + i) xs
    in
    aux 1 0 0 (List.rev counter)

  let rec add_lists s1 s2 =
    match (s1, s2) with
    | [], [] -> []
    | [], y :: ys -> y :: add_lists [] ys
    | x :: xs, y :: ys -> (x + y) :: add_lists xs ys
    | _ -> failwith "List length error."

  let update_counter ones line =
    line |> String.to_seq |> List.of_seq |> List.map bit_value |> add_lists ones

  let naloga1 data =
    let gamma, epsilon =
      data |> List.lines |> List.fold_left update_counter [] |> gamma_epsilon
    in
    string_of_int (gamma * epsilon)

  let rec match_counter oxy_best co2_best line counter =
    match (line, counter) with
    | '1' :: xs, c :: cs when c >= 0 ->
        match_counter (oxy_best + 1) co2_best xs cs
    | '0' :: xs, c :: cs when c < 0 ->
        match_counter (oxy_best + 1) co2_best xs cs
    | '1' :: xs, c :: cs when c < 0 ->
        match_counter oxy_best (co2_best + 1) xs cs
    | '0' :: xs, c :: cs when c >= 0 ->
        match_counter oxy_best (co2_best + 1) xs cs
    | [], [] -> (oxy_best, co2_best)
    | _ -> failwith "Invalid input length."

  let updated_best ((oxy, best_o), (co2, best_c)) line counter =
    let o, c = match_counter 0 0 line counter in
    let new_oxy = if o > best_o then (line, o) else (oxy, best_o) in
    let new_co2 = if c > best_c then (line, c) else (co2, best_c) in
    (new_oxy, new_co2)

  let oxygen_co2 counter data =
    let rec aux (oxy, co2) = function
      | line :: data -> aux (updated_best (oxy, co2) line counter) data
      | [] -> (oxy, co2)
    in
    aux (([], 0), ([], 0)) data

  (*
  let to_decimal chlst =
    let rec aux i d = function
      | '1' :: cs -> aux (i * 2) (d + i) cs
      | '0' :: cs -> aux (i * 2) d cs
      | [] -> d
      | _ -> failwith "Not a bit character."
    in
    aux 1 0 (List.rev chlst)
*)

  let filter_by_first most_common lines 

  let naloga2 data _ =
    let c = data |> List.lines in
    ""
end
