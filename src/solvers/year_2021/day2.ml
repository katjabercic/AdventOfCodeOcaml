open Solvers.Signature
open Utils.List_utils
open Utils.String_utils

module Solver : Solver = struct
  let update_position_1 (horizontal, depth) line =
    match String.split_on_char ' ' line with
    | [ command; units_str ] -> (
        let units = int_of_string units_str in
        match command with
        | "forward" -> (horizontal + units, depth)
        | "down" -> (horizontal, depth + units)
        | "up" -> (horizontal, depth - units)
        | _ -> failwith "Invalid command")
    | _ -> failwith "Invalid input"

  let update_position_2 (horizontal, depth, aim) line =
    match String.split_on_char ' ' line with
    | [ command; units_str ] -> (
        let units = int_of_string units_str in
        match command with
        | "forward" -> (horizontal + units, depth + (aim * units), aim)
        | "down" -> (horizontal, depth, aim + units)
        | "up" -> (horizontal, depth, aim - units)
        | _ -> failwith "Invalid command")
    | _ -> failwith "Invalid input"

  let naloga1 data =
    let horizontal, depth =
      data |> List.lines |> List.fold_left update_position_1 (0, 0)
    in
    string_of_int (horizontal * depth)

  let naloga2 data _ =
    let horizontal, depth, _ =
      data |> List.lines |> List.fold_left update_position_2 (0, 0, 0)
    in
    string_of_int (horizontal * depth)
end
