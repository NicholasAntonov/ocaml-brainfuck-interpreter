open Core_kernel.Std

let copy_with (l: 'a list) (index: int) (value: 'a) : 'a list =
  let rec loop (result: 'a list) (index: int) = function
    | head::tail -> if index = 0
      then loop (value::result) (pred index) tail
      else loop (head::result) (pred index) tail
    | [] -> List.rev result
  in
  loop [] index l

let safe_get (arr: 'a array) (index: int) : 'a option =
  try Some arr.(index)
  with | Invalid_argument _ -> None

let find_loop_end (start: int) (arr: char array) : int =
  let rec check_next arr index nesting =
    match nesting, safe_get arr index with
    | _, Some '[' -> check_next arr (succ index) (succ nesting)
    | 1, Some ']' -> index
    | n, Some ']' -> check_next arr (succ index) (pred nesting)
    | _, Some _ -> check_next arr (succ index) nesting
    | _, None -> failwith "mismatched ["
  in
  check_next arr start 0

let find_loop_start (start: int) (arr: char array) : int =
  let rec check_prev arr index nesting =
    match nesting, safe_get arr index with
    | _, Some ']' -> check_prev arr (pred index) (succ nesting)
    | 1, Some '[' -> index
    | n, Some '[' -> check_prev arr (pred index) (pred nesting)
    | _, Some _ -> check_prev arr (pred index) nesting
    | _, None -> failwith "mismatched ]"
  in
  check_prev arr start 0

let eval (program: char array) (ip: int) (dp: int) (input: int list) (data: int list) : unit =
  let rec interpret (ip: int) (dp: int) (input: int list) (data: int list) : unit =
      let loop = interpret (succ ip) in
      match safe_get program ip with
      | Some '>' -> let next = loop (succ dp) input in
        if (succ dp) >= List.length data
        then next (List.append data [0])
        else next data
      | Some '<' -> loop (pred dp) input data
      | Some '+' -> let next =
                      match List.nth data dp with
                      | Some 255 -> 0
                      | Some x -> succ x
                      | None -> failwith "invalid data pointer on increment"
        in loop dp input (copy_with data dp next)
      | Some '-' -> let next =
                      match List.nth data dp with
                      | Some 0 -> 255
                      | Some x -> pred x
                      | None -> failwith "invalid data pointer on decrement"
        in loop dp input (copy_with data dp next)
      | Some '.' -> let current_char = Option.bind (List.nth data dp) Char.of_int in
        let () = match current_char with
          | Some x -> print_char x
          | None -> failwith "Index out of bounds on print"
        in loop dp input data
      | Some ',' -> loop dp (List.tl_exn input) (copy_with data dp (List.hd_exn input))
      | Some '[' -> if phys_equal (List.nth_exn data dp) 0
        then interpret (find_loop_end ip program) dp input data
        else loop dp input data
      | Some ']' -> if phys_equal (List.nth_exn data dp) 0
        then loop dp input data
        else interpret (find_loop_start ip program) dp input data
      | Some _ -> loop dp input data
      | None -> ()
  in interpret ip dp input data

let rec read_lines () =
  try let line = read_line () in
    line :: read_lines()
  with
    End_of_file -> []

let string_to_list = Fn.compose List.rev String.to_list_rev

let () =
  let (raw_input, raw_program) = match read_lines () with
    | raw_input::raw_program -> (raw_input, raw_program)
    | _ -> failwith "Not enough input lines"
  in
  let input = raw_input
              |> string_to_list
              |> Caml.List.map Char.to_int
  in
  let program = raw_program
                |> Caml.List.map string_to_list
                |> Caml.List.concat
                |> Array.of_list
  in
  eval program 0 0 input [0];
  print_newline ()
