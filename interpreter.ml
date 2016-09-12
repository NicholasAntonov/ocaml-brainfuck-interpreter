open Core_kernel.Std

let copy_with (l: 'a list) (index: int) (value: 'a) : 'a list =
  let rec loop (result: 'a list) (index: int) = function
    | head::tail -> if index = 0
      then loop (value::result) (index - 1) tail
      else loop (head::result) (index - 1) tail
    | [] -> List.rev result
  in
  loop [] index l

let safe_get (arr: 'a array) (index: int) : 'a option =
  try
    Some arr.(index)
  with
  | Invalid_argument _ -> None

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

let rec interpret (program: char array) (budget: int) (ip: int) (dp: int) (input: int list) (data: int list) : unit =
  if budget < 1 && not (Option.is_none (safe_get program ip))
  then Printf.printf "\nPROCESS TIME OUT. KILLED!!!"
  else
    let loop = interpret program (budget - 1) (ip + 1) in
    match safe_get program ip with
    | Some '>'   (* Increment data pointer so that it points to next location in memory. *)
      -> let next = loop (succ dp) input in
      if (succ dp) >= List.length data
      then next (List.append data [0])
      else next data

    | Some '<'   (* Decrement data pointer so that it points to previous locaion in memory. *)
      -> loop (pred dp) input data

    | Some '+'   (* Increment the byte pointed by data pointer by 1.
                    If it is already at its maximum value, 255, then new value will be 0. *)
      -> let current = List.nth data dp in
      let next =
        match current with
        | Some 255 -> 0
        | Some x -> x + 1
        | None -> failwith "invalid data pointer on increment"
      in
      loop dp input (copy_with data dp next)

    | Some '-'   (* Decrement the byte pointed by data pointer by 1.
                    If it is at its minimum value, 0, then new value will be 255. *)
      -> let current = List.nth data dp in
      let next =
        match current with
        | Some 0 -> 255
        | Some x -> x - 1
        | None -> failwith "invalid data pointer on decrement"
      in
      loop dp input (copy_with data dp next)

    | Some '.'   (* Output the character represented by the byte at the data pointer. *)
      -> let current_char = Option.bind (List.nth data dp) Char.of_int in
      let () = match current_char with
        | Some x -> print_char x
        | None -> failwith "Index out of bounds on print"
      in
      loop dp input data

    | Some ','   (* Read one byte and store it at the memory location pointed by data pointer. *)
      -> loop dp (List.tl_exn input) (copy_with data dp (List.hd_exn input))

    | Some '['   (* If the byte pointed by data pointer is zero,
                    then move instruction pointer to next matching ']',
                    otherwise move instruction pointer to next command. *)
      -> if phys_equal (List.nth_exn data dp) 0
      then interpret program (budget - 1) (find_loop_end ip program) dp input data
      else loop dp input data

    | Some ']'   (* If the byte pointed by data pointer is non-zero,
                    then move instruction pointer to previous matching '[' command,
                    otherwise to next command. *)
      -> if phys_equal (List.nth_exn data dp) 0
      then loop dp input data
      else interpret program (budget - 1) (find_loop_start ip program) dp input data
    | _ -> ()

let rec read_lines () =
  try let line = read_line () in
    line :: read_lines()
  with
    End_of_file -> []

let string_to_list = Fn.compose List.rev String.to_list_rev

let () =
  let (raw_input, raw_program) = match read_lines () with
    | _::raw_input::raw_program -> (raw_input, raw_program)
    | _ -> failwith "Not enough input lines"
  in
  let input = raw_input
              |> String.to_list_rev
              |> List.tl_exn (* Remove the '$' that represents end of input *)
              |> Caml.List.map Char.to_int
              |> List.rev
  in
  let program = raw_program
                |> Caml.List.map string_to_list
                |> Caml.List.concat
                |> Caml.List.filter (fun c -> List.exists ['>'; '<'; '+'; '-'; '.'; ','; '['; ']'] ((=) c))
                |> Array.of_list
  in
  interpret program 100000 0 0 input [0];
  print_newline ()
