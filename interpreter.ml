open Core_kernel.Std

let copy_with (l: 'a list) (index: int) (value: 'a) : 'a list =
    let rec loop (result: 'a list) (index: int) = function
        | head::tail -> if index = 0
            then loop (value::result) (index - 1) tail
            else loop (head::result) (index - 1) tail
        | [] -> List.rev result
    in
    loop [] index l

let rec list_after (index: int) (l: char list) : char list =
    match index, l with
    | 0, _ -> l
    | _, head::tail -> list_after (index - 1) tail
    | _, [] -> failwith "Invalid list index"

let rec find_index (pred: 'a -> bool) (index: int) (l: 'a list) : int =
    match l with
    | head::tail -> if pred head
        then index
        else find_index pred (index + 1) tail
    | [] -> failwith "Could not find loop end"

let find_loop_end (start: int) (l: char list) : int =
    list_after start l
    |> find_index (fun x -> x = ']') start

let index_from_end (l: 'a list) (index: int) =
    List.length l - 1 - index

let find_loop_start (start: int) (l: char list) : int =
    List.rev l
    |> find_index (fun x -> x = '[') (index_from_end l start)
    |> index_from_end l

let rec interpret (budget: int) (ip: int) (dp: int) (input: char list) (program: char list) (data: int list) : unit =
    if budget = 0 then Printf.printf "\nPROCESS TIME OUT. KILLED!!!\n" else
    let loop = interpret (budget - 1) (ip + 1) in
    match List.nth program ip with
    | '>'   (* Increment data pointer so that it points to next location in memory. *)
        -> let next = loop (dp + 1) input program in
            if (dp + 1) >= List.length data
            then next (List.append data [0])
            else next data

    | '<'   (* Decrement data pointer so that it points to previous locaion in memory. *)
        -> loop (dp - 1) input program data

    | '+'   (* Increment the byte pointed by data pointer by 1. If it is already at its maximum value, 255, then new value will be 0. *)
        -> let current = List.nth data dp in
            let next = if current = 255
                then 0
                else current + 1
            in
            loop dp input program (copy_with data dp next)

    | '-'   (* Decrement the byte pointed by data pointer by 1. If it is at its minimum value, 0, then new value will be 255. *)
        -> let current = List.nth data dp in
            let next = if current = 0
                then 255
                else current - 1
            in
            loop dp input program (copy_with data dp next)

    | '.'   (* Output the character represented by the byte at the data pointer. *)
        -> print_char (Char.chr (List.nth data dp));
        loop dp input program data

    | ','   (* Read one byte and store it at the memory location pointed by data pointer. *)
        -> loop dp (List.tl input) program (copy_with data dp (Char.code (List.hd input)))

    | '['   (* If the byte pointed by data pointer is zero, then move instruction pointer to next matching ']', otherwise move instruction pointer to next command. *)
        -> if List.nth data dp = 0
            then interpret (budget - 1) (find_loop_end dp program) dp input program data
            else loop dp input program data

    | ']'   (* If the byte pointed by data pointer is non-zero, then move instruction pointer to previous matching '[' command, otherwise to next command. *)
        -> if List.nth data dp != 0
            then interpret (budget - 1) (find_loop_start dp program) dp input program data
            else loop dp input program data
    | _ -> print_newline ()

let rec read_lines () =
    try let line = read_line () in
        line :: read_lines()
    with
        End_of_file -> []

let string_to_list = Core_kernel.Fn.compose List.rev Core_kernel.Core_string.to_list_rev

let () =
    let _::raw_input::raw_program = read_lines() in
    let input = raw_input
    |> Core_kernel.Core_string.to_list_rev
    |> List.tl (* Remove the '$' that represents end of input *)
    |> List.rev
    in
    let program = raw_program
    |> List.map string_to_list
    |> List.concat
    |> List.filter (fun c -> List.exists ((=) c) ['>'; '<'; '+'; '-'; '.'; ','; '['; ']'])
    in
    interpret 100000 0 0 input (program @ ['$']) [0]
