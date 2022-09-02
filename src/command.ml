open Map_gen

type object_phrase = string list

type spot = int * int

type command =
  | Start
  | Shovel of spot
  | Flag of spot
  | Clear of spot
  | Create of config
  | Rules
  | Reset
  | Error
  | Quit

exception Empty

exception Malformed of string

let user_fig = function
  | numr :: numc :: numb :: t ->
      let numr = int_of_string numr in
      let numc = int_of_string numc in
      let numb = int_of_string numb in
      if numr > 0 && numc > 0 && numb > 0 && numb < numr * numc then
        { rows = numr; cols = numc; bombs = numb }
      else raise (Malformed "Invalid input")
  | _ -> raise (Malformed "Invalid input")

let spot = function
  | [ h ] -> raise (Malformed "Invalid input")
  | [ h1; h2 ] -> (int_of_string h1, int_of_string h2)
  | _ -> raise (Malformed "Invalid input")

let transform = function
  | [] -> raise Empty
  | [ h ] ->
      if h = "Start" then Start
      else if h = "Reset" then Reset
      else if h = "Quit" then Quit
      else raise (Malformed "Invalid input")
  | h :: t ->
      if h = "Shovel" then Shovel (spot t)
      else if h = "Flag" then Flag (spot t)
      else if h = "Clear" then Clear (spot t)
      else if h = "Create" then Create (user_fig t)
      else raise (Malformed "Invalid input")

let parse str =
  str
  |> String.split_on_char ' '
  |> List.filter (fun word -> word <> "")
  |> List.map (fun word -> String.capitalize_ascii word)
  |> transform

let fin_parse (cmd : command) =
  try cmd with
  | Malformed _ -> Error
  | x -> x
