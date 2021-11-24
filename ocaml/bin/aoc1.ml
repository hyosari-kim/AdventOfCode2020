open Lib

let filename = "/Users/hyoeunkim/code/2020_AOC_Rescript/ocaml/input/day1/sample.txt"

let () = 
  let lines = Loader.read_file_to_string_list filename in  List.iter print_endline lines