open Core

let rec read_line ic lines =
  let line = In_channel.input_line ic in
    match line with
    | None -> lines
    | Some(str) -> read_line ic (str::lines)

let read_file_to_string_list filename =
  let in_ch = In_channel.create filename in read_line in_ch []