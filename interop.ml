type requestmsg = Do of string | ParseErr of string
type replymsg = No of string | Yes of string

let line_stream_of_channel channel =
  Stream.from (fun _ -> try Some (input_line channel) with End_of_file -> None)

let parse_request buf =
  let start = String.sub buf 0 3 in
  if start = "DO|" then
    Do (String.sub buf 3 (String.length buf - 3))
  else
    ParseErr buf

let handle_messages parse_and_respond =
  let inch = Unix.in_channel_of_descr Unix.stdin in
  let stream = line_stream_of_channel inch in
  let reply str = print_endline str in
  let r = try Stream.next stream |> Result.ok with Stream.Failure -> Result.Error "No more input"
  in
  match r with
  | Ok buf ->
     begin match parse_request buf with
     | ParseErr b -> Log.error "ParseError |%s|" b 
     | Do act -> match parse_and_respond act with
                 | Ok resp -> reply resp
                 | Error e -> Log.fatal "%s" e
        end
  | Error _ -> Log.fatal "Error in interprocess communication"
