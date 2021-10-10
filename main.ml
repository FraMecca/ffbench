(*
- prendi un file 
- prendi 6 sample da 20 secondi l'uno
- concatenali
- genera X nuovi file con Y parametri differenti
- genera un sample con i parametri originali (come copiarli/calcolarli?)
- calcola ssim per ognuno: -lavfi ssim=stats_file=ssim_logfile.txt -f null -
- presenta i risultati e salvali su file


- mostra il comando ffmpeg per il migliore dato un file di risultati
- mosta vari parametri audio
 *)

let tryhandle f = try Ok(f ()) with err -> Error err

let result_list_to_result (lst: (('a, string) result list)) : ('a list, string) result = 
  if List.exists Result.is_error lst then
    lst |> List.filter Result.is_error |> List.map Result.get_error |> String.concat "; " |> Result.error
  else
    lst |> List.map Result.get_ok |> Result.ok

let logerror str e = let _ = Log.error "%s" str in e

let logerrormsg str e = let _ = Log.error "%s %s" str (Result.get_error e) in e

let loginfo fmt obj = let _ = Log.info fmt in obj

let iota start gap n =
  let rec iota_ start n acc =
    if n = 0 then acc
    else 
      iota_ (start + gap) (n-1) (start::acc)
  in
  iota_ start n [] |> List.rev


type inputfile = { name:string; nsamples:int; sampleduration:int }
type dstfile = { name:string; src:string; params:string }
type score = { mean:float; min:float; max:float; variance:float; median: float }
type ffmpeg_result = { name:string; score:score; size:int; parameters:string; }

type action =
  | ComputeOriginalParams of string
  | MakeSnapshots of inputfile (* Take nsamples from input file *)
  | MakeSource of (string list * string) (* Concatenate previous samples into a single file *)
  | MakeDst of dstfile (* Transcode source into a new file using one of the many parameters *)
  | ScoreDst of string (* Generate scores for the destination file specified *)

(* Very stupid communication protocol, semicolon divided *)
let parse_action buf =
  match String.split_on_char ';' buf with
  | "PARAMS"::fname::[] -> ComputeOriginalParams fname |> Option.some
  | "MAKESRC"::src::samples -> MakeSource (samples, src) |> Option.some
  | "MAKEDST"::src::dst::params::[] -> MakeDst { name=dst; src=src; params=params } |> Option.some
  | "SCORE"::fname::[] -> ScoreDst fname |> Option.some
  | "SNAPSHOTS"::fname::nsamples::sampleduration::[] ->
     begin match (int_of_string_opt nsamples, int_of_string_opt sampleduration) with
     | Some n, Some d -> MakeSnapshots { name=fname; nsamples=n; sampleduration=d } |> Option.some
     | _ -> None
     end
  | _ -> None

type resultant = (* string * string = filename, md5sum *)
  | OriginalParams of (string * string * string) (* last is params *)
  | Snapshots of (string * string) list
  | MakeSource of (string * string)
  | MakeDst of (string * string)
  | ScoreDst of (string * string * ffmpeg_result)

let score_to_string_list s =
  ["mean"; string_of_float s.mean;
   "min"; string_of_float s.min;
   "max"; string_of_float s.max;
   "variance"; string_of_float s.variance;
   "median"; string_of_float s.median]

let generate_reply = function
  | OriginalParams (fname, hash, params) -> String.concat ";" ["PARAMS"; fname; hash; params]
  | MakeSource (fname, hash) -> String.concat ";" ["MAKESRC"; fname; hash]
  | MakeDst (fname, hash) -> String.concat ";" ["MAKEDST"; fname; hash]
  | ScoreDst (logname, hash, fr) ->
     ["PARAMS"; logname; hash; fr.name; string_of_int fr.size; fr.parameters;]@(score_to_string_list fr.score)
     |> String.concat ";" 
  | Snapshots lst ->
     let lst = List.map (fun (f, h) -> f^";"^h) lst |> String.concat ";" in
     "SNAPSHOTS"^lst
    

let run cmd =
  let timeout_1h = "timeout 1h "
  in
  let prog = Unix.open_process_full (timeout_1h^cmd) [||]
  in
  let ch, _, _ = prog
  in
  let lines = Interop.line_stream_of_channel ch
  in
  let rec to_list stream acc =
    try (Stream.next stream)::acc |> to_list stream 
    with Stream.Failure -> let _ = close_in ch in acc
  in
  let ret = to_list lines [] |> List.rev
  in
  match Unix.close_process_full prog with
  | WEXITED 0 -> Ok ret
  (* | WEXITED n -> Error n *)
  (* | WSIGNALED n -> Error n *)
(* | WSTOPPED n -> Error n *)
  | _ -> Error ("Failed subprocess: "^cmd)

let original_parameters inputfile =
  let cmd = "mediainfo "^inputfile^" | grep 'Encoding settings' | cut -d':' -f2- | tr '/' '\n' | sed 's/ //'"
  in
  run cmd |> Result.map (String.concat " ")

let duration inputfile =
  let minduration = inputfile.nsamples * inputfile.sampleduration
  in
  let div1000 f = Float.div f 1000.0
  in
  let cmd = "mediainfo -f "^inputfile.name^" | grep Duration | head -n 1 | cut -d: -f 2 | sed 's/ //'"
  in
  let dur = run cmd |> Result.map (fun ls -> ls |> List.hd |> float_of_string |> div1000 |> Float.floor |> int_of_float)
  in
  Result.bind dur (fun dur -> if dur >= minduration then Ok dur else Error "Video file lasts less than 120 seconds")

let make_source_samples inputfile duration =
  let sampleduration_st = string_of_int inputfile.sampleduration
  in
  let compute_samples duration nsamples sampleduration =
    (* Dividi il file in 8 parti uguali
     * estraine le 6 centrali, evitando inizio e fine
     * se il file è più breve di 2 minuti e 40 secondi usa tutto il file come sample *)
    let minduration = nsamples * sampleduration
    in
    let max = minduration / nsamples * (nsamples+2)
    in
    if duration < minduration then
      [0]
    else if duration < max then
      let scarto = (duration - minduration) / 2
      in [0+scarto]
    else
      let gap = duration / (nsamples+2)
      in
      iota gap gap nsamples
  in
  let extract_sample inputfile start =
    let start = string_of_int start
    in
    let filename = "original"^start^".mkv"
    in
    let cmd = "ffmpeg -y -ss "^start^" -t "^sampleduration_st^" -i "^inputfile^" "^filename
    in
    run cmd |> Result.map (fun _ -> filename)
  in
  compute_samples duration inputfile.nsamples inputfile.sampleduration
  |> List.map (extract_sample inputfile.name)
  |> result_list_to_result
  

let concat_video filenames destination =
  let create_listtxt files =
    let fp = open_out "list.txt" in
    let _ = files |> List.iter (fun f -> Printf.fprintf fp "file %s\n" f) in close_out fp
  in
  let cmd = "ffmpeg -y -f concat -i list.txt -c copy "^destination
  in
  let _ = create_listtxt filenames in
  match run cmd with
  | Ok _ -> Ok destination
  | Error _ -> Error "Can't concatenate source samples into a single file"

let transcode_file src dst params =
  let cmd = "ffmpeg -y -i "^src^" "^params^" "^dst
  in
  let _ = Log.info "Transcoding %s using: %s" dst cmd
  in
  match run cmd with
  | Ok _ -> Ok dst
  | Error _ -> Error ("failed encode for: "^dst)

let score_result_filename mkv = "ssim_logfile_"^mkv^".txt"

let score_files src files =
  let score_file file =
    let score_logfile = score_result_filename file
    in
    let cmd = "ffmpeg -i "^src^" -i "^file^" -lavfi ssim=stats_file="^score_logfile^" -f null -"
    in
    let _ = Log.info "Computing score using: %s" cmd
    in
    match run cmd with
    | Ok _ -> Ok score_logfile
    | Error _ -> Error ("failed encode for: "^file)
  in
  List.map score_file files |> result_list_to_result

let compute_file_score logname =
  let file_to_list filename =
    let inch = open_in filename
    in
    let rec to_list stream acc =
      try (Stream.next stream)::acc |> to_list stream 
      with Stream.Failure -> let _ = close_in inch in acc
    in
    let stream = inch |> Interop.line_stream_of_channel
    in
    to_list stream [] |> List.rev
  in
  let make_ffmpeg_result logname scores =
    let len = List.length scores |> float_of_int in
    let max = List.fold_left max 0.0 scores in
    let min = List.fold_left min max scores in
    let avg = let sum = List.fold_left (fun a b -> a +. b) 0.0 scores in sum /. len in
    let var = 0.0 in
    let median = 0.0 in
    let score = { mean=avg; min=min; max=max; variance=var; median=median } in
    { name=logname; score=score; size=0; parameters=""; }
  in
  let get_aggr_score line =
    line
    |> String.split_on_char ' '
    |> (fun l -> List.nth l 4)
    |> (fun s -> String.sub s 0 (String.length s))
    |> float_of_string
  in
  try
    logname
    |> file_to_list
    |> List.map get_aggr_score
    |> make_ffmpeg_result logname
    |> Result.ok
  with err -> Result.Error err
  
  

let hashof filename =
  let cmd = "md5sum "^filename^" | cut -f1 -d ' '" in
  run cmd |> Result.map List.hd
let assoc_hash file = hashof file |> Result.map (fun hash -> (file, hash))

let handle_action = function
  | ComputeOriginalParams file ->
     begin match original_parameters file with
     | Ok params ->
        hashof file
        |> Result.map (fun h -> OriginalParams (file, h, params))
        |> Result.map_error (fun _ -> "md5sum")
     | Error err -> Error err
     end
  | MakeSnapshots inputf ->
     let r = duration inputf |> Result.map (make_source_samples inputf) |> Result.join in
     begin match r with
     | Ok stl ->
        stl
        |> List.map assoc_hash
        |> result_list_to_result
        |> Result.map (fun r -> Snapshots r)
     | Error err -> Error err
     end
  | MakeSource (samples, srcname) ->
     begin match concat_video samples srcname with
     | Ok c_source -> hashof c_source |> Result.map (fun h -> MakeSource (c_source, h))
     | Error err -> Error err
     end
  | MakeDst dstfile ->
     begin match transcode_file dstfile.src dstfile.name dstfile.params with
     | Ok dst -> assoc_hash dst |> Result.map (fun t -> MakeDst t)
     | Error err -> Error err
     end
  | ScoreDst logfile ->
     match compute_file_score logfile with
     | Ok ffmpeg_result -> assoc_hash logfile |> Result.map (fun t -> ScoreDst (fst t, snd t, ffmpeg_result))
     | Error exn -> Printexc.to_string exn |> Result.error

let parse_and_respond buf =
  let resultant = parse_action buf |> Option.map handle_action |> Option.to_result ~none:"" |> Result.join in
  match resultant with
  | Ok res -> Ok (generate_reply res)
  | Error err -> Error err

let () =
  Log.set_log_level Log.INFO;
  Log.color_on();
  Log.set_output stderr;

  Interop.handle_messages parse_and_respond
