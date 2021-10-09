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

let parameters = [
    (* default parameters, different crf *)
    "-crf 15 -codec:v libx265 -preset slow";
    "-crf 18 -codec:v libx265 -preset slow";
    "-crf 21 -codec:v libx265 -preset slow";
    (* ANIME PRESET *)
    (* general purpose *)
    "crf=18 -x265-params \"limit-sao:bframes=8:psy-rd=1:aq-mode=3\"";
    "crf=19 -x265-params \"limit-sao:bframes=8:psy-rd=1:aq-mode=3\"";
    "crf=20 -x265-params \"limit-sao:bframes=8:psy-rd=1:aq-mode=3\"";
    (* Flat slow anime, slice of life, everything is well lit *)
    "crf=18 -x265-params \"bframes=8:psy-rd=1:aq-mode=3:aq-strength=0.8:deblock=1,1\"";
    "crf=20 -x265-params \"bframes=8:psy-rd=1:aq-mode=3:aq-strength=0.8:deblock=1,1\"";
    (* Some dark scene, some battle scene, motion + fancy FX *)
    "crf=18 limit-sao:bframes=8:psy-rd=1.5:psy-rdoq=2:aq-mode=3\"";
    "crf=19 limit-sao:bframes=8:psy-rd=1.5:psy-rdoq=2:aq-mode=3\"";
    (* Some dark scene, some battle scene, non-complex, motion only alternative *)
    "crf=20  -x265-params \"bframes=8:psy-rd=1:psy-rdoq=1:aq-mode=3:qcomp=0.8\"";
    (* Movie-tier dark scene, complex grain/detail *)
    "crf=16 -x265-params \"no-sao:bframes=8:psy-rd=1.5:psy-rdoq=4:aq-mode=3:ref=6\"";
    "crf=18 -x265-params \"no-sao:bframes=8:psy-rd=1.5:psy-rdoq=4:aq-mode=3:ref=6\"";
  ]

let sampledurationint = 5
let sampleduration = string_of_int sampledurationint
let nsamples = 2
let minduration = nsamples * sampledurationint
let samples_filename = "original_samples.mkv"

let tryhandle f = try Ok(f ()) with err -> Error err

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


let inputfile = "input.mkv" |> Filename.quote

type score = { mean:float; min:float; max:float; variance:float }
type ffmpeg_result = { name:string; score:score; size:int; parameters:string; encoder:string }

let run cmd =
  let timeout_1h = "timeout 1h "
  in
  let line_stream_of_channel channel =
    Stream.from (fun _ -> try Some (input_line channel) with End_of_file -> None)
  in
  let prog = Unix.open_process_full (timeout_1h^cmd) [||]
  in
  let ch, _, _ = prog
  in
  let lines = line_stream_of_channel ch
  in
  let rec to_list stream acc =
    try (Stream.next stream)::acc |> to_list stream 
    with Stream.Failure -> let _ = close_in ch in acc
  in
  let ret = to_list lines [] |> List.rev
  in
  match Unix.close_process_full prog with
  | WEXITED 0 -> Ok ret
  | WEXITED n -> Error n
  | WSIGNALED n -> Error n
  | WSTOPPED n -> Error n

let original_parameters () =
  let cmd = "mediainfo "^inputfile^" | grep 'Encoding settings' | cut -d':' -f2- | tr '/' '\n' | sed 's/ //'"
  in
  run cmd |> Result.map (String.concat " ")

let duration () =
  let div1000 f = Float.div f 1000.0
  in
  let cmd = "mediainfo -f "^inputfile^" | grep Duration | head -n 1 | cut -d: -f 2 | sed 's/ //'"
  in
  let dur = run cmd |> Result.map (fun ls -> ls |> List.hd |> float_of_string |> div1000 |> Float.floor |> int_of_float)
            |> Result.map_error (fun e -> "Can't compute duration: process exited with error: "^(string_of_int e))
  in
  Log.debug "%s" ("Computed duration|"^(match dur with | Ok d ->  string_of_int d  | _ -> "error"));
  Result.bind dur (fun dur -> if dur >= minduration then Ok dur else Error "Video file lasts less than 120 seconds")

let make_source_sample duration =
  let compute_samples duration =
    (* Dividi il file in 8 parti uguali
     * estraine le 6 centrali, evitando inizio e fine
     * se il file è più breve di 2 minuti e 40 secondi usa tutto il file come sample *)
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
      |> fun samples -> let _ = Log.debug "function: compute_samples=%s" (List.map string_of_int samples |> String.concat ", " ) in samples
  in
  let extract_sample start =
    Log.debug "function: extract_sample: %d" start;
    let start = string_of_int start
    in
    let filename = "original"^start^".mkv"
    in
    let cmd = "ffmpeg -y -ss "^start^" -t "^sampleduration^" -i "^inputfile^" "^filename
    in
    run cmd
    |> Result.map (fun _ -> let _ = loginfo "Extracted sample@%ss" start in filename)
    |> Result.map_error (logerror "Can't extract sample")
  in
  let concat_video filenames =
    let _ = Log.debug "function: concat_video" in
    let create_listtxt files =
      let fp = open_out "list.txt" in
      let _ = files |> List.iter (fun f -> Printf.fprintf fp "file %s\n" f) in close_out fp
    in
    let cmd = "ffmpeg -y -f concat -i list.txt -c copy "^samples_filename
    in
    let _ = create_listtxt filenames in
    match run cmd with
    | Ok _ -> Ok samples_filename
    | Error _ -> Error "Can't concatenate source samples into a single file"
  in
  let samples = compute_samples duration
                |> (fun l -> Log.debug "computed the interval for %d samples" (List.length l); List.map extract_sample l)
  in
  if samples |> List.exists (fun r -> Result.is_error r |> not) then
    samples |> List.map Result.get_ok |> concat_video 
  else 
    Error "Can't create short samples from the input file" |> logerrormsg "function make_source_sample:"

let () =
  Log.set_log_level Log.DEBUG;
  Log.color_on();
  Log.set_output stdout;

  let duration = duration ()
  in
  let source = Result.map make_source_sample duration |> Result.join
  in
  match source with
  | Ok source -> Printf.printf "%s " source
  | e -> let _ = logerrormsg "" e in ()
