
(* Console *)

let screen_lock = Mutex.create ()

let write s =
  Mutex.lock screen_lock;
  print_string s;
  flush stdout;
  Mutex.unlock screen_lock

(* Asynchronous Updates *)

let rooms = ref []

let id: string option ref = ref None

let rec process_response input =
  let try_parse_token response =
    let regexp = Str.regexp "Your token is \\(\\([a-z]\\|[A-Z]\\|[0-9]\\)*\\)." in
    if Str.string_match regexp response 0 then
      (id := Some (Str.matched_group 1 response); true)
    else false
  and try_parse_room response =
    let args = Str.split (Str.regexp_string ":") response in
    if List.length args > 1 && not (List.mem (List.hd args) !rooms) then
      rooms := (List.hd args)::!rooms in
  try
    let response = input_line input in
    if try_parse_token response then () else
      (try_parse_room response;
       if response="" then () else
	 write (response^"\n"); process_response input)
  with End_of_file -> ()
    
let process_command server_address command =
  let input, output = Unix.open_connection server_address in
  (match !id with
    | Some token ->
      output_string output (token^": "^command^"\n")
    | None ->
      output_string output (command^"\n"));
  flush output;
  process_response input;
  close_out output

let update server_address () =
  let call_for_updates room =
    process_command server_address ("?"^room) in
  while true do
    Thread.delay 1.;
    match !id with
      | None -> ()
      | Some id ->
	List.iter call_for_updates !rooms
  done
  
let _ =
  let server_address = 
    (try
       let addr = Unix.inet_addr_of_string (Sys.argv.(1))
       and port = int_of_string (Sys.argv.(2)) in
       Unix.ADDR_INET(addr,port)
     with _ ->
       let addr = (Unix.gethostbyname (Unix.gethostname ())).Unix.h_addr_list.(0)
       and port = 8086 in
       Unix.ADDR_INET(addr,port)) in
  ignore (Thread.create (update server_address) ());
  while true do
    ignore (input_line stdin);
    Mutex.lock screen_lock;
    print_string "> ";
    flush stdout;
    let command = (input_line stdin) in
    Mutex.unlock screen_lock;
    process_command server_address command
  done;;
      
