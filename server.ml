open Unix
open Protocol

type job = in_channel * out_channel

(* Producer/Consumer Queue for jobs *)

let _job_queue = Queue.create ()
let lock = Mutex.create ()
let condition = Condition.create ()

let enqueue job = 
  Mutex.lock lock;
  Queue.add job _job_queue;
  Condition.signal condition;
  Mutex.unlock lock

let rec dequeue () =
  Mutex.lock lock;
  let job = ref None in
  while !job = None do
    try
      job := Some (Queue.take _job_queue)
    with Queue.Empty ->
      Condition.wait condition lock
  done;
  Mutex.unlock lock;
  match !job with
    | Some job -> job
    | None -> raise (Failure "Something's not right")

(* End Queue *)

let _home_socket = ref None

let rec init ?port:(port=8086) () =
  try
    print_endline "Initializing";
    let my_name = gethostname () in
    let my_entry = gethostbyname my_name in
    let my_address = my_entry.h_addr_list.(0) in
    let sckt = socket PF_INET SOCK_STREAM 0 in
    bind sckt (ADDR_INET(my_address, port));
    listen sckt 256;
    print_endline ("Ready on port "^(string_of_int 8086));
    _home_socket := Some sckt
  with _ -> init ()

let rec _get_socket () =
  match !_home_socket with
    | None -> init (); _get_socket ()
    | Some sckt -> sckt

let accept_connections () =
  let socket = _get_socket () in
  while true do
    let (s_descr,_) = (accept socket) in
    let job = ((in_channel_of_descr s_descr),(out_channel_of_descr s_descr)) in
    enqueue job
  done

let process_job () =
  while true do
    let (input,output) = dequeue () in
    let msg = input_line input in
    output_string output ((Protocol.process_message msg)^"\n");
    flush output;
    Thread.delay 0.1;
    close_out output
  done

let _ =
  init ();
  for i=0 to 3 do
    ignore (Thread.create process_job ())
  done;
  accept_connections ()
