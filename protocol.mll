{
  open Room
  open Hashtbl

  let rooms = create 16
  let users = create 16
    
  exception Username_exists
      
  let rec new_user (username: string): string =
    if fold (fun _ name acc -> acc or (name=username)) users false then
      raise Username_exists
    else
      let token =
	let high = Int64.to_string (Random.int64 Int64.max_int)
	and low = Int64.to_string (Random.int64 Int64.max_int) in
	high^low in
      if mem users token then
	new_user username
      else
	(add users token username; token)

}

let digit = ['0'-'9']
let id = (['a'-'z' 'A'-'Z' '0'-'9']*) as id
let rest = _* as rest

rule unqualified = parse
  | "Hello."             { "Name?" }
  | "My name is " id "." { try
			     print_string ("My name is "^id^".");
			     let token = new_user id in
			     ("Your token is "^token^".")
                           with Username_exists ->
			     "Username taken."
			 }
  | id": "rest           { try
                             let username = find users id in
			     print_string (username^": "^rest);
			     qualified username (Lexing.from_string rest)
                           with
			     | Not_found ->
			       print_endline ("No such id: "^id);
			       "ID not found." }
  | _                    { print_endline "Garbage";
		           "Garbage" }

and qualified name = parse
  | "Start "id"."      { try 
			   ignore (find rooms id);
			   "Room already started."
                         with Not_found ->
			   add rooms id (Room.create name id);
			   (id^": "^name^" started room "^id^".") }
  | "Join "id"."       { try
			   let room = find rooms id in
			   Room.add_user room name;
		           (id^": "^name^" has joined the room.")
                         with
			   | Not_found -> 
			    print_endline ("No such room: "^id^".");
			    "No such room." }
  | "Leave "id"."      { try
		           let room = find rooms id in
			   Room.remove_user room name;
		           "OK."
                         with
                          | Not_found ->
			    "No such room."  }
  | "?"id              { try
		           let room = find rooms id in 
			   String.concat "\n" (get_backlog room name)
                         with
                          | Not_found ->
			    "No such room." }
  | "!"id" "rest       { try
		           let room = find rooms id in 
			   user_message room name rest;
			   String.concat "\n" (get_backlog room name)
                         with
                          | Not_found ->
			    "No such room." }
  | "Quit."            { iter (fun _ room -> Room.remove_user room name) rooms;
		         "Goodbye."  }
  | _                  { "Garbage." }

{
  let process_message msg =
    let lexbuf = Lexing.from_string msg in
    try
      let response = unqualified lexbuf in
      print_endline (" => "^response);
      response
    with _ -> "Garbage."
}
