open Hashtbl

(* Weakly Linked List Module *)

module type DataObject = sig
  type t
end

module MakeWeaklyLinkedList (Data : DataObject)= struct
  type node = { 
    data: Data.t; 
    mutable next: node option
  }

  type cursor = node ref

end

(* End Weakly Linked List Module *)

module ChatHistory = MakeWeaklyLinkedList (String);;

type username = string

type t = { 
  name: string;
  mutable last: ChatHistory.node;
  mutable logs: (username, ChatHistory.cursor) Hashtbl.t
}

let _announce room message =
  let announcement = { 
    ChatHistory.data = room.name^": "^message; 
    ChatHistory.next = None } in
  room.last.ChatHistory.next <- Some announcement;
  room.last <- announcement

let get_backlog room username =
  let rec read_log node acc =
    match node.ChatHistory.next with
      |	None -> List.rev acc
      | Some next -> 
	read_log next (next.ChatHistory.data::acc) in
  let backlog = read_log !(find room.logs username) [] in
  replace room.logs username (ref room.last);
  backlog

let add_user room username =
  _announce room (username^" has joined the room.");
  add room.logs username (ref room.last)

let remove_user room username =
  if Hashtbl.mem room.logs username then
    (remove room.logs username;
     _announce room (username^" has left the room."))

let user_message room username message =
  if Hashtbl.mem room.logs username then
    _announce room (username^": "^message)

let create (username: username) room_name = 
  let room = {
    name = room_name;
    last = {
      ChatHistory.data = ("User "^username^" started "^room_name); 
      ChatHistory.next = None };
    logs = Hashtbl.create 16 } in
  add_user room username;
  room
