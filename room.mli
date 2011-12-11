
type t
type username = string

val create : username -> string -> t
val get_backlog : t -> username -> String.t list
val add_user : t -> username -> unit
val remove_user : t -> username -> unit
val user_message : t -> string -> string -> unit
