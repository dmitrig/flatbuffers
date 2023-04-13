type offset
type t

val create : ?init_capacity:int -> unit -> t
val reset : t -> unit
val start_table : t -> n_fields:int -> t
val end_table : t -> offset
val start_vector : t -> n_elts:int -> elt_size:int -> unit
val end_vector : t -> offset

val finish
  :  ?identifier:string
  -> ?size_prefixed:bool
  -> 'a Primitives.t
  -> t
  -> offset
  -> 'a

(* new api? *)
val save_slot : id:int -> t -> unit
val prep : align:int -> bytes:int -> t -> unit
val current_offset : t -> offset

(* write values to current offset *)
val set_scalar : 'a Primitives.ty -> t -> int -> 'a -> unit
val set_uoffset : t -> int -> offset -> unit
val set_string : t -> int -> string -> unit
val set_padding : t -> int -> int -> unit

(* push *)
val push_slot_scalar : 'a Primitives.ty -> int -> 'a -> t -> t
val push_slot_scalar_default : 'a Primitives.ty -> int -> default:'a -> 'a -> t -> t
val push_slot_ref : int -> offset -> t -> t
val push_slot_union : int -> int -> Primitives.T.ubyte -> offset -> t -> t
val push_slot_struct : (t -> int -> 'a -> unit) -> int -> int -> int -> 'a -> t -> t

(* vectors *)
val create_vector : 'a Primitives.ty -> t -> 'a array -> offset
val create_vector_ref : t -> offset array -> offset
val create_vector_struct : (t -> int -> 'a -> unit) -> size:int -> t -> 'a array -> offset
val create_string : t -> string -> offset
val create_shared_string : t -> string -> offset
