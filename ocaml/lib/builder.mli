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

(* shared strings *)
val find_shared_string : t -> string -> offset option
val add_shared_string : t -> string -> offset -> unit
