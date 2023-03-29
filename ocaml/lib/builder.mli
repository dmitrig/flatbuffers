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
val set_bool : t -> int -> Primitives.T.bool -> unit
val set_byte : t -> int -> Primitives.T.byte -> unit
val set_ubyte : t -> int -> Primitives.T.ubyte -> unit
val set_short : t -> int -> Primitives.T.short -> unit
val set_ushort : t -> int -> Primitives.T.ushort -> unit
val set_int : t -> int -> Primitives.T.int -> unit
val set_uint : t -> int -> Primitives.T.uint -> unit
val set_uoffset : t -> int -> offset -> unit
val set_long : t -> int -> Primitives.T.long -> unit
val set_ulong : t -> int -> Primitives.T.ulong -> unit
val set_float : t -> int -> Primitives.T.float -> unit
val set_double : t -> int -> Primitives.T.double -> unit
val set_string : t -> int -> string -> unit
val set_padding : t -> int -> int -> unit

(* shared strings *)
val find_shared_string : t -> string -> offset option
val add_shared_string : t -> string -> offset -> unit
