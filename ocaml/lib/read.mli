type offset = int

type _ tag =
  | TScalar : 'a Primitives.ty -> 'a tag
  | TRef : offset tag
  | TStruct :
      { sz : int
      ; align : int
      }
      -> offset tag

(** Generic ops *)
val get_val : 'a tag -> 'b Primitives.t -> 'b -> int -> 'a
val read_table : 'a tag -> 'b Primitives.t -> 'b -> int -> int -> 'a

(** Scalar ops *)
val read_table_default : 'a Primitives.ty -> 'b Primitives.t -> 'b -> offset -> int -> default:'a -> 'a
val read_table_opt : 'a Primitives.ty -> 'b Primitives.t -> 'b -> int -> int -> 'a option

val read_table_opt_ref : 'a Primitives.t -> 'a -> int -> int -> offset
val read_table_struct : 'a Primitives.t -> 'a -> int -> int -> int
val read_table_opt_struct : 'a Primitives.t -> 'a -> int -> int -> int

(** Vector ops *)
val length_vec : 'a Primitives.t -> 'a -> int -> int
val get_vec : 'a tag -> 'b Primitives.t -> 'b -> int -> int -> 'a
val to_list_vec : 'a tag -> 'b Primitives.t -> 'b -> int -> 'a list
val to_array_vec : 'a tag -> 'b Primitives.t -> 'b -> int -> 'a array
val to_seq_vec : 'a tag -> 'b Primitives.t -> 'b -> int -> 'a Seq.t
val iter_vec : 'a tag -> 'b Primitives.t -> 'b -> ('a -> unit) -> int -> unit
