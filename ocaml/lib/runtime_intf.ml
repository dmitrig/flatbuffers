(** User-facing runtime lib interface.

    Just defines scalars, buffer and builder types, and vectors of
    each scalar type. Most of the user-facing API is in generated code *)
module type Intf = sig
  (** Builder *)
  module Builder : sig
    type t

    val create : ?init_capacity:int -> unit -> t
    val reset : t -> unit
  end

  (** Abstract buffer handle. Parameter is an existential associating offsets with buffers *)
  type 'b buf

  (** Offset in buffer 'b containing data of type 't *)
  type ('b, 't) fb

  (** A possibly invalid offset *)
  type ('b, 't) fbopt

  (** A buffer and offset to a root table of type 't *)
  type 't root = Root : 'b buf * ('b, 't) fb -> 't root

  (** An offset into a builder containing data of type 't *)
  type 't wip

  (** Signatures *)

  module type VectorS = sig
    type t
    type 'b elt
    type builder_elt

    val length : 'b buf -> ('b, t) fb -> int
    val get : 'b buf -> ('b, t) fb -> int -> 'b elt
    val iter : 'b buf -> ('b elt -> unit) -> ('b, t) fb -> unit
    val to_list : 'b buf -> ('b, t) fb -> 'b elt list
    val to_array : 'b buf -> ('b, t) fb -> 'b elt array
    val to_seq : 'b buf -> ('b, t) fb -> 'b elt Seq.t
    val create : Builder.t -> builder_elt array -> t wip
  end

  module type ScalarS := sig
    type t

    module Vector : VectorS with type 'b elt := t and type builder_elt := t
  end

  (** Scalars *)

  module Bool : ScalarS with type t = Primitives.T.bool
  module Byte : ScalarS with type t = Primitives.T.byte
  module UByte : ScalarS with type t = Primitives.T.ubyte
  module UType : ScalarS with type t = Primitives.T.ubyte
  module Short : ScalarS with type t = Primitives.T.short
  module UShort : ScalarS with type t = Primitives.T.ushort
  module Int : ScalarS with type t = Primitives.T.int
  module UInt : ScalarS with type t = Primitives.T.uint
  module Long : ScalarS with type t = Primitives.T.long
  module ULong : ScalarS with type t = Primitives.T.ulong
  module Float : ScalarS with type t = Primitives.T.float
  module Double : ScalarS with type t = Primitives.T.double

  (** String *)
  module String : sig
    include VectorS with type 'b elt := UByte.t and type builder_elt := UByte.t

    val to_string : 'b buf -> ('b, t) fb -> string
    val create : Builder.t -> string -> t wip
    val create_shared : Builder.t -> string -> t wip

    module Vector : VectorS with type 'b elt := ('b, t) fb and type builder_elt := t wip
  end

  (** Flat options for offsets *)
  module Option : sig
    type ('b, 't) t = ('b, 't) fbopt

    val is_none : ('b, 't) t -> bool
    val is_some : ('b, 't) t -> bool
    val value : ('b, 't) t -> default:('b, 't) fb -> ('b, 't) fb
    val get : ('b, 't) t -> ('b, 't) fb
    val fold : none:'a -> some:(('b, 't) fb -> 'a) -> ('b, 't) t -> 'a
    val iter : (('b, 't) fb -> unit) -> ('b, 't) t -> unit
    val to_option : ('b, 't) t -> ('b, 't) fb option
  end
end

(** Full runtime lib interface, only for use by generated code *)
module type Intf_impl = sig

  (* builder with same scalar types *)
  module Builder = Builder

  (* Buffers and offsets are abstract to generated code. Non-scalar types are
      represented as offsets. Phantom param for safety in generated code *)

  type 'b buf
  (* type offset *)
  type ('b, 't) fb = Read.offset
  type ('b, 't) fbopt = Read.offset
  type 'a wip = Builder.offset
  type 't root = Root : 'b buf * ('b, 't) fb -> 't root

  val get_identifier : ?off:int -> ?size_prefixed:bool -> 'b Primitives.t -> 'b -> string
  val get_root : ?off:int -> ?size_prefixed:bool -> 'b Primitives.t -> 'b -> 'a root

  module type VectorS = sig
    type t
    type 'b elt
    type builder_elt

    val length : 'b buf -> ('b, t) fb -> int
    val get : 'b buf -> ('b, t) fb -> int -> 'b elt
    val iter : 'b buf -> ('b elt -> unit) -> ('b, t) fb -> unit
    val to_list : 'b buf -> ('b, t) fb -> 'b elt list
    val to_array : 'b buf -> ('b, t) fb -> 'b elt array
    val to_seq : 'b buf -> ('b, t) fb -> 'b elt Seq.t
    val create : Builder.t -> builder_elt array -> t wip
  end

  module type ScalarS := sig
    type t
    type default

    val read_offset : 'b buf -> Read.offset -> int -> t
    val read_table_default : 'b buf -> Read.offset -> int -> default:t -> t
    val read_table : 'b buf -> Read.offset -> int -> t
    val read_table_opt : 'b buf -> Read.offset -> int -> t option
    val push_slot : int -> t -> Builder.t -> Builder.t
    val push_slot_default : int -> default:t -> t -> Builder.t -> Builder.t
    val of_default : default -> t
    val to_default : t -> default

    module Vector : VectorS with type 'b elt := t and type builder_elt := t
  end

  module Bool : ScalarS with type default := bool and type t = Primitives.T.bool
  module Byte : ScalarS with type default := int64 and type t = Primitives.T.byte
  module UByte : ScalarS with type default := int64 and type t = Primitives.T.ubyte
  module UType : ScalarS with type default := int64 and type t = Primitives.T.ubyte
  module Short : ScalarS with type default := int64 and type t = Primitives.T.short
  module UShort : ScalarS with type default := int64 and type t = Primitives.T.ushort
  module Int : ScalarS with type default := int64 and type t = Primitives.T.int
  module UInt : ScalarS with type default := int64 and type t = Primitives.T.uint
  module Long : ScalarS with type default := int64 and type t = Primitives.T.long
  module ULong : ScalarS with type default := int64 and type t = Primitives.T.ulong
  module Float : ScalarS with type default := float and type t = Primitives.T.float
  module Double : ScalarS with type default := float and type t = Primitives.T.double

  module Struct : sig
    val read_offset : 'b buf -> Read.offset -> int -> Read.offset
    val read_table : 'b buf -> Read.offset -> int -> Read.offset
    val read_table_opt : 'b buf -> Read.offset -> int -> Read.offset

    val push_slot
      :  (Builder.t -> int -> 'a -> unit)
      -> int (* size *)
      -> int (* align *)
      -> int (* field *)
      -> 'a
      -> Builder.t
      -> Builder.t

    module Vector (T : sig
      type builder_elt

      val size : int
      val set : Builder.t -> int -> builder_elt -> unit
    end) : VectorS with type 'b elt := Read.offset and type builder_elt := T.builder_elt
  end

  module Ref : sig
    val size : int
    val read_table : 'b buf -> Read.offset -> int -> Read.offset
    val read_table_opt : 'b buf -> Read.offset -> int -> Read.offset
    val push_slot : int -> Builder.offset -> Builder.t -> Builder.t
    val push_union : int -> int -> UByte.t -> Builder.offset -> Builder.t -> Builder.t

    module Vector :
      VectorS with type 'b elt := Read.offset and type builder_elt := Builder.offset
  end

  module String : sig
    include VectorS with type 'b elt := UByte.t and type builder_elt := UByte.t

    val to_string : 'b buf -> ('b, t) fb -> string
    val create : Builder.t -> string -> t wip
    val create_shared : Builder.t -> string -> t wip

    module Vector : VectorS with type 'b elt := ('b, t) fb and type builder_elt := t wip
  end

  module Option : sig
    type ('b, 't) t = ('b, 't) fbopt

    val is_none : ('b, 't) t -> bool
    val is_some : ('b, 't) t -> bool
    val value : ('b, 't) t -> default:('b, 't) fb -> ('b, 't) fb
    val get : ('b, 't) t -> ('b, 't) fb
    val fold : none:'a -> some:(('b, 't) fb -> 'a) -> ('b, 't) t -> 'a
    val iter : (('b, 't) fb -> unit) -> ('b, 't) t -> unit
    val to_option : ('b, 't) t -> ('b, 't) fb option
  end
end
