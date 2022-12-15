module type Intf_types = sig
  type buf
  type bool
  type byte
  type ubyte
  type short
  type ushort
  type int
  type uint
  type long
  type ulong
  type float
  type double
end

module type Intf = sig
  module T : Intf_types

  (** Data read by the runtime, not abstract *)

  val get_string : T.buf -> off:int -> len:int -> string
  val get_uoffset : T.buf -> int -> int
  val get_voffset : T.buf -> int -> int
  val get_soffset : T.buf -> int -> int

  (** Read from buf *)

  val get_bool : T.buf -> int -> T.bool
  val get_byte : T.buf -> int -> T.byte
  val get_ubyte : T.buf -> int -> T.ubyte
  val get_short : T.buf -> int -> T.short
  val get_ushort : T.buf -> int -> T.ushort
  val get_int : T.buf -> int -> T.int
  val get_uint : T.buf -> int -> T.uint
  val get_long : T.buf -> int -> T.long
  val get_ulong : T.buf -> int -> T.ulong
  val get_float : T.buf -> int -> T.float
  val get_double : T.buf -> int -> T.double

  (** Write to bytes in builder *)

  val set_bool : bytes -> int -> T.bool -> unit
  val set_byte : bytes -> int -> T.byte -> unit
  val set_ubyte : bytes -> int -> T.ubyte -> unit
  val set_short : bytes -> int -> T.short -> unit
  val set_ushort : bytes -> int -> T.ushort -> unit
  val set_int : bytes -> int -> T.int -> unit
  val set_uint : bytes -> int -> T.uint -> unit
  val set_long : bytes -> int -> T.long -> unit
  val set_ulong : bytes -> int -> T.ulong -> unit
  val set_float : bytes -> int -> T.float -> unit
  val set_double : bytes -> int -> T.double -> unit

  (** Output from bytes in builder *)
  val buf_of_bytes : bytes -> off:int -> len:int -> T.buf

  (** Convert from "default"
      used to read generated default values *)

  val of_default_bool : bool -> T.bool
  val of_default_byte : int64 -> T.byte
  val of_default_ubyte : int64 -> T.ubyte
  val of_default_short : int64 -> T.short
  val of_default_ushort : int64 -> T.ushort
  val of_default_int : int64 -> T.int
  val of_default_uint : int64 -> T.uint
  val of_default_long : int64 -> T.long
  val of_default_ulong : int64 -> T.ulong
  val of_default_float : float -> T.float
  val of_default_double : float -> T.double

  (** Convert to "default" representation
      used for matching in generated enum to_string, union *)

  val to_default_bool : T.bool -> bool
  val to_default_byte : T.byte -> int64
  val to_default_ubyte : T.ubyte -> int64
  val to_default_short : T.short -> int64
  val to_default_ushort : T.ushort -> int64
  val to_default_int : T.int -> int64
  val to_default_uint : T.uint -> int64
  val to_default_long : T.long -> int64
  val to_default_ulong : T.ulong -> int64
  val to_default_float : T.float -> float
  val to_default_double : T.double -> float
end
