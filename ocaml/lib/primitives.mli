module T : sig
  type bool = Bool.t
  type byte = Int.t
  type ubyte = Char.t
  type short = Int.t
  type ushort = Int.t
  type int = Int32.t
  type uint = Int32.t
  type long = Int64.t
  type ulong = Int64.t
  type float = Float.t
  type double = Float.t
end

type _ t =
  | Bytes : bytes t
  | String : string t
  | Bigstring : Bigstringaf.t t

(** Data read by the runtime, not abstract *)

val get_string : 'b t -> 'b -> off:int -> len:int -> string
val get_uoffset : 'b t -> 'b -> int -> int
val get_voffset : 'b t -> 'b -> int -> int
val get_soffset : 'b t -> 'b -> int -> int

(** Read from buf *)

type _ ty =
  | TBool : T.bool ty
  | TByte : T.byte ty
  | TUByte : T.ubyte ty
  | TShort : T.short ty
  | TUShort : T.ushort ty
  | TInt : T.int ty
  | TUInt : T.uint ty
  | TLong : T.long ty
  | TULong : T.ulong ty
  | TFloat : T.float ty
  | TDouble : T.double ty

val get_scalar : 'a ty ->  'b t -> 'b -> int -> 'a

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
val buf_of_bytes : 'a t -> bytes -> off:int -> len:int -> 'a

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
