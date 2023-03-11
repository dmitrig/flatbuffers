module Primitives = struct
  let buf_of_bytes b ~off ~len = Bytes.sub_string b off len
  let get_string b ~off ~len = String.sub b off len
  let get_uoffset b i = Util.int32_unsigned_to_int (String.get_int32_le b i)
  let get_voffset = String.get_int16_le
  let get_soffset b i = Int32.to_int (String.get_int32_le b i)

  module T = struct
    type buf = String.t
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

  let get_bool b i = String.get b i == '\001'
  let get_byte = String.get_int8
  let get_ubyte = String.get
  let get_short = String.get_int16_le
  let get_ushort = String.get_uint16_le
  let get_int = String.get_int32_le
  let get_uint = String.get_int32_le
  let get_long = String.get_int64_le
  let get_ulong = String.get_int64_le
  let get_float b i = Stdlib.Int32.float_of_bits (String.get_int32_le b i)
  let get_double b i = Stdlib.Int64.float_of_bits (String.get_int64_le b i)

  (* common *)

  let set_bool b i x = Bytes.set_int8 b i (if x then 1 else 0)
  let set_byte = Bytes.set_int8
  let set_ubyte b i x = Bytes.set_int8 b i (Char.code x)
  let set_short = Bytes.set_int16_le
  let set_ushort = Bytes.set_int16_le
  let set_int = Bytes.set_int32_le
  let set_uint = set_int
  let set_long = Bytes.set_int64_le
  let set_ulong = set_long
  let set_float b i x = Bytes.set_int32_le b i (Int32.bits_of_float x)
  let set_double b i x = Bytes.set_int64_le b i (Int64.bits_of_float x)
  let of_default_bool = Fun.id
  let of_default_byte = Int64.to_int
  let of_default_ubyte x = Char.chr (Int64.to_int x)
  let of_default_short = Int64.to_int
  let of_default_ushort = Int64.to_int
  let of_default_int = Int64.to_int32
  let of_default_uint = Int64.to_int32
  let of_default_long = Fun.id
  let of_default_ulong = Fun.id
  let of_default_float = Fun.id
  let of_default_double = Fun.id
  let to_default_bool = Fun.id
  let to_default_byte = Int64.of_int
  let to_default_ubyte x = Int64.of_int (Char.code x)
  let to_default_short = Int64.of_int
  let to_default_ushort = Int64.of_int
  let to_default_int = Int64.of_int32
  let to_default_uint = Int64.of_int32
  let to_default_long = Fun.id
  let to_default_ulong = Fun.id
  let to_default_float = Fun.id
  let to_default_double = Fun.id
end
