module Primitives = struct
  (* TODO: same intrinsics as bigstringaf, significantly better perf without flambda
     Endianness check probably prevents inlining? *)
  (* external get_char : Bigstringaf.t -> int -> char = "%caml_ba_ref_1" *)
  (* external get_uint16 : Bigstringaf.t -> int -> int = "%caml_bigstring_get16" *)
  (* external get_int32 : Bigstringaf.t -> int -> int32 = "%caml_bigstring_get32" *)
  (* external get_int64 : Bigstringaf.t -> int -> int64 = "%caml_bigstring_get64" *)

  let get_char b i = Bigstringaf.get b i
  let get_uint16 b i = Bigstringaf.get_int16_le b i
  let get_int32 b i = Bigstringaf.get_int32_le b i
  let get_int64 b i = Bigstringaf.get_int64_le b i

  let buf_of_bytes b ~off ~len =
    let buf = Bigstringaf.create len in
    Bigstringaf.blit_from_bytes b ~src_off:off buf ~dst_off:0 ~len;
    buf
  ;;

  let get_string b ~off ~len = Bigstringaf.substring b ~off ~len
  let get_uoffset b i = Util.int32_unsigned_to_int (get_int32 b i)
  let get_voffset = get_uint16
  let get_soffset b i = Int32.to_int (get_int32 b i)

  module T = struct
    type buf = Bigstringaf.t
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

  let get_bool b i = get_char b i == '\001'
  let get_byte b i = Util.as_signed 8 (Char.code (get_char b i))
  let get_ubyte = get_char
  let get_short b i = Util.as_signed 16 (get_uint16 b i)
  let get_ushort = get_uint16
  let get_int = get_int32
  let get_uint = get_int
  let get_long = get_int64
  let get_ulong = get_long
  let get_float b i = Int32.float_of_bits (get_int32 b i)
  let get_double b i = Int64.float_of_bits (get_int64 b i)
  let set_bool b i x = Bytes.set_int8 b i (if x then 1 else 0)
  let set_byte b i x = Bytes.set_int8 b i x
  let set_ubyte b i x = Bytes.set_int8 b i (Char.code x)
  let set_short b i x = Bytes.set_int16_le b i x
  let set_ushort b i x = Bytes.set_int16_le b i x
  let set_int b i x = Bytes.set_int32_le b i x
  let set_uint b i x = set_int b i x
  let set_long b i x = Bytes.set_int64_le b i x
  let set_ulong b i x = set_long b i x
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
  (* include StdlibCommon *)
end
