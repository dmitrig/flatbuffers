include Primitives_intf

let[@inline] as_signed bits i =
  let shift = Sys.int_size - bits in
  (i lsl shift) asr shift
;;

(* non-allocating version of Int32.unsigned_to_int
   TODO: could use int32_unsigned_of_int that checks bounds? *)
let int32_unsigned_to_int =
  match Sys.word_size with
  | 32 ->
    let max_int = Int32.of_int Stdlib.max_int in
    fun n ->
      if compare Int32.zero n <= 0 && compare n max_int <= 0
      then Int32.to_int n
      else failwith "int32_unsigned_to_int overflow"
  | 64 ->
    (* So that it compiles in 32-bit *)
    let mask = (0xFFFF lsl 16) lor 0xFFFF in
    fun n -> Int32.to_int n land mask
  | _ -> assert false
;;

(* common fb mappings when using stdlib types *)
module StdlibT = struct
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

(* common functions when using stdlib types *)
module StdlibCommon = struct
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

module Bigstring = struct
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
  let get_uoffset b i = int32_unsigned_to_int (get_int32 b i)
  let get_voffset = get_uint16
  let get_soffset b i = Int32.to_int (get_int32 b i)

  module T = struct
    type buf = Bigstringaf.t

    include StdlibT
  end

  let get_bool b i = get_char b i == '\001'
  let get_byte b i = as_signed 8 (Char.code (get_char b i))
  let get_ubyte = get_char
  let get_short b i = as_signed 16 (get_uint16 b i)
  let get_ushort = get_uint16
  let get_int = get_int32
  let get_uint = get_int
  let get_long = get_int64
  let get_ulong = get_long
  let get_float b i = Int32.float_of_bits (get_int32 b i)
  let get_double b i = Int64.float_of_bits (get_int64 b i)

  include StdlibCommon
end

module String = struct
  let buf_of_bytes b ~off ~len = Bytes.sub_string b off len
  let get_string b ~off ~len = String.sub b off len
  let get_uoffset b i = int32_unsigned_to_int (String.get_int32_le b i)
  let get_voffset = String.get_int16_le
  let get_soffset b i = Int32.to_int (String.get_int32_le b i)

  module T = struct
    type buf = String.t

    include StdlibT
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

  include StdlibCommon
end

module Bytes = struct
  let buf_of_bytes b ~off ~len = Bytes.sub b off len
  let get_string b ~off ~len = Bytes.sub_string b off len
  let get_uoffset b i = int32_unsigned_to_int (Bytes.get_int32_le b i)
  let get_voffset = Bytes.get_int16_le
  let get_soffset b i = Int32.to_int (Bytes.get_int32_le b i)

  module T = struct
    type buf = Bytes.t

    include StdlibT
  end

  let get_bool b i = Bytes.get b i == '\001'
  let get_byte = Bytes.get_int8
  let get_ubyte = Bytes.get
  let get_short = Bytes.get_int16_le
  let get_ushort = Bytes.get_uint16_le
  let get_int = Bytes.get_int32_le
  let get_uint = Bytes.get_int32_le
  let get_long = Bytes.get_int64_le
  let get_ulong = Bytes.get_int64_le
  let get_float b i = Stdlib.Int32.float_of_bits (Bytes.get_int32_le b i)
  let get_double b i = Stdlib.Int64.float_of_bits (Bytes.get_int64_le b i)

  include StdlibCommon
end
