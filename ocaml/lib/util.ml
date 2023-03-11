let as_signed bits i =
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
(* module StdlibT = struct *)
(*   type bool = Bool.t *)
(*   type byte = Int.t *)
(*   type ubyte = Char.t *)
(*   type short = Int.t *)
(*   type ushort = Int.t *)
(*   type int = Int32.t *)
(*   type uint = Int32.t *)
(*   type long = Int64.t *)
(*   type ulong = Int64.t *)
(*   type float = Float.t *)
(*   type double = Float.t *)
(* end *)
