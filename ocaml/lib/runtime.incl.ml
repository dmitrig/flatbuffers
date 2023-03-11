module T = Primitives.T

type 'b buf = T.buf
type offset = int
type ('b, 't) fb = offset
type ('b, 't) fbopt = offset
type 'a wip = Builder.offset
type 't root = Root : 'b buf * ('b, 't) fb -> 't root

module type VectorS = sig
  type t
  type 'b elt
  type builder_elt

  val length : 'b buf -> ('b, t) fb -> int
  val get : 'b buf -> ('b, t) fb -> int -> 'b elt
  val iter : 'b buf -> ('b buf -> 'b elt -> unit) -> ('b, t) fb -> unit
  val to_list : 'b buf -> ('b, t) fb -> 'b elt list
  val to_array : 'b buf -> ('b, t) fb -> 'b elt array
  val to_seq : 'b buf -> ('b, t) fb -> 'b elt Seq.t
  val create : Builder.t -> builder_elt array -> t wip
end

(* get offset of a table via vtable *)
let get_indirect b i voff =
  let vi = i - Primitives.get_soffset b i in
  (* TODO: just a uint16, not an offset *)
  let vsz = Primitives.get_voffset b vi in
  if voff >= vsz
  then -1
  else (
    let foff = Primitives.get_voffset b (vi + voff) in
    if foff == 0 then -1 else i + foff)
;;

#define VECTOR(get_,set_,size_) \
struct \
  type t \
  (* TODO: also not really a uoffset *) \
  let length b i = Primitives.get_uoffset b i \
  let unsafe_get b i j = get_ b (i + 4 + (size_ * j)) \
  let get b i j = \
    if j < length b i then unsafe_get b i j else invalid_arg "index out of bounds" \
  ;; \
  let to_list b i = List.init (length b i) (unsafe_get b i) \
  let to_array b i = Array.init (length b i) (unsafe_get b i) \
  let to_seq b i = \
    let len = length b i in \
    let rec aux j () = \
      if j < len \
      then ( \
        let x = unsafe_get b i j in \
        Seq.Cons (x, aux (j + 1))) \
      else Seq.Nil \
    in \
    aux 0 \
  ;; \
  let iter b f i = \
    for j = 0 to length b i - 1 do \
      f b (unsafe_get b i j) \
    done \
  ;; \
  let create b a = \
    let len = Array.length a in \
    Builder.start_vector b ~n_elts:(Array.length a) ~elt_size:size_; \
    for i = 0 to len - 1 do \
      set_ b (i * size_) a.(i) \
    done; \
    Builder.end_vector b \
  ;; \
end

(** User-facing API  *)

#define SCALAR(name_,ty_,default_,size_) \
module name_ = struct \
  type t = T.ty_ \
  type default = default_ \
  let get b i = Primitives.(CONCAT(get_, ty_) b i) \
  let read_offset b i off = get b (i + off) \
  let read_table_default b i n ~default = \
    let i' = get_indirect b i n in \
    if i' < 0 then default else get b i' \
  ;; \
  let read_table b i n = \
    let i' = get_indirect b i n in \
    if i' < 0 then invalid_arg "required field not set" else get b i' \
  ;; \
  let read_table_opt b i n = \
    let i' = get_indirect b i n in \
    if i' < 0 then None else Some (get b i') \
  ;; \
  let size = size_ \
  let set b i x = Builder.(CONCAT(set_, ty_)) b i x \
  let push_slot f x b = \
    Builder.prep ~align:size ~bytes:size b; \
    set b 0 x; \
    Builder.save_slot ~id:f b; \
    b \
  ;; \
  let push_slot_default f ~default x b = \
    (* use compare since nan <> nan *) \
    if compare x default = 0 then b else push_slot f x b \
  ;; \
  let to_default = Primitives.(CONCAT(to_default_, ty_)) \
  let of_default = Primitives.(CONCAT(of_default_, ty_)) \
  module Vector = VECTOR(get,set,size)\
end

SCALAR(Bool, bool, bool, 1)
SCALAR(Byte, byte, int64, 1)
SCALAR(UByte, ubyte, int64, 1)
SCALAR(Short, short, int64, 2)
SCALAR(UShort, ushort, int64, 2)
SCALAR(Int, int, int64, 4)
SCALAR(UInt, uint, int64, 4)
SCALAR(Long, long, int64, 8)
SCALAR(ULong, ulong, int64, 8)
SCALAR(Float, float, float, 4)
SCALAR(Double, double, float, 8)


module UType = UByte

module Struct = struct
  let read_offset _ i off = i + off

  let read_table b i n =
    let i' = get_indirect b i n in
    if i' < 0 then invalid_arg "required field not set" else i'
  ;;

  let read_table_opt b i n = get_indirect b i n

  (* uses generated set function *)
  let push_slot set size align f s b =
    Builder.prep ~align ~bytes:size b;
    set b 0 s;
    Builder.save_slot ~id:f b;
    b
  ;;

  let get _ i = i

  module Vector (T : sig
    type builder_elt

    val size : int
    val set : Builder.t -> int -> builder_elt -> unit
  end) = VECTOR(get,T.set,T.size)
end

module Ref = struct
  (* tables, vectors, strings require extra indirection *)
  let get b i = i + Primitives.get_uoffset b i

  let read_table b i n =
    let i' = get_indirect b i n in
    if i' < 0 then invalid_arg "required field not set" else get b i'
  ;;

  let read_table_opt b i n =
    let i' = get_indirect b i n in
    if i' < 0 then i' else get b i'
  ;;

  let size = 4

  let push_slot f x b =
    Builder.prep ~align:size ~bytes:size b;
    Builder.set_uoffset b 0 x;
    Builder.save_slot ~id:f b;
    b
  ;;

  let push_union ft fo t o b =
    Builder.prep ~align:UByte.size ~bytes:UByte.size b;
    Builder.set_ubyte b 0 t;
    Builder.save_slot ~id:ft b;
    Builder.prep ~align:size ~bytes:size b;
    Builder.set_uoffset b 0 o;
    Builder.save_slot ~id:fo b;
    b
  ;;

  module Vector = VECTOR(get,Builder.set_uoffset,size)
end

module String = struct
  include VECTOR(Primitives.get_ubyte,Builder.set_ubyte,UByte.size)

  let to_string b i =
    let len = length b i in
    Primitives.get_string b ~off:(i + 4) ~len
  ;;

  let create b s =
    (* ensure null terminator; there may be more padding inserted *)
    Builder.prep b ~align:1 ~bytes:UByte.size;
    Builder.set_padding b 0 1;
    (* string is a regular ubyte vectory otherwise *)
    Builder.start_vector b ~n_elts:(String.length s) ~elt_size:UByte.size;
    Builder.set_string b 0 s;
    Builder.end_vector b
  ;;

  let create_shared b s =
    match Builder.find_shared_string b s with
    | Some o -> o
    | None ->
      let o = create b s in
      Builder.add_shared_string b s o;
      o
  ;;

  module Vector = Ref.Vector
end

let get_root ?(off = 0) ?(size_prefixed = false) b =
  let start = off + if size_prefixed then UInt.size else 0 in
  Root (b, Ref.get b start)
;;

let get_identifier ?(off = 0) ?(size_prefixed = false) b =
  let ident_start = off + Ref.size + if size_prefixed then UInt.size else 0 in
  Primitives.get_string b ~off:ident_start ~len:4
;;

module Option = struct
  type ('b, 't) t = ('b, 't) fbopt

  let is_none o = o < 0
  let is_some o = not (is_none o)
  let value o ~default = if o < 0 then default else o
  let get o = if o < 0 then invalid_arg "fbopt is None" else o
  let fold ~none ~some o = if o < 0 then none else some o
  let iter f o = if o < 0 then () else f o
  let to_option o = if o < 0 then None else Some o
end

#undef SCALAR
#undef VECTOR
