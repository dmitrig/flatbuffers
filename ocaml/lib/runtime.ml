module type Intf = Runtime_intf.Intf

module Builder = Builder

#define VT_VEC_SIGS(name_,ty_) \
    CONCAT(get_vec_,name_) : 'b -> Read.offset -> int -> ty_ \
  ; CONCAT(to_list_vec_,name_) : 'b -> Read.offset -> ty_ list \
  ; CONCAT(to_array_vec_,name_) : 'b -> Read.offset -> ty_ array \
  ; CONCAT(to_seq_vec_,name_) : 'b -> Read.offset -> ty_ Seq.t \
  ; CONCAT(iter_vec_,name_) : 'b -> (ty_ -> unit) -> Read.offset -> unit

#define VT_SCALAR_SIGS(name_,ty_) \
    CONCAT(read_offset_,name_) : 'b -> Read.offset -> int -> ty_ \
  ; CONCAT(read_table_,name_) : 'b -> Read.offset -> int -> ty_ \
  ; CONCAT(read_table_opt_,name_) : 'b -> Read.offset -> int -> ty_ option \
  ; CONCAT(read_table_default_,name_) : 'b -> Read.offset -> int -> default:ty_ -> ty_ \
  ; VT_VEC_SIGS(name_,ty_)

type 'b vt =
  { length_vec : 'b -> Read.offset -> int
  ; get_string : 'b -> Read.offset -> string
  (* scalar *)
  ; VT_SCALAR_SIGS(bool, Primitives.T.bool)
  ; VT_SCALAR_SIGS(byte, Primitives.T.byte)
  ; VT_SCALAR_SIGS(ubyte, Primitives.T.ubyte)
  ; VT_SCALAR_SIGS(short, Primitives.T.short)
  ; VT_SCALAR_SIGS(ushort, Primitives.T.ushort)
  ; VT_SCALAR_SIGS(int, Primitives.T.int)
  ; VT_SCALAR_SIGS(uint, Primitives.T.uint)
  ; VT_SCALAR_SIGS(long, Primitives.T.long)
  ; VT_SCALAR_SIGS(ulong, Primitives.T.ulong)
  ; VT_SCALAR_SIGS(float, Primitives.T.float)
  ; VT_SCALAR_SIGS(double, Primitives.T.double)
  (* ref *)
  ; read_table_ref : 'b -> Read.offset -> int -> Read.offset
  ; read_table_opt_ref : 'b -> Read.offset -> int -> Read.offset
  ; VT_VEC_SIGS(ref, Read.offset)
  (* struct *)
  ; read_table_struct : 'b -> Read.offset -> int -> Read.offset
  ; read_table_opt_struct : 'b -> Read.offset -> int -> Read.offset
  (* generic ops *)
  ; get_vec : 'a. 'a Read.tag -> 'b -> int -> int -> 'a
  ; to_list_vec : 'a. 'a Read.tag -> 'b -> int -> 'a list
  ; to_array_vec : 'a. 'a Read.tag -> 'b -> int -> 'a array
  ; to_seq_vec : 'a. 'a Read.tag -> 'b -> int -> 'a Seq.t
  ; iter_vec : 'a. 'a Read.tag -> 'b -> ('a -> unit) -> int -> unit
  }

type 'b buf = Buf : 'a vt * 'a -> 'b buf
type ('b, 't) fb = Read.offset
type ('b, 't) fbopt = Read.offset
type 'a wip = Builder.offset
type 't root = Root : 'b buf * ('b, 't) fb -> 't root

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

#define VT_VEC_FNS(name_,tag_,prim_) \
    CONCAT(get_vec_,name_) = (fun b i j -> Read.get_vec tag_ prim_ b i j)[@inline] \
  ; CONCAT(to_list_vec_,name_) = (fun b i -> Read.to_list_vec tag_ prim_ b i)[@inline] \
  ; CONCAT(to_array_vec_,name_) = (fun b i -> Read.to_array_vec tag_ prim_ b i)[@inline] \
  ; CONCAT(to_seq_vec_,name_) = (fun b i -> Read.to_seq_vec  tag_ prim_ b i)[@inline] \
  ; CONCAT(iter_vec_,name_) = (fun b f i -> Read.iter_vec tag_ prim_ b f i)[@inline]

#define VT_SCALAR_FNS(name_,tag_,prim_) \
    CONCAT(read_offset_,name_) = (fun b i off -> Read.get_val (TScalar tag_) prim_ b (i + off))[@inline] \
  ; CONCAT(read_table_,name_) = (fun b i n -> Read.read_table (TScalar tag_) prim_ b i n)[@inline] \
  ; CONCAT(read_table_opt_,name_) = (fun b i n -> Read.read_table_opt tag_ prim_ b i n)[@inline] \
  ; CONCAT(read_table_default_,name_) = (fun b i n ~default -> Read.read_table_default tag_ prim_ b i n ~default)[@inline] \
  ; VT_VEC_FNS(name_,(TScalar tag_),prim_)

#define VT(prim_) \
  { length_vec = (fun b i -> Read.length_vec prim_ b i) \
  ; get_string = (fun b i -> Read.get_string prim_ b i) \
  ; VT_SCALAR_FNS(bool, Primitives.TBool, prim_) \
  ; VT_SCALAR_FNS(byte, Primitives.TByte, prim_) \
  ; VT_SCALAR_FNS(ubyte, Primitives.TUByte, prim_) \
  ; VT_SCALAR_FNS(short, Primitives.TShort, prim_) \
  ; VT_SCALAR_FNS(ushort, Primitives.TUShort, prim_) \
  ; VT_SCALAR_FNS(int, Primitives.TInt, prim_) \
  ; VT_SCALAR_FNS(uint, Primitives.TUInt, prim_) \
  ; VT_SCALAR_FNS(long, Primitives.TLong, prim_) \
  ; VT_SCALAR_FNS(ulong, Primitives.TULong, prim_) \
  ; VT_SCALAR_FNS(float, Primitives.TFloat, prim_) \
  ; VT_SCALAR_FNS(double, Primitives.TDouble, prim_) \
  (* ref *) \
  ; read_table_ref = (fun b i n -> Read.read_table TRef prim_ b i n) \
  ; read_table_opt_ref = (fun b i n -> Read.read_table_opt_ref prim_ b i n) \
  ; VT_VEC_FNS(ref, TRef, prim_) \
  (* struct *) \
  ; read_table_struct = (fun b i n -> Read.read_table_struct prim_ b i n) \
  ; read_table_opt_struct = (fun b i n -> Read.read_table_opt_struct prim_ b i n) \
  (* generic ops *) \
  ; get_vec = (fun t b i j -> Read.get_vec t prim_ b i j)[@inline] \
  ; to_list_vec = (fun t b i -> Read.to_list_vec t prim_ b i)[@inline] \
  ; to_array_vec = (fun t b i -> Read.to_array_vec t prim_ b i)[@inline] \
  ; to_seq_vec = (fun t b i -> Read.to_seq_vec t prim_ b i)[@inline] \
  ; iter_vec = (fun t b f i -> Read.iter_vec t prim_ b f i)[@inline] \
  }

let vt_bytes = VT(Primitives.Bytes)
let vt_string = VT(Primitives.String)
let vt_bigstring = VT(Primitives.Bigstring)

let[@inline] vt_of (type a) (p : a Primitives.t) : a vt =
  match p with
  | Primitives.Bytes -> vt_bytes
  | Primitives.String -> vt_string
  | Primitives.Bigstring -> vt_bigstring

(** User-facing API  *)

#define SCALAR(name_,ty_,default_,size_) \
module name_ = struct \
  type t = Primitives.T.ty_ \
  let[@inline] read_offset (Buf (vt, b)) i off = vt.CONCAT(read_offset_, ty_) b i off \
  let[@inline] read_table_default (Buf (vt, b)) i n ~default = vt.CONCAT(read_table_default_, ty_) b i n ~default \
  let[@inline] read_table (Buf (vt, b)) i n = vt.CONCAT(read_table_, ty_) b i n \
  let[@inline] read_table_opt (Buf (vt, b)) i n = vt.CONCAT(read_table_opt_, ty_) b i n \
  let[@inline] push_slot f x b = Builder.push_slot_scalar CONCAT(T,name_) f x b \
  let[@inline] push_slot_default f ~default x b = Builder.push_slot_scalar_default CONCAT(T, name_) f ~default x b \
  let[@inline] to_default x = Primitives.CONCAT(to_default_, ty_) x \
  let[@inline] of_default x = Primitives.CONCAT(of_default_, ty_) x \
  module Vector = struct \
    type t \
    let[@inline] length (Buf (vt, b)) i = vt.length_vec b i \
    let[@inline] get (Buf (vt, b)) i j = vt.CONCAT(get_vec_,ty_) b i j \
    let[@inline] to_list (Buf (vt, b)) i = vt.CONCAT(to_list_vec_,ty_) b i \
    let[@inline] to_array (Buf (vt, b)) i = vt.CONCAT(to_array_vec_,ty_) b i \
    let[@inline] to_seq (Buf (vt, b)) i = vt.CONCAT(to_seq_vec_,ty_) b i \
    let[@inline] iter (Buf (vt, b)) f i = vt.CONCAT(iter_vec_,ty_) b f i \
    let[@inline] create b a = Builder.create_vector CONCAT(T,name_) b a \
  end \
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
  let[@inline] read_offset _ i off = i + off
  let[@inline] read_table (Buf (vt, b)) i n = vt.read_table_struct b i n
  let[@inline] read_table_opt (Buf (vt, b)) i n = vt.read_table_opt_struct b i n
  let push_slot = Builder.push_slot_struct

  module Vector (T : sig
    type builder_elt

    val size : int
    val set : Builder.t -> int -> builder_elt -> unit
  end) =
  struct
    (* TODO: this doesn't quite work:
       Want a specialized version of the vector ops that just takes an element
       size parameter (and folds branches for primitives, refs). Baseline
       compiler can't do that when TStruct is a block.
       No way to do it with codegen either, since buffer-specialized functions
       are indirected through the vtable.
     *)
    type t
    let tag = Read.TStruct { sz = T.size; align = 0 }
    let[@inline] length (Buf (vt, b)) i = vt.length_vec b i
    let[@inline] get (Buf (vt, b)) i j = vt.get_vec tag b i j
    let[@inline] to_list (Buf (vt, b)) i = vt.to_list_vec tag b i
    let[@inline] to_array (Buf (vt, b)) i = vt.to_array_vec tag b i
    let[@inline] to_seq (Buf (vt, b)) i = vt.to_seq_vec tag b i
    let[@inline] iter (Buf (vt, b)) f i = vt.iter_vec tag b f i
    let[@inline] create b a = Builder.create_vector_struct T.set ~size:T.size b a
  end

end

module Ref = struct
  let[@inline] read_table (Buf (vt, b)) i n = vt.read_table_ref b i n
  let[@inline] read_table_opt (Buf (vt, b)) i n = vt.read_table_opt_ref b i n
  let[@inline] push_slot f x b = Builder.push_slot_ref f x b
  let[@inline] push_union ft fo t o b = Builder.push_slot_union ft fo t o b

  module Vector = struct
    type t
    let[@inline] length (Buf (vt, b)) i = vt.length_vec b i
    let[@inline] get (Buf (vt, b)) i j = vt.get_vec_ref b i j
    let[@inline] to_list (Buf (vt, b)) i = vt.to_list_vec_ref b i
    let[@inline] to_array (Buf (vt, b)) i = vt.to_array_vec_ref b i
    let[@inline] to_seq (Buf (vt, b)) i = vt.to_seq_vec_ref b i
    let[@inline] iter (Buf (vt, b)) f i = vt.iter_vec_ref b f i
    let[@inline] create b a = Builder.create_vector_ref b a
  end
end

module String = struct
  include UByte.Vector

  let[@inline] to_string (Buf (vt, b)) i = vt.get_string b i
  let[@inline] create b s = Builder.create_string b s
  let[@inline] create_shared b s = Builder.create_shared_string b s

  module Vector = Ref.Vector
end

let[@inline] get_root ?(off = 0) ?(size_prefixed = false) (t: 'b Primitives.t) (b: 'b) =
  let start = off + if size_prefixed then 4 else 0 in
  let buf = Buf (vt_of t, b) in
  Root (buf, Read.get_val TRef t b start)
;;

let get_identifier ?(off = 0) ?(size_prefixed = false) (t: 'b Primitives.t) (b: 'b) =
  let ident_start = off + 4 + if size_prefixed then 4 else 0 in
  Primitives.get_string t b ~off:ident_start ~len:4
;;

module Option = struct
  type ('b, 't) t = ('b, 't) fbopt

  let[@inline] is_none o = o < 0
  let[@inline] is_some o = not (is_none o)
  let[@inline] value o ~default = if o < 0 then default else o
  let[@inline] get o = if o < 0 then invalid_arg "fbopt is None" else o
  let[@inline] fold ~none ~some o = if o < 0 then none else some o
  let[@inline] iter f o = if o < 0 then () else f o
  let[@inline] to_option o = if o < 0 then None else Some o
end

#undef VT_SCALAR_SIGS
#undef VT_SCALAR_FNS
#undef VT
#undef SCALAR
