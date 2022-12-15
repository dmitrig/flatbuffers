include Runtime_intf

module Make (Primitives : Primitives.Intf) = struct
  module T = Primitives.T
  module Builder = Builder.Make (Primitives)

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

  (** Instantiate vector API for the given representation of elements *)
  module MakeVector (E : sig
    type elt
    type builder_elt

    val size : int
    val get : T.buf -> int -> elt
    val set : Builder.t -> int -> builder_elt -> unit
  end) =
  struct
    type t

    (* TODO: also not really a uoffset *)
    let length b i = Primitives.get_uoffset b i
    let unsafe_get b i j = E.get b (i + 4 + (E.size * j))

    let get b i j =
      if j < length b i then unsafe_get b i j else invalid_arg "index out of bounds"
    ;;

    let to_list b i = List.init (length b i) (unsafe_get b i)
    let to_array b i = Array.init (length b i) (unsafe_get b i)

    let to_seq b i =
      let len = length b i in
      let rec aux j () =
        if j < len
        then (
          let x = unsafe_get b i j in
          Seq.Cons (x, aux (j + 1)))
        else Seq.Nil
      in
      aux 0
    ;;

    let iter b f i =
      for j = 0 to length b i - 1 do
        f b (unsafe_get b i j)
      done
    ;;

    let create b a =
      let len = Array.length a in
      Builder.start_vector b ~n_elts:(Array.length a) ~elt_size:E.size;
      for i = 0 to len - 1 do
        E.set b (i * E.size) a.(i)
      done;
      Builder.end_vector b
    ;;
  end

  module MakeScalar (T : sig
    type t
    type default

    val size : int
    val get : T.buf -> int -> t
    val set : Builder.t -> int -> t -> unit
    val to_default : t -> default
    val of_default : default -> t
  end) =
  struct
    type t = T.t

    let read_offset b i off = T.get b (i + off)

    let read_table_default b i n ~default =
      let i' = get_indirect b i n in
      if i' < 0 then default else T.get b i'
    ;;

    let read_table b i n =
      let i' = get_indirect b i n in
      if i' < 0 then invalid_arg "required field not set" else T.get b i'
    ;;

    let read_table_opt b i n =
      let i' = get_indirect b i n in
      if i' < 0 then None else Some (T.get b i')
    ;;

    let size = T.size
    let set = T.set

    let push_slot f x b =
      Builder.prep ~align:T.size ~bytes:T.size b;
      T.set b 0 x;
      Builder.save_slot ~id:f b;
      b
    ;;

    let push_slot_default f ~default x b =
      (* use compare since nan <> nan *)
      if compare x default = 0 then b else push_slot f x b
    ;;

    let to_default = T.to_default
    let of_default = T.of_default

    module Vector = MakeVector (struct
      type elt = T.t
      type builder_elt = T.t

      let size = T.size
      let get = T.get
      let set = T.set
    end)
  end
  [@@inline]

  (** User-facing API  *)

  module Bool = MakeScalar (struct
    type t = T.bool
    type default = bool

    let size = 1
    let get = Primitives.get_bool
    let set = Builder.set_bool
    let to_default = Primitives.to_default_bool
    let of_default = Primitives.of_default_bool
  end)

  module Byte = MakeScalar (struct
    type t = T.byte
    type default = int64

    let size = 1
    let get = Primitives.get_byte
    let set = Builder.set_byte
    let to_default = Primitives.to_default_byte
    let of_default = Primitives.of_default_byte
  end)

  module UByte = MakeScalar (struct
    type t = T.ubyte
    type default = int64

    let size = 1
    let get = Primitives.get_ubyte
    let set = Builder.set_ubyte
    let to_default = Primitives.to_default_ubyte
    let of_default = Primitives.of_default_ubyte
  end)

  module Short = MakeScalar (struct
    type t = T.short
    type default = int64

    let size = 2
    let get = Primitives.get_short
    let set = Builder.set_short
    let to_default = Primitives.to_default_short
    let of_default = Primitives.of_default_short
  end)

  module UShort = MakeScalar (struct
    type t = T.ushort
    type default = int64

    let size = 2
    let get = Primitives.get_ushort
    let set = Builder.set_ushort
    let to_default = Primitives.to_default_ushort
    let of_default = Primitives.of_default_ushort
  end)

  module Int = MakeScalar (struct
    type t = T.int
    type default = int64

    let size = 4
    let get = Primitives.get_int
    let set = Builder.set_int
    let to_default = Primitives.to_default_int
    let of_default = Primitives.of_default_int
  end)

  module UInt = MakeScalar (struct
    type t = T.uint
    type default = int64

    let size = 4
    let get = Primitives.get_uint
    let set = Builder.set_uint
    let to_default = Primitives.to_default_uint
    let of_default = Primitives.of_default_uint
  end)

  (* type of union tags *)
  module UType = UByte

  module Long = MakeScalar (struct
    type t = T.long
    type default = int64

    let size = 8
    let get = Primitives.get_long
    let set = Builder.set_long
    let to_default = Primitives.to_default_long
    let of_default = Primitives.of_default_long
  end)

  module ULong = MakeScalar (struct
    type t = T.ulong
    type default = int64

    let size = 8
    let get = Primitives.get_ulong
    let set = Builder.set_ulong
    let to_default = Primitives.to_default_ulong
    let of_default = Primitives.of_default_ulong
  end)

  module Float = MakeScalar (struct
    type t = T.float
    type default = float

    let size = 4
    let get = Primitives.get_float
    let set = Builder.set_float
    let to_default = Primitives.to_default_float
    let of_default = Primitives.of_default_float
  end)

  module Double = MakeScalar (struct
    type t = T.double
    type default = float

    let size = 8
    let get = Primitives.get_double
    let set = Builder.set_double
    let to_default = Primitives.to_default_double
    let of_default = Primitives.of_default_double
  end)

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

    module Vector (T : sig
      type builder_elt

      val size : int
      val set : Builder.t -> int -> builder_elt -> unit
    end) =
    MakeVector (struct
      type elt = offset
      type builder_elt = T.builder_elt

      let size = T.size
      let get _ i = i
      let set = T.set
    end)
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

    module Vector = MakeVector (struct
      type elt = offset
      type builder_elt = Builder.offset

      let size = size
      let get = get
      let set = Builder.set_uoffset
    end)
  end

  module String = struct
    include MakeVector (struct
      type elt = T.ubyte
      type builder_elt = T.ubyte

      let size = UByte.size
      let get = Primitives.get_ubyte
      let set = Builder.set_ubyte
    end)

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
end
