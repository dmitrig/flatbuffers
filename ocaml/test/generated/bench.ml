(** Automatically generated by the FlatBuffers compiler

    root type: benchmarks_flatbuffers.FooBarContainer (//bench.fbs)
    flatc version: 23.3.3
*)

[@@@warning "-32"]

module Rt = Flatbuffers.Runtime

module Struct = struct

  let rec set_bar__3 b i (parent_, time_, ratio_, size_) =
    set_foo__2 b (i + 0) parent_;
    Rt.Builder.set_scalar TInt b (i + 16) time_;
    Rt.Builder.set_scalar TFloat b (i + 20) ratio_;
    Rt.Builder.set_scalar TUShort b (i + 24) size_;
    Rt.Builder.set_padding b (i + 26) 6;

  and set_foo__2 b i (id_, count_, prefix_, length_) =
    Rt.Builder.set_scalar TULong b (i + 0) id_;
    Rt.Builder.set_scalar TShort b (i + 8) count_;
    Rt.Builder.set_scalar TByte b (i + 10) prefix_;
    Rt.Builder.set_padding b (i + 11) 1;
    Rt.Builder.set_scalar TUInt b (i + 12) length_;
end

module BenchmarksFlatbuffers = struct
  module Enum = struct
    type t = Rt.Short.t

    let apples = Rt.Short.of_default 0L
    let pears = Rt.Short.of_default 1L
    let bananas = Rt.Short.of_default 2L

    let to_string e =
      match Rt.Short.to_default e with
      | 0L -> "apples"
      | 1L -> "pears"
      | 2L -> "bananas"
      | x -> "<benchmarks_flatbuffers.Enum: " ^ (Int64.to_string x) ^ ">"

    module Vector = Rt.Short.Vector
  end

  module FooBarContainer = struct
    type t

    module Vector = Rt.Ref.Vector

    let extension = None
    let identifier = None
    let[@inline] root ?(size_prefixed = false) ?(off = 0) p b = Rt.get_root p b ~size_prefixed ~off
    let finish_buf ?(size_prefixed = false) = Rt.Builder.finish ?identifier ~size_prefixed

    let[@inline] list b o = Rt.Ref.read_table_opt b o 4
    let[@inline] initialized b o = Rt.Bool.(read_table_default b o 6 ~default:(of_default false))
    let[@inline] fruit b o = Rt.Short.(read_table_default b o 8 ~default:(of_default 0L))
    let[@inline] location b o = Rt.Ref.read_table_opt b o 10

    module Builder = struct
      type t = Rt.Builder.t

      let start b = Rt.Builder.start_table b ~n_fields:4
      let finish b = Rt.Builder.end_table b
      let add_list = Rt.Ref.push_slot 0
      let add_initialized = Rt.Bool.(push_slot_default 1 ~default:(of_default false))
      let add_fruit = Rt.Short.(push_slot_default 2 ~default:(of_default 0L))
      let add_location = Rt.Ref.push_slot 3
    end
  end

  module FooBar = struct
    type t

    module Vector = Rt.Ref.Vector

    let[@inline] sibling b o = Rt.Struct.read_table_opt b o 4
    let[@inline] name b o = Rt.Ref.read_table_opt b o 6
    let[@inline] rating b o = Rt.Double.(read_table_default b o 8 ~default:(of_default 0.0))
    let[@inline] postfix b o = Rt.UByte.(read_table_default b o 10 ~default:(of_default 0L))

    module Builder = struct
      type t = Rt.Builder.t

      let start b = Rt.Builder.start_table b ~n_fields:4
      let finish b = Rt.Builder.end_table b
      let add_sibling = Rt.Struct.push_slot Struct.set_bar__3 32 8 0
      let add_name = Rt.Ref.push_slot 1
      let add_rating = Rt.Double.(push_slot_default 2 ~default:(of_default 0.0))
      let add_postfix = Rt.UByte.(push_slot_default 3 ~default:(of_default 0L))
    end
  end

  module Foo = struct
    type t = (Rt.ULong.t * Rt.Short.t * Rt.Byte.t * Rt.UInt.t)

    module Vector = Rt.Struct.Vector (struct type builder_elt = t let size = 16 let set = Struct.set_foo__2 end)

    let[@inline] id b s = Rt.ULong.read_offset b s 0
    let[@inline] count b s = Rt.Short.read_offset b s 8
    let[@inline] prefix b s = Rt.Byte.read_offset b s 10
    let[@inline] length b s = Rt.UInt.read_offset b s 12
  end

  module Bar = struct
    type t = ((Rt.ULong.t * Rt.Short.t * Rt.Byte.t * Rt.UInt.t) * Rt.Int.t * Rt.Float.t * Rt.UShort.t)

    module Vector = Rt.Struct.Vector (struct type builder_elt = t let size = 32 let set = Struct.set_bar__3 end)

    let[@inline] parent b s = Rt.Struct.read_offset b s 0
    let[@inline] time b s = Rt.Int.read_offset b s 16
    let[@inline] ratio b s = Rt.Float.read_offset b s 20
    let[@inline] size b s = Rt.UShort.read_offset b s 24
  end
end (* BenchmarksFlatbuffers *)
