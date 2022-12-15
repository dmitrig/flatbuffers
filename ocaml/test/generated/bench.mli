(** Automatically generated by the FlatBuffers compiler

    root type: benchmarks_flatbuffers.FooBarContainer (//bench.fbs)
    flatc version: 23.3.3
*)

module Make (R : Flatbuffers.Runtime.Intf_impl) : sig
module Rt : Flatbuffers.Runtime.Intf with module T := R.T

module rec BenchmarksFlatbuffers : sig
  (* Enum benchmarks_flatbuffers.Enum (//bench.fbs) *)
  module rec Enum : sig
    type t = private Rt.Short.t

    val apples : t
    val pears : t
    val bananas : t
    val to_string : t -> string

    module Vector : Rt.VectorS with type 'b elt := t and type builder_elt := t
  end

  (* Table benchmarks_flatbuffers.FooBarContainer (//bench.fbs) *)
  and FooBarContainer : sig
    type t

    module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t Rt.wip

    val extension : string option
    val identifier : string option
    val root : ?size_prefixed:bool -> ?off:int -> R.T.buf -> t Rt.root
    val finish_buf : ?size_prefixed:bool -> Rt.Builder.t -> t Rt.wip -> R.T.buf

    val list : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, FooBar.Vector.t) Rt.fbopt
    val initialized : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Bool.t
    val fruit : 'b Rt.buf -> ('b, t) Rt.fb -> Enum.t
    val location : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.String.t) Rt.fbopt

    module Builder : sig
      type t

      val start : Rt.Builder.t -> t
      val finish : t -> FooBarContainer.t Rt.wip
      val add_list : FooBar.Vector.t Rt.wip -> t -> t
      val add_initialized : Rt.Bool.t -> t -> t
      val add_fruit : Enum.t -> t -> t
      val add_location : Rt.String.t Rt.wip -> t -> t
    end
  end

  (* Table benchmarks_flatbuffers.FooBar (//bench.fbs) *)
  and FooBar : sig
    type t

    module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t Rt.wip

    val sibling : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Bar.t) Rt.fbopt
    val name : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.String.t) Rt.fbopt
    val rating : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Double.t
    val postfix : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.UByte.t

    module Builder : sig
      type t

      val start : Rt.Builder.t -> t
      val finish : t -> FooBar.t Rt.wip
      val add_sibling : Bar.t -> t -> t
      val add_name : Rt.String.t Rt.wip -> t -> t
      val add_rating : Rt.Double.t -> t -> t
      val add_postfix : Rt.UByte.t -> t -> t
    end
  end

  (* Struct benchmarks_flatbuffers.Foo (//bench.fbs) *)
  and Foo : sig
    type t = (Rt.ULong.t * Rt.Short.t * Rt.Byte.t * Rt.UInt.t)

    module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t

    val id : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.ULong.t
    val count : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Short.t
    val prefix : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Byte.t
    val length : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.UInt.t
  end

  (* Struct benchmarks_flatbuffers.Bar (//bench.fbs) *)
  and Bar : sig
    type t = (Foo.t * Rt.Int.t * Rt.Float.t * Rt.UShort.t)

    module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t

    val parent : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Foo.t) Rt.fb
    val time : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Int.t
    val ratio : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Float.t
    val size : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.UShort.t
  end
end (* BenchmarksFlatbuffers *)
end
