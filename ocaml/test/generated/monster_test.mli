(** Automatically generated by the FlatBuffers compiler

    root type: MyGame.Example.Monster (//monster_test.fbs)
    flatc version: 23.3.3
*)

module Make (R : Flatbuffers.Runtime.Intf_impl) : sig
module Rt : Flatbuffers.Runtime.Intf with module T := R.T

(* Table TableA (//include_test/include_test1.fbs) *)
module rec TableA : sig
  type t

  module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t Rt.wip

  val b : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, MyGame.OtherNameSpace.TableB.t) Rt.fbopt

  module Builder : sig
    type t

    val start : Rt.Builder.t -> t
    val finish : t -> TableA.t Rt.wip
    val add_b : MyGame.OtherNameSpace.TableB.t Rt.wip -> t -> t
  end
end

and MyGame : sig
  module rec OtherNameSpace : sig
    (* Enum MyGame.OtherNameSpace.FromInclude (//include_test/sub/include_test2.fbs) *)
    module rec FromInclude : sig
      type t = private Rt.Long.t

      val include_val : t
      val to_string : t -> string

      module Vector : Rt.VectorS with type 'b elt := t and type builder_elt := t
    end

    (* Struct MyGame.OtherNameSpace.Unused (//include_test/sub/include_test2.fbs) *)
    and Unused : sig
      type t = (Rt.Int.t)

      module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t

      val a : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Int.t
    end

    (* Table MyGame.OtherNameSpace.TableB (//include_test/sub/include_test2.fbs) *)
    and TableB : sig
      type t

      module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t Rt.wip

      val a : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, TableA.t) Rt.fbopt

      module Builder : sig
        type t

        val start : Rt.Builder.t -> t
        val finish : t -> TableB.t Rt.wip
        val add_a : TableA.t Rt.wip -> t -> t
      end
    end
  end (* OtherNameSpace *)

  (* Table MyGame.InParentNamespace (//monster_test.fbs) *)
  and InParentNamespace : sig
    type t

    module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t Rt.wip


    module Builder : sig
      type t

      val start : Rt.Builder.t -> t
      val finish : t -> InParentNamespace.t Rt.wip
    end
  end

  and Example2 : sig
    (* Table MyGame.Example2.Monster (//monster_test.fbs) *)
    module rec Monster : sig
      type t

      module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t Rt.wip


      module Builder : sig
        type t

        val start : Rt.Builder.t -> t
        val finish : t -> Monster.t Rt.wip
      end
    end
  end (* Example2 *)

  and Example : sig
    (* Enum MyGame.Example.Race (//monster_test.fbs) *)
    module rec Race : sig
      type t = private Rt.Byte.t

      val none : t
      val human : t
      val dwarf : t
      val elf : t
      val to_string : t -> string

      module Vector : Rt.VectorS with type 'b elt := t and type builder_elt := t
    end

    (* Enum MyGame.Example.LongEnum (//monster_test.fbs) *)
    and LongEnum : sig
      type t = private Rt.ULong.t

      val long_one : t
      val long_two : t
      val long_big : t
      val to_string : t -> string

      module Vector : Rt.VectorS with type 'b elt := t and type builder_elt := t
    end

    (** Composite components of Monster color.

        Enum MyGame.Example.Color (//monster_test.fbs) *)
    and Color : sig
      type t = private Rt.UByte.t

      val red : t

      (** \brief color Green Green is bit_flag with value (1u << 1) *)
      val green : t

      (** \brief color Blue (1u << 3) *)
      val blue : t
      val to_string : t -> string

      module Vector : Rt.VectorS with type 'b elt := t and type builder_elt := t
    end

    (* Union MyGame.Example.AnyUniqueAliases (//monster_test.fbs) *)
    and AnyUniqueAliases : sig
      type t = private Rt.UType.t

      val none : t
      val m : t
      val ts : t
      val m2 : t
      val to_string : t -> string
    end

    (* Union MyGame.Example.AnyAmbiguousAliases (//monster_test.fbs) *)
    and AnyAmbiguousAliases : sig
      type t = private Rt.UType.t

      val none : t
      val m1 : t
      val m2 : t
      val m3 : t
      val to_string : t -> string
    end

    (* Union MyGame.Example.Any (//monster_test.fbs) *)
    and Any : sig
      type t = private Rt.UType.t

      val none : t
      val monster : t
      val test_simple_table_with_enum : t
      val my_game_example2_monster : t
      val to_string : t -> string
    end

    (* Struct MyGame.Example.Vec3 (//monster_test.fbs) *)
    and Vec3 : sig
      type t = (Rt.Float.t * Rt.Float.t * Rt.Float.t * Rt.Double.t * Color.t * Test.t)

      module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t

      val x : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Float.t
      val y : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Float.t
      val z : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Float.t
      val test1 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Double.t
      val test2 : 'b Rt.buf -> ('b, t) Rt.fb -> Color.t
      val test3 : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Test.t) Rt.fb
    end

    (* Table MyGame.Example.TypeAliases (//monster_test.fbs) *)
    and TypeAliases : sig
      type t

      module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t Rt.wip

      val i8 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Byte.t
      val u8 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.UByte.t
      val i16 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Short.t
      val u16 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.UShort.t
      val i32 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Int.t
      val u32 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.UInt.t
      val i64 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Long.t
      val u64 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.ULong.t
      val f32 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Float.t
      val f64 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Double.t
      val v8 : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.Byte.Vector.t) Rt.fbopt
      val vf64 : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.Double.Vector.t) Rt.fbopt

      module Builder : sig
        type t

        val start : Rt.Builder.t -> t
        val finish : t -> TypeAliases.t Rt.wip
        val add_i8 : Rt.Byte.t -> t -> t
        val add_u8 : Rt.UByte.t -> t -> t
        val add_i16 : Rt.Short.t -> t -> t
        val add_u16 : Rt.UShort.t -> t -> t
        val add_i32 : Rt.Int.t -> t -> t
        val add_u32 : Rt.UInt.t -> t -> t
        val add_i64 : Rt.Long.t -> t -> t
        val add_u64 : Rt.ULong.t -> t -> t
        val add_f32 : Rt.Float.t -> t -> t
        val add_f64 : Rt.Double.t -> t -> t
        val add_v8 : Rt.Byte.Vector.t Rt.wip -> t -> t
        val add_vf64 : Rt.Double.Vector.t Rt.wip -> t -> t
      end
    end

    (* Table MyGame.Example.TestSimpleTableWithEnum (//monster_test.fbs) *)
    and TestSimpleTableWithEnum : sig
      type t

      module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t Rt.wip

      val color : 'b Rt.buf -> ('b, t) Rt.fb -> Color.t

      module Builder : sig
        type t

        val start : Rt.Builder.t -> t
        val finish : t -> TestSimpleTableWithEnum.t Rt.wip
        val add_color : Color.t -> t -> t
      end
    end

    (* Struct MyGame.Example.Test (//monster_test.fbs) *)
    and Test : sig
      type t = (Rt.Short.t * Rt.Byte.t)

      module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t

      val a : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Short.t
      val b : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Byte.t
    end

    (* Struct MyGame.Example.StructOfStructsOfStructs (//monster_test.fbs) *)
    and StructOfStructsOfStructs : sig
      type t = (StructOfStructs.t)

      module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t

      val a : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, StructOfStructs.t) Rt.fb
    end

    (* Struct MyGame.Example.StructOfStructs (//monster_test.fbs) *)
    and StructOfStructs : sig
      type t = (Ability.t * Test.t * Ability.t)

      module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t

      val a : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Ability.t) Rt.fb
      val b : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Test.t) Rt.fb
      val c : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Ability.t) Rt.fb
    end

    (* Table MyGame.Example.Stat (//monster_test.fbs) *)
    and Stat : sig
      type t

      module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t Rt.wip

      val id : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.String.t) Rt.fbopt
      val val_ : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Long.t
      val count : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.UShort.t

      module Builder : sig
        type t

        val start : Rt.Builder.t -> t
        val finish : t -> Stat.t Rt.wip
        val add_id : Rt.String.t Rt.wip -> t -> t
        val add_val_ : Rt.Long.t -> t -> t
        val add_count : Rt.UShort.t -> t -> t
      end
    end

    (* Table MyGame.Example.Referrable (//monster_test.fbs) *)
    and Referrable : sig
      type t

      module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t Rt.wip

      val id : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.ULong.t

      module Builder : sig
        type t

        val start : Rt.Builder.t -> t
        val finish : t -> Referrable.t Rt.wip
        val add_id : Rt.ULong.t -> t -> t
      end
    end

    (** an example documentation comment: "monster object"

        Table MyGame.Example.Monster (//monster_test.fbs) *)
    and Monster : sig
      type t

      module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t Rt.wip

      val extension : string option
      val identifier : string option
      val has_identifier : ?size_prefixed:bool -> ?off:int -> R.T.buf -> bool
      val root : ?size_prefixed:bool -> ?off:int -> R.T.buf -> t Rt.root
      val finish_buf : ?size_prefixed:bool -> Rt.Builder.t -> t Rt.wip -> R.T.buf

      val pos : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Vec3.t) Rt.fbopt
      val mana : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Short.t
      val hp : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Short.t
      val name : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.String.t) Rt.fb
      val inventory : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.UByte.Vector.t) Rt.fbopt
      val color : 'b Rt.buf -> ('b, t) Rt.fb -> Color.t
      val test_type : 'b Rt.buf -> ('b, t) Rt.fb -> Any.t
      val test : ?none:'a -> ?monster:(('b, t) Rt.fb -> 'a) -> ?test_simple_table_with_enum:(('b, TestSimpleTableWithEnum.t) Rt.fb -> 'a) -> ?my_game_example2_monster:(('b, Example2.Monster.t) Rt.fb -> 'a) -> default:(Any.t -> 'a) -> 'b Rt.buf -> ('b, t) Rt.fb -> 'a
      val test4 : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Test.Vector.t) Rt.fbopt
      val testarrayofstring : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.String.Vector.t) Rt.fbopt

      (** an example documentation comment: this will end up in the generated code multiline too *)
      val testarrayoftables : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Vector.t) Rt.fbopt
      val enemy : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, t) Rt.fbopt
      val testnestedflatbuffer : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.UByte.Vector.t) Rt.fbopt
      val testempty : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Stat.t) Rt.fbopt
      val testbool : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Bool.t
      val testhashs32_fnv1 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Int.t
      val testhashu32_fnv1 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.UInt.t
      val testhashs64_fnv1 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Long.t
      val testhashu64_fnv1 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.ULong.t
      val testhashs32_fnv1_a : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Int.t
      val testhashu32_fnv1_a : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.UInt.t
      val testhashs64_fnv1_a : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Long.t
      val testhashu64_fnv1_a : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.ULong.t
      val testarrayofbools : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.Bool.Vector.t) Rt.fbopt
      val testf : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Float.t
      val testf2 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Float.t
      val testf3 : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Float.t
      val testarrayofstring2 : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.String.Vector.t) Rt.fbopt
      val testarrayofsortedstruct : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Ability.Vector.t) Rt.fbopt
      val flex : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.UByte.Vector.t) Rt.fbopt
      val test5 : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Test.Vector.t) Rt.fbopt
      val vector_of_longs : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.Long.Vector.t) Rt.fbopt
      val vector_of_doubles : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.Double.Vector.t) Rt.fbopt
      val parent_namespace_test : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, InParentNamespace.t) Rt.fbopt
      val vector_of_referrables : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Referrable.Vector.t) Rt.fbopt
      val single_weak_reference : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.ULong.t
      val vector_of_weak_references : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.ULong.Vector.t) Rt.fbopt
      val vector_of_strong_referrables : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Referrable.Vector.t) Rt.fbopt
      val co_owning_reference : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.ULong.t
      val vector_of_co_owning_references : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.ULong.Vector.t) Rt.fbopt
      val non_owning_reference : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.ULong.t
      val vector_of_non_owning_references : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.ULong.Vector.t) Rt.fbopt
      val any_unique_type : 'b Rt.buf -> ('b, t) Rt.fb -> AnyUniqueAliases.t
      val any_unique : ?none:'a -> ?m:(('b, t) Rt.fb -> 'a) -> ?ts:(('b, TestSimpleTableWithEnum.t) Rt.fb -> 'a) -> ?m2:(('b, Example2.Monster.t) Rt.fb -> 'a) -> default:(AnyUniqueAliases.t -> 'a) -> 'b Rt.buf -> ('b, t) Rt.fb -> 'a
      val any_ambiguous_type : 'b Rt.buf -> ('b, t) Rt.fb -> AnyAmbiguousAliases.t
      val any_ambiguous : ?none:'a -> ?m1:(('b, t) Rt.fb -> 'a) -> ?m2:(('b, t) Rt.fb -> 'a) -> ?m3:(('b, t) Rt.fb -> 'a) -> default:(AnyAmbiguousAliases.t -> 'a) -> 'b Rt.buf -> ('b, t) Rt.fb -> 'a
      val vector_of_enums : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Color.Vector.t) Rt.fbopt
      val signed_enum : 'b Rt.buf -> ('b, t) Rt.fb -> Race.t
      val testrequirednestedflatbuffer : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Rt.UByte.Vector.t) Rt.fbopt
      val scalar_key_sorted_tables : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Stat.Vector.t) Rt.fbopt
      val native_inline : 'b Rt.buf -> ('b, t) Rt.fb -> ('b, Test.t) Rt.fbopt
      val long_enum_non_enum_default : 'b Rt.buf -> ('b, t) Rt.fb -> LongEnum.t
      val long_enum_normal_default : 'b Rt.buf -> ('b, t) Rt.fb -> LongEnum.t
      val nan_default : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Float.t
      val inf_default : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Float.t
      val positive_inf_default : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Float.t
      val infinity_default : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Float.t
      val positive_infinity_default : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Float.t
      val negative_inf_default : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Float.t
      val negative_infinity_default : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Float.t
      val double_inf_default : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.Double.t

      module Builder : sig
        type t

        val start : Rt.Builder.t -> t
        val finish : t -> Monster.t Rt.wip
        val add_pos : Vec3.t -> t -> t
        val add_mana : Rt.Short.t -> t -> t
        val add_hp : Rt.Short.t -> t -> t
        val add_name : Rt.String.t Rt.wip -> t -> t
        val add_inventory : Rt.UByte.Vector.t Rt.wip -> t -> t
        val add_color : Color.t -> t -> t
        val add_test_monster : Monster.t Rt.wip -> t -> t
        val add_test_test_simple_table_with_enum : TestSimpleTableWithEnum.t Rt.wip -> t -> t
        val add_test_my_game_example2_monster : Example2.Monster.t Rt.wip -> t -> t
        val add_test4 : Test.Vector.t Rt.wip -> t -> t
        val add_testarrayofstring : Rt.String.Vector.t Rt.wip -> t -> t
        val add_testarrayoftables : Monster.Vector.t Rt.wip -> t -> t
        val add_enemy : Monster.t Rt.wip -> t -> t
        val add_testnestedflatbuffer : Rt.UByte.Vector.t Rt.wip -> t -> t
        val add_testempty : Stat.t Rt.wip -> t -> t
        val add_testbool : Rt.Bool.t -> t -> t
        val add_testhashs32_fnv1 : Rt.Int.t -> t -> t
        val add_testhashu32_fnv1 : Rt.UInt.t -> t -> t
        val add_testhashs64_fnv1 : Rt.Long.t -> t -> t
        val add_testhashu64_fnv1 : Rt.ULong.t -> t -> t
        val add_testhashs32_fnv1_a : Rt.Int.t -> t -> t
        val add_testhashu32_fnv1_a : Rt.UInt.t -> t -> t
        val add_testhashs64_fnv1_a : Rt.Long.t -> t -> t
        val add_testhashu64_fnv1_a : Rt.ULong.t -> t -> t
        val add_testarrayofbools : Rt.Bool.Vector.t Rt.wip -> t -> t
        val add_testf : Rt.Float.t -> t -> t
        val add_testf2 : Rt.Float.t -> t -> t
        val add_testf3 : Rt.Float.t -> t -> t
        val add_testarrayofstring2 : Rt.String.Vector.t Rt.wip -> t -> t
        val add_testarrayofsortedstruct : Ability.Vector.t Rt.wip -> t -> t
        val add_flex : Rt.UByte.Vector.t Rt.wip -> t -> t
        val add_test5 : Test.Vector.t Rt.wip -> t -> t
        val add_vector_of_longs : Rt.Long.Vector.t Rt.wip -> t -> t
        val add_vector_of_doubles : Rt.Double.Vector.t Rt.wip -> t -> t
        val add_parent_namespace_test : InParentNamespace.t Rt.wip -> t -> t
        val add_vector_of_referrables : Referrable.Vector.t Rt.wip -> t -> t
        val add_single_weak_reference : Rt.ULong.t -> t -> t
        val add_vector_of_weak_references : Rt.ULong.Vector.t Rt.wip -> t -> t
        val add_vector_of_strong_referrables : Referrable.Vector.t Rt.wip -> t -> t
        val add_co_owning_reference : Rt.ULong.t -> t -> t
        val add_vector_of_co_owning_references : Rt.ULong.Vector.t Rt.wip -> t -> t
        val add_non_owning_reference : Rt.ULong.t -> t -> t
        val add_vector_of_non_owning_references : Rt.ULong.Vector.t Rt.wip -> t -> t
        val add_any_unique_m : Monster.t Rt.wip -> t -> t
        val add_any_unique_ts : TestSimpleTableWithEnum.t Rt.wip -> t -> t
        val add_any_unique_m2 : Example2.Monster.t Rt.wip -> t -> t
        val add_any_ambiguous_m1 : Monster.t Rt.wip -> t -> t
        val add_any_ambiguous_m2 : Monster.t Rt.wip -> t -> t
        val add_any_ambiguous_m3 : Monster.t Rt.wip -> t -> t
        val add_vector_of_enums : Color.Vector.t Rt.wip -> t -> t
        val add_signed_enum : Race.t -> t -> t
        val add_testrequirednestedflatbuffer : Rt.UByte.Vector.t Rt.wip -> t -> t
        val add_scalar_key_sorted_tables : Stat.Vector.t Rt.wip -> t -> t
        val add_native_inline : Test.t -> t -> t
        val add_long_enum_non_enum_default : LongEnum.t -> t -> t
        val add_long_enum_normal_default : LongEnum.t -> t -> t
        val add_nan_default : Rt.Float.t -> t -> t
        val add_inf_default : Rt.Float.t -> t -> t
        val add_positive_inf_default : Rt.Float.t -> t -> t
        val add_infinity_default : Rt.Float.t -> t -> t
        val add_positive_infinity_default : Rt.Float.t -> t -> t
        val add_negative_inf_default : Rt.Float.t -> t -> t
        val add_negative_infinity_default : Rt.Float.t -> t -> t
        val add_double_inf_default : Rt.Double.t -> t -> t
      end
    end

    (* Struct MyGame.Example.Ability (//monster_test.fbs) *)
    and Ability : sig
      type t = (Rt.UInt.t * Rt.UInt.t)

      module Vector : Rt.VectorS with type 'b elt := ('b, t) Rt.fb and type builder_elt := t

      val id : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.UInt.t
      val distance : 'b Rt.buf -> ('b, t) Rt.fb -> Rt.UInt.t
    end
  end (* Example *)
end (* MyGame *)
end
