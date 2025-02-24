(** Automatically generated by the FlatBuffers compiler

    root type: MyGame.Sample.Monster (//monster.fbs)
    flatc version: 23.3.3
*)

[@@@warning "-32"]

module Rt = Flatbuffers.Runtime

module Struct = struct

  let set_vec3__0 b i (x_, y_, z_) =
    Rt.Builder.set_scalar TFloat b (i + 0) x_;
    Rt.Builder.set_scalar TFloat b (i + 4) y_;
    Rt.Builder.set_scalar TFloat b (i + 8) z_;
end

module Union = struct
  let read_table_equipment__1 ?none ?weapon ~default b i t o =
    match Rt.UType.to_default t with
    | 0L when Option.is_some none -> Option.get none
    | 1L when Option.is_some weapon -> Option.get weapon (Rt.Ref.read_table b o i)
    | _ -> default t
end

module MyGame = struct
  module Sample = struct
    module Equipment = struct
      type t = Rt.UType.t

      let none = Rt.UType.of_default 0L
      let weapon = Rt.UType.of_default 1L

      let to_string e =
        match Rt.UType.to_default e with
        | 0L -> "none"
        | 1L -> "weapon"
        | x -> "<MyGame.Sample.Equipment: " ^ (Int64.to_string x) ^ ">"
    end

    module Color = struct
      type t = Rt.Byte.t

      let red = Rt.Byte.of_default 0L
      let green = Rt.Byte.of_default 1L
      let blue = Rt.Byte.of_default 2L

      let to_string e =
        match Rt.Byte.to_default e with
        | 0L -> "red"
        | 1L -> "green"
        | 2L -> "blue"
        | x -> "<MyGame.Sample.Color: " ^ (Int64.to_string x) ^ ">"

      module Vector = Rt.Byte.Vector
    end

    module Weapon = struct
      type t

      module Vector = Rt.Ref.Vector

      let[@inline] name b o = Rt.Ref.read_table_opt b o 4
      let[@inline] damage b o = Rt.Short.(read_table_default b o 6 ~default:(of_default 0L))

      module Builder = struct
        type t = Rt.Builder.t

        let start b = Rt.Builder.start_table b ~n_fields:2
        let finish b = Rt.Builder.end_table b
        let add_name = Rt.Ref.push_slot 0
        let add_damage = Rt.Short.(push_slot_default 1 ~default:(of_default 0L))
      end
    end

    module Vec3 = struct
      type t = (Rt.Float.t * Rt.Float.t * Rt.Float.t)

      module Vector = Rt.Struct.Vector (struct type builder_elt = t let size = 12 let set = Struct.set_vec3__0 end)

      let[@inline] x b s = Rt.Float.read_offset b s 0
      let[@inline] y b s = Rt.Float.read_offset b s 4
      let[@inline] z b s = Rt.Float.read_offset b s 8
    end

    module Monster = struct
      type t

      module Vector = Rt.Ref.Vector

      let extension = None
      let identifier = None
      let[@inline] root ?(size_prefixed = false) ?(off = 0) p b = Rt.get_root p b ~size_prefixed ~off
      let finish_buf ?(size_prefixed = false) = Rt.Builder.finish ?identifier ~size_prefixed

      let[@inline] pos b o = Rt.Struct.read_table_opt b o 4
      let[@inline] mana b o = Rt.Short.(read_table_default b o 6 ~default:(of_default 150L))
      let[@inline] hp b o = Rt.Short.(read_table_default b o 8 ~default:(of_default 100L))
      let[@inline] name b o = Rt.Ref.read_table_opt b o 10
      let[@inline] inventory b o = Rt.Ref.read_table_opt b o 14
      let[@inline] color b o = Rt.Byte.(read_table_default b o 16 ~default:(of_default 2L))
      let[@inline] weapons b o = Rt.Ref.read_table_opt b o 18
      let[@inline] equipped_type b o = Rt.UType.(read_table_default b o 20 ~default:(of_default 0L))
      let[@inline] equipped ?none ?weapon ~default b o = Union.read_table_equipment__1 b 22 (equipped_type b o) ?none ?weapon ~default o
      let[@inline] path b o = Rt.Ref.read_table_opt b o 24

      module Builder = struct
        type t = Rt.Builder.t

        let start b = Rt.Builder.start_table b ~n_fields:11
        let finish b = Rt.Builder.end_table b
        let add_pos = Rt.Struct.push_slot Struct.set_vec3__0 12 4 0
        let add_mana = Rt.Short.(push_slot_default 1 ~default:(of_default 150L))
        let add_hp = Rt.Short.(push_slot_default 2 ~default:(of_default 100L))
        let add_name = Rt.Ref.push_slot 3
        let add_inventory = Rt.Ref.push_slot 5
        let add_color = Rt.Byte.(push_slot_default 6 ~default:(of_default 2L))
        let add_weapons = Rt.Ref.push_slot 7
        let add_equipped_weapon = Rt.Ref.push_union 8 9 Equipment.weapon
        let add_path = Rt.Ref.push_slot 10
      end
    end
  end (* Sample *)
end (* MyGame *)
