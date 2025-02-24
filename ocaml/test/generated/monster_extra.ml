(** Automatically generated by the FlatBuffers compiler

    root type: MyGame.MonsterExtra (//monster_extra.fbs)
    flatc version: 23.3.3
*)

[@@@warning "-32"]

module Rt = Flatbuffers.Runtime

module MyGame = struct
  module MonsterExtra = struct
    type t

    module Vector = Rt.Ref.Vector

    let extension = Some "mon"
    let identifier = Some "MONE"
    let has_identifier ?(size_prefixed = false) ?(off = 0) p b = Rt.get_identifier p b ~size_prefixed ~off = Option.get identifier
    let[@inline] root ?(size_prefixed = false) ?(off = 0) p b = Rt.get_root p b ~size_prefixed ~off
    let finish_buf ?(size_prefixed = false) = Rt.Builder.finish ?identifier ~size_prefixed

    let[@inline] d0 b o = Rt.Double.(read_table_default b o 4 ~default:(of_default nan))
    let[@inline] d1 b o = Rt.Double.(read_table_default b o 6 ~default:(of_default nan))
    let[@inline] d2 b o = Rt.Double.(read_table_default b o 8 ~default:(of_default infinity))
    let[@inline] d3 b o = Rt.Double.(read_table_default b o 10 ~default:(of_default neg_infinity))
    let[@inline] f0 b o = Rt.Float.(read_table_default b o 12 ~default:(of_default nan))
    let[@inline] f1 b o = Rt.Float.(read_table_default b o 14 ~default:(of_default nan))
    let[@inline] f2 b o = Rt.Float.(read_table_default b o 16 ~default:(of_default infinity))
    let[@inline] f3 b o = Rt.Float.(read_table_default b o 18 ~default:(of_default neg_infinity))
    let[@inline] dvec b o = Rt.Ref.read_table_opt b o 20
    let[@inline] fvec b o = Rt.Ref.read_table_opt b o 22

    module Builder = struct
      type t = Rt.Builder.t

      let start b = Rt.Builder.start_table b ~n_fields:11
      let finish b = Rt.Builder.end_table b
      let add_d0 = Rt.Double.(push_slot_default 0 ~default:(of_default nan))
      let add_d1 = Rt.Double.(push_slot_default 1 ~default:(of_default nan))
      let add_d2 = Rt.Double.(push_slot_default 2 ~default:(of_default infinity))
      let add_d3 = Rt.Double.(push_slot_default 3 ~default:(of_default neg_infinity))
      let add_f0 = Rt.Float.(push_slot_default 4 ~default:(of_default nan))
      let add_f1 = Rt.Float.(push_slot_default 5 ~default:(of_default nan))
      let add_f2 = Rt.Float.(push_slot_default 6 ~default:(of_default infinity))
      let add_f3 = Rt.Float.(push_slot_default 7 ~default:(of_default neg_infinity))
      let add_dvec = Rt.Ref.push_slot 8
      let add_fvec = Rt.Ref.push_slot 9
    end
  end
end (* MyGame *)
