module Make (Primitives : Primitives_intf.Intf) :
  Builder_intf.Intf with module T := Primitives.T
