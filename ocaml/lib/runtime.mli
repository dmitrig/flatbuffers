module type Intf = Runtime_intf.Intf
module type Intf_impl = Runtime_intf.Intf_impl

module Make (Primitives : Primitives_intf.Intf) :
  Runtime_intf.Intf_impl with module T = Primitives.T
