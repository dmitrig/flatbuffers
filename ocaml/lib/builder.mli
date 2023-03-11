module type Intf = Builder_intf.Intf

include Intf with module T = Primitives.T
