module type Intf = Primitives_intf.Intf
module type Intf_types = Primitives_intf.Intf_types

module Bigstring :
  Primitives_intf.Intf
    with type T.buf = Bigstringaf.t
     and type T.bool = bool
     and type T.byte = int
     and type T.ubyte = char
     and type T.short = int
     and type T.ushort = int
     and type T.int = int32
     and type T.uint = int32
     and type T.long = int64
     and type T.ulong = int64
     and type T.float = float
     and type T.double = float

module String :
  Primitives_intf.Intf
    with type T.buf = String.t
     and type T.bool = bool
     and type T.byte = int
     and type T.ubyte = char
     and type T.short = int
     and type T.ushort = int
     and type T.int = int32
     and type T.uint = int32
     and type T.long = int64
     and type T.ulong = int64
     and type T.float = float
     and type T.double = float

module Bytes :
  Primitives_intf.Intf
    with type T.buf = Bytes.t
     and type T.bool = bool
     and type T.byte = int
     and type T.ubyte = char
     and type T.short = int
     and type T.ushort = int
     and type T.int = int32
     and type T.uint = int32
     and type T.long = int64
     and type T.ulong = int64
     and type T.float = float
     and type T.double = float
