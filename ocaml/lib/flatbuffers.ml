module Runtime = Runtime
module BigstringRuntime = Runtime.Make (Primitives.Bigstring)
module BytesRuntime = Runtime.Make (Primitives.Bytes)
module StringRuntime = Runtime.Make (Primitives.String)
