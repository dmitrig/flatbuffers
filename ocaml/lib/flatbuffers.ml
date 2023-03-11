module Runtime = Runtime

module BytesRuntime = struct

  #include "primitives_bytes.ml"

  #include "builder.incl.ml"

  #include "runtime.incl.ml"

end

module StringRuntime = struct

  #include "primitives_string.ml"

  #include "builder.incl.ml"

  #include "runtime.incl.ml"

end

module BigstringRuntime = struct

  #include "primitives_bigstring.ml"

  #include "builder.incl.ml"

  #include "runtime.incl.ml"

end
