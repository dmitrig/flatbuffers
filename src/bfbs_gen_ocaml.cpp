/*
 * Copyright 2021 Google Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "bfbs_gen_ocaml.h"

#include <cstdint>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <unordered_set>
#include <vector>

// Ensure no includes to flatc internals. bfbs_gen.h and generator.h are OK.
#include "bfbs_gen.h"
#include "bfbs_namer.h"

// The intermediate representation schema.
#include "flatbuffers/flexbuffers.h"
#include "flatbuffers/idl.h"
#include "flatbuffers/reflection.h"
#include "flatbuffers/reflection_generated.h"
#include "flatbuffers/util.h"

namespace flatbuffers {
namespace {

// To reduce typing
namespace r = ::reflection;

// TODO(dmitrig): treat identifiers used in generation as keywords for renaming?
std::set<std::string> OCamlKeywords() {
  return { "and",     "as",         "assert",    "asr",         "begin",
           "class",   "constraint", "do",        "done",        "downto",
           "else",    "end",        "exception", "external",    "false",
           "for",     "fun",        "function",  "functor",     "if",
           "in",      "include",    "inherit",   "initializer", "land",
           "lazy",    "let",        "lor",       "lsl",         "lsr",
           "lxor",    "match",      "method",    "mod",         "module",
           "mutable", "new",        "nonrec",    "object",      "of",
           "open",    "or",         "private",   "rec",         "sig",
           "struct",  "then",       "to",        "true",        "try",
           "type",    "val",        "virtual",   "when",        "while",
           "with" };
}

Namer::Config OCamlDefaultConfig() {
  return { /*types=*/Case::kSnake2,
           /*constants=*/Case::kSnake2,
           /*methods=*/Case::kSnake2,
           /*functions=*/Case::kSnake2,
           /*fields=*/Case::kSnake2,
           /*variables=*/Case::kSnake2,
           /*variants=*/Case::kSnake2,
           /*enum_variant_seperator=*/"",
           /*escape_keywords=*/Namer::Config::Escape::AfterConvertingCase,
           /*namespaces=*/Case::kUpperCamel,
           /*namespace_seperator=*/".",
           /*object_prefix=*/"",
           /*object_suffix=*/"",
           /*keyword_prefix=*/"",
           /*keyword_suffix=*/"_",
           /*filenames=*/Case::kSnake2,
           /*directories=*/Case::kKeep,
           /*output_path=*/"",
           /*filename_suffix=*/"",
           /*filename_extension=*/".ml" };
}

class OCamlBfbsGenerator : public BaseBfbsGenerator {
 public:
  explicit OCamlBfbsGenerator(const std::string &flatc_version)
      : BaseBfbsGenerator(),
        root_node_(),
        ident_counter_(0),
        enum_reader_idents_(),
        flatc_version_(flatc_version),
        namer_(OCamlDefaultConfig(), OCamlKeywords()) {}

  Status GenerateFromSchema(const r::Schema *schema) FLATBUFFERS_OVERRIDE {
    // build single module tree for all definitions
    ForAllObjects(schema->objects(), [&](const r::Object *object) {
      auto node = AddNode(object->name()->str());
      node->object = object;
    });
    ForAllEnums(schema->enums(), [&](const r::Enum *enum_def) {
      auto node = AddNode(enum_def->name()->str());
      node->enum_def = enum_def;
    });

    std::string intf, impl;
    EmitCodeHeader(intf, impl);

    // generate out-of-line definitions to avoid references between modules
    GenerateStructSetFns(impl);
    GenerateUnionReadFns(impl);

    // generate user-facing modules
    EmitCode(intf, impl);

    WriteFiles(intf, impl);
    return OK;
  }

  using BaseBfbsGenerator::GenerateCode;

  Status GenerateCode(const Parser &parser, const std::string &path,
                      const std::string &filename) FLATBUFFERS_OVERRIDE {
    (void)parser;
    (void)path;
    (void)filename;
    return NOT_IMPLEMENTED;
  }

  Status GenerateMakeRule(const Parser &parser, const std::string &path,
                          const std::string &filename,
                          std::string &output) override {
    (void)parser;
    (void)path;
    (void)filename;
    (void)output;
    return Status::NOT_IMPLEMENTED;
  }

  Status GenerateGrpcCode(const Parser &parser, const std::string &path,
                          const std::string &filename) override {
    (void)parser;
    (void)path;
    (void)filename;
    return Status::NOT_IMPLEMENTED;
  }

  Status GenerateRootFile(const Parser &parser,
                          const std::string &path) override {
    (void)parser;
    (void)path;
    return Status::NOT_IMPLEMENTED;
  }

  bool IsSchemaOnly() const override { return true; }

  bool SupportsBfbsGeneration() const override { return true; }

  bool SupportsRootFileGeneration() const override { return false; }

  IDLOptions::Language Language() const override { return IDLOptions::kOCaml; }

  std::string LanguageName() const override { return "OCaml"; }

  uint64_t SupportedAdvancedFeatures() const FLATBUFFERS_OVERRIDE {
    return r::OptionalScalars;
  }

 private:
  // TODO(dmitrig): just 'open Rt' don't bother trying to make this a variable
  const std::string RuntimeNS = "Rt";

  std::string UnionReadIdent(const r::Enum *enum_def) {
    const auto enum_name = enum_def->name()->str();
    std::string read_fn = enum_reader_idents_[enum_name];
    if (read_fn.empty()) {
      read_fn = "read_table_" + namer_.Function(namer_.Denamespace(enum_name)) +
                "__" + NumToString(ident_counter_++);
      enum_reader_idents_[enum_name] = read_fn;
    }
    return read_fn;
  }

  std::string StructSetIdent(const r::Object *object) {
    const auto object_name = object->name()->str();
    std::string repr = enum_reader_idents_[object_name];
    if (repr.empty()) {
      repr = "set_" + namer_.Function(namer_.Denamespace(object_name)) + "__" +
             NumToString(ident_counter_++);
      enum_reader_idents_[object_name] = repr;
    }
    return repr;
  }

  void GenerateUnionReadFns(std::string &impl) {
    const auto indent = Indent(1);

    std::vector<const r::Enum *> unions;
    ForAllEnums(schema_->enums(), [&](const r::Enum *enum_def) {
      if (enum_def->is_union()) unions.push_back(enum_def);
    });

    if (unions.empty()) return;

    impl += "module Union = struct";
    for (auto enum_def : unions) {
      const std::string ns = GenerateImplNs(enum_def->underlying_type());
      const std::string reader_name = UnionReadIdent(enum_def);

      const std::string args = GenerateUnionArgs(enum_def->underlying_type());
      impl += "\n" + indent + "let " + reader_name + args + " b i t o =\n";
      impl += indent + "  match " + ns + ".to_default t with\n";
      ForAllEnumValues(enum_def, [&](const reflection::EnumVal *e) {
        auto arg = namer_.Variable(e->name()->str());
        impl += indent + "  | " + Int64ToString(e->value()) +
                " when Option.is_some " + arg + " -> ";
        impl += "Option.get " + arg;
        if (e->union_type()->base_type() == r::None) {
          impl += "\n";
        } else {
          impl +=
              " (" + GenerateImplNs(e->union_type()) + ".read_table b o i)\n";
        }
      });
      impl += indent + "  | _ -> default t\n";
    }
    impl += "end\n\n";
  }

  void GenerateStructSetFns(std::string &impl) {
    const auto indent = Indent(1);

    std::vector<const r::Object *> structs;
    ForAllObjects(schema_->objects(), [&](const r::Object *object) {
      if (object->is_struct()) structs.push_back(object);
    });

    if (structs.empty()) return;

    impl += "module Struct = struct\n";
    bool first = true;
    bool rec = (structs.size() > 1);
    for (auto object : structs) {
      std::string set_args;
      std::string set_body;
      bool first_field = true;
      ForAllFields(object, /*reverse=*/false, [&](const r::Field *field) {
        const auto arg_name = namer_.Variable(field->name()->str());
        set_args += (first_field ? "" : ", ") + arg_name + "_";
        set_body += indent + "  " + StructSetFn(field->type()) + " b (i + " +
                    NumToString(field->offset()) + ") " + arg_name + "_;\n";
        if (field->padding() != 0) {
          set_body +=
              indent + "  Rt.Builder.set_padding b (i + " +
              NumToString(field->offset() + field->type()->base_size()) + ") " +
              NumToString(field->padding()) + ";\n";
        }

        first_field = false;
      });

      // TODO(dmitrig): overly complicated way to avoid "unused rec"
      impl += "\n" + indent + (first ? "let " : "and ") + (rec ? "rec " : "") +
              StructSetIdent(object) + " b i (" + set_args + ") =\n" + set_body;
      first = false;
      rec = false;
    }
    impl += "end\n\n";
  }

  void GenerateEnum(const r::Enum *enum_def, int level, std::string &intf,
                    std::string &impl) {
    const auto indent = Indent(level);
    const std::string ns = GenerateImplNs(enum_def->underlying_type());
    const std::string type = ns + ".t";

    intf += indent + "type t = private " + type + "\n\n";
    impl += indent + "type t = " + type + "\n\n";

    // enum constants
    ForAllEnumValues(enum_def, [&](const reflection::EnumVal *e) {
      const auto comment = GenerateDocumentation(e->documentation(), level);
      if (!comment.empty()) intf += "\n" + comment;
      intf += indent + "val " + namer_.Variant(e->name()->str()) + " : t\n";
      impl += indent + "let " + namer_.Variant(e->name()->str()) + " = " + ns +
              ".of_default " + Int64ToString(e->value()) + "\n";
    });

    // to_string for enums and union tags
    intf += indent + "val to_string : t -> string\n";
    impl += "\n" + indent + "let to_string e =\n";
    impl += indent + "  match " + ns + ".to_default e with\n";
    ForAllEnumValues(enum_def, [&](const reflection::EnumVal *e) {
      impl += indent + "  | " + Int64ToString(e->value()) + " -> \"" +
              namer_.Variant(e->name()->str()) + "\"\n";
    });
    impl += indent + "  | x -> \"<" + enum_def->name()->str() +
            ": \" ^ (Int64.to_string x) ^ \">\"\n";

    if (!enum_def->is_union()) {
      // vector module
      intf += "\n" + indent +
              "module Vector : Rt.VectorS with type 'b elt := t and type "
              "builder_elt := t\n";
      impl += "\n" + indent + "module Vector = " + ns + ".Vector\n";
    }
  }

  void GenerateObject(const r::Object *object, int level, std::string &intf,
                      std::string &impl) {
    const std::string indent = Indent(level);
    const std::string obj_name = object->name()->str();
    std::string obj_ns;
    const std::string obj_shortname = namer_.Denamespace(obj_name, obj_ns);

    if (object->is_struct()) {
      intf +=
          indent + "type t = " + StructReprArgTypes(object, obj_ns) + "\n\n";
      impl += indent + "type t = " + StructReprArgTypes(object, obj_ns, false) +
              "\n\n";
    } else {
      intf += indent + "type t\n\n";
      impl += indent + "type t\n\n";
    }

    // vector module
    if (object->is_struct()) {
      // TODO(dmitrig): use minalign as well?
      intf += indent +
              "module Vector : Rt.VectorS with type 'b elt := ('b, t) " +
              RuntimeNS + ".fb and type builder_elt := t\n\n";
      impl += indent + "module Vector = " + RuntimeNS +
              ".Struct.Vector (struct type builder_elt = t let size = " +
              NumToString(object->bytesize()) + " let set = Struct." +
              StructSetIdent(object) + " end)\n\n";
    } else {
      intf += indent +
              "module Vector : Rt.VectorS with type 'b elt := ('b, t) " +
              RuntimeNS + ".fb and type builder_elt := " + "t " + RuntimeNS +
              ".wip" + "\n\n";
      impl += indent + "module Vector = " + RuntimeNS + ".Ref.Vector\n\n";
    }

    // root table
    if (schema_->root_table() == object) {
      intf += indent + "val extension : string option\n";
      auto ident = schema_->file_ext()->str();
      impl += indent + "let extension = " +
              (ident.empty() ? "None" : "Some \"" + ident + "\"") + "\n";

      intf += indent + "val identifier : string option\n";
      auto ext = schema_->file_ident()->str();
      impl += indent + "let identifier = " +
              (ext.empty() ? "None" : "Some \"" + ext + "\"") + "\n";

      if (!ident.empty()) {
        intf += indent +
                "val has_identifier : ?size_prefixed:bool -> ?off:int -> "
                "'b Flatbuffers.Primitives.t -> 'b -> bool\n";
        impl +=
            indent +
            "let has_identifier ?(size_prefixed = false) ?(off = 0) p b = " +
            RuntimeNS +
            ".get_identifier p b ~size_prefixed ~off = Option.get identifier\n";
      }

      intf += indent +
              "val root : ?size_prefixed:bool -> ?off:int -> 'b "
              "Flatbuffers.Primitives.t "
              "-> 'b -> t " +
              RuntimeNS + ".root\n";
      impl += indent +
              "let[@inline] root ?(size_prefixed = false) ?(off = 0) p b = " +
              RuntimeNS + ".get_root p b ~size_prefixed ~off\n";

      if (schema_->root_table() == object) {
        intf +=
            indent +
            "val finish_buf : ?size_prefixed:bool -> 'a "
            "Flatbuffers.Primitives.t -> Rt.Builder.t -> t Rt.wip -> 'a\n\n";
        impl += indent +
                "let finish_buf ?(size_prefixed = false) = " + RuntimeNS +
                ".Builder.finish ?identifier ~size_prefixed\n\n";
      }
    }

    // create all the field accessors.
    ForAllFields(object, /*reverse=*/false, [&](const r::Field *field) {
      if (field->deprecated()) { return; }

      const auto comment = GenerateDocumentation(field->documentation(), level);
      if (!comment.empty()) intf += "\n" + comment;

      const std::string field_name = namer_.Field(*field);
      const r::BaseType field_type = field->type()->base_type();

      // generate accessor interface
      if (field_type == r::Union) {
        auto args = GenerateUnionArgTypes(field->type(), obj_name);
        intf += indent + "val " + namer_.Function(field_name) + " :" + args +
                " 'b Rt.buf -> ('b, t) " + RuntimeNS + ".fb -> 'a\n";
      } else {
        intf += indent + "val " + namer_.Function(field_name) +
                " : 'b Rt.buf -> ('b, t) " + RuntimeNS + ".fb -> " +
                GenerateReaderType(
                    field->type(), obj_name,
                    /*optional=*/field->optional() && !object->is_struct()) +
                "\n";
      }

      // generate accessor implementation
      if (object->is_struct()) {
        impl += indent + "let[@inline] " + namer_.Function(field_name) +
                " b s = " + GenerateImplNs(field->type()) +
                ".read_offset b s " + NumToString(field->offset()) + "\n";
      } else {
        if (field_type == r::Union) {
          auto ns = GenerateIntfNs(field->type(), obj_name);
          auto args = GenerateUnionArgs(field->type());
          // TODO(dmitrig): default tag may not be stored, need default arg
          impl += indent + "let[@inline] " + namer_.Function(field_name) +
                  args + " b o = Union." +
                  UnionReadIdent(GetEnum(field->type())) + " b " +
                  NumToString(field->offset()) + " (" + field_name +
                  UnionTypeFieldSuffix() + " b o)" + args + " o\n";
        } else {
          std::string getter_fn, default_arg;
          if (field->optional()) {
            getter_fn = ".read_table_opt";
          } else if (field->required()) {
            getter_fn = ".read_table";
          } else {
            getter_fn = ".(read_table_default";
            default_arg = " ~default:(" + GenerateDefault(field) + "))";
          }
          impl += indent + "let[@inline] " + namer_.Function(field_name) +
                  " b o = " + GenerateImplNs(field->type()) + getter_fn +
                  " b o " + NumToString(field->offset()) + default_arg + "\n";
        }
      }
    });

    if (!object->is_struct()) {
      // generate builder
      const std::string indent2 = Indent(level + 1);

      impl += "\n" + indent + "module Builder = struct\n";
      impl += indent2 + "type t = Rt.Builder.t\n\n";

      intf += "\n" + indent + "module Builder : sig\n";
      intf += indent2 + "type t\n\n";

      impl += indent2 + "let start b = Rt.Builder.start_table b ~n_fields:" +
              NumToString(object->fields()->size()) + "\n";
      impl += indent2 + "let finish b = Rt.Builder.end_table b\n";

      intf += indent2 + "val start : Rt.Builder.t -> t\n";
      // TODO(dmitrig): gross namespacing here
      intf += indent2 + "val finish : t -> " + obj_shortname + ".t Rt.wip\n";

      ForAllFields(object, /*reverse=*/false, [&](const r::Field *field) {
        // Skip writing deprecated fields altogether.
        if (field->deprecated()) { return; }

        const std::string field_name = namer_.Field(*field);
        const r::BaseType field_type = field->type()->base_type();

        if (field_type == r::UType) {
          return;  // no builder function for tags
        } else if (field_type == r::Union) {
          ForAllEnumValues(GetEnum(field->type()), [&](const r::EnumVal *e) {
            if (e->union_type()->base_type() == r::None) return;
            const auto variant_name = namer_.Variant(e->name()->str());
            impl += indent2 + "let add_" + namer_.Function(field_name) + "_" +
                    variant_name + " = Rt.Ref.push_union " +
                    NumToString(field->id() - 1) + " " +
                    NumToString(field->id()) + " " +
                    GenerateIntfNs(field->type(), obj_ns) + "." + variant_name +
                    "\n";
            intf += indent2 + "val add_" + namer_.Function(field_name) + "_" +
                    variant_name + " : " +
                    GenerateBuilderType(e->union_type(), obj_ns) +
                    " -> t -> t\n";
          });
        } else {
          // struct fields take an extra arg
          std::string push_fn, repr_arg, default_arg;
          if (IsStruct(field->type())) {
            auto object = GetObject(field->type());
            repr_arg = " Struct." + StructSetIdent(object) + " " +
                       NumToString(object->bytesize()) + " " +
                       NumToString(object->minalign());
          }
          if (!field->optional() && !field->required()) {
            push_fn = ".(push_slot_default";
            default_arg = " ~default:(" + GenerateDefault(field) + "))";
          } else {
            push_fn = ".push_slot";
          }
          impl += indent2 + "let add_" + namer_.Function(field_name) + " = " +
                  GenerateImplNs(field->type()) + push_fn + repr_arg + " " +
                  NumToString(field->id()) + default_arg + "\n";
          intf += indent2 + "val add_" + namer_.Function(field_name) + " : " +
                  GenerateBuilderType(field->type(), obj_ns) + " -> t -> t\n";
        }
      });
      impl += indent + "end\n";
      intf += indent + "end\n";
    }
  }

  std::string Int64ToString(int64_t x) const {
    if (x < 0)
      return "(" + NumToString(x) + "L)";
    else
      return NumToString(x) + "L";
  }

  std::string FloatToString(float x) const {
    if (x != x)
      return "nan";
    else if (x == std::numeric_limits<double>::infinity())
      return "infinity";
    else if (x == -std::numeric_limits<double>::infinity())
      return "neg_infinity";

    if (x < 0.0)
      return "(" + NumToString(x) + ")";
    else
      return NumToString(x);
  }

  std::string GenerateDefault(const r::Field *field) const {
    const r::BaseType base_type = field->type()->base_type();
    const std::string f = "of_default ";
    if (IsBool(base_type))
      return f + (field->default_integer() ? "true" : "false");
    else if (IsInteger(base_type))
      return f + Int64ToString(field->default_integer());
    else if (IsFloatingPoint(base_type))
      return f + FloatToString(field->default_real());
    else
      // should be unreachable
      return "{{ ERROR GenerateDefault }}";
  }

  // these are NOT represented by an offset (RT.fb, RT.fbopt) in the runtime:
  // scalars, enums (incl union tags)
  bool IsImmediateType(const r::Type *type, bool element_type = false) {
    const r::BaseType base_type =
        element_type ? type->element() : type->base_type();
    return IsScalar(base_type);
  }

  // in_ns should be an object/enum name as-is
  std::string GenerateIntfNs(const r::Type *type, const std::string &in_ns,
                             bool element_type = false) {
    const r::BaseType base_type =
        element_type ? type->element() : type->base_type();
    if ((IsScalar(base_type) && type->index() < 0) || base_type == r::String) {
      return RuntimeNS + "." + r::EnumNameBaseType(base_type);
    } else if (base_type == r::Vector) {
      auto ns = GenerateIntfNs(type, in_ns, /*elt_type=*/true);
      if (!ns.empty()) ns += ".";
      return ns + "Vector";
    } else if (base_type == r::Obj) {
      auto object = GetObject(type, element_type);
      return namer_.Namespace(
          NamespaceRelComponents(object->name()->str(), in_ns));
    } else if (base_type == r::Union || IsInteger(base_type)) {
      auto enum_def = GetEnum(type, element_type);
      return namer_.Namespace(
          NamespaceRelComponents(enum_def->name()->str(), in_ns));
    } else {
      return "{{ ERROR GenerateIntfNs }}\n";
    }
  }

  // namespace of reader implementations: Rt.Ref, Rt.Struct, or Rt.Scalar
  std::string GenerateImplNs(const r::Type *type, bool element_type = false) {
    const r::BaseType base_type =
        element_type ? type->element() : type->base_type();
    if (IsScalar(base_type)) {
      return RuntimeNS + "." + r::EnumNameBaseType(base_type);
    } else if (base_type == r::Vector || base_type == r::String) {
      return RuntimeNS + ".Ref";
    } else if (base_type == r::Obj) {
      auto object = GetObject(type, element_type);
      if (object->is_struct())
        return RuntimeNS + ".Struct";
      else
        return RuntimeNS + ".Ref";
    } else if (base_type == r::Union) {
      const auto enum_def = GetEnum(type);
      return GenerateImplNs(enum_def->underlying_type());
    } else {
      return "{{ ERROR GenerateImplNs }}\n";
    }
  }

  std::string GenerateType(const r::Type *type, const std::string &in_ns,
                           bool intf = true) {
    auto ns = intf ? GenerateIntfNs(type, in_ns) : GenerateImplNs(type);
    if (!ns.empty()) ns += ".";
    return ns + "t";
  }

  std::string GenerateReaderType(const r::Type *type, const std::string &in_ns,
                                 bool optional = false) {
    auto type_name = GenerateType(type, in_ns);
    if (IsImmediateType(type))
      return type_name + (optional ? " option" : "");
    else
      return "('b, " + type_name + ") " + RuntimeNS +
             (optional ? ".fbopt" : ".fb");
  }

  // types for builder arguments. Just uses tuple arguments for struct types,
  // just like the Struct.Vector definition does for builder_elt ....
  std::string GenerateBuilderType(const r::Type *type,
                                  const std::string &in_ns) {
    auto type_name = GenerateType(type, in_ns);
    if (IsImmediateType(type) || IsStruct(type))
      return type_name;
    else
      return type_name + " " + RuntimeNS + ".wip";
  }

  // need 3 forms:
  // - unexpanded, MyStruct.t (implmented with GenerateType?); for builder args
  // - 1 level: if top level type it's a struct, expanded to tuple; for mli
  // definition
  // - full: expand structs recursively, enums replaced with underlying type;
  // for ml definition
  std::string StructReprArgTypes(const r::Object *object,
                                 const std::string &in_ns, bool intf = true) {
    bool first = true;
    std::string res = "(";
    ForAllFields(object, /*reverse=*/false, [&](const r::Field *field) {
      if (IsScalar(field->type()->base_type()) ||
          (IsStruct(field->type()) && intf)) {
        res += (first ? "" : " * ") + GenerateType(field->type(), in_ns, intf);
      } else if (IsStruct(field->type()) && !intf) {
        res += (first ? "" : " * ") +
               StructReprArgTypes(GetObject(field->type()), in_ns, intf);
      } else {
        res += "{{ ERROR StructReprArgTypes }}\n";
      }
      first = false;
    });
    return res + ")";
  }

  std::string StructSetFn(const r::Type *type) {
    const r::BaseType base_type = type->base_type();
    if (IsScalar(base_type)) {
      return RuntimeNS + ".Builder.set_" +
             namer_.Function(r::EnumNameBaseType(base_type));
    } else if (base_type == r::Obj) {
      auto object = GetObject(type);
      if (object->is_struct()) return StructSetIdent(object);
    }
    return "{{ ERROR StructReprArg }}";
  }

  std::string GenerateUnionArgTypes(const r::Type *type,
                                    const std::string &in_ns) {
    std::string args = "";
    ForAllEnumValues(GetEnum(type), [&](const reflection::EnumVal *e) {
      if (e->union_type()->base_type() == r::None) {
        args += " ?none:'a ->";
      } else {
        args += " ?" + namer_.Variable(e->name()->str()) + ":(" +
                GenerateReaderType(e->union_type(), in_ns) + " -> 'a) ->";
      }
    });
    args += " default:(" + GenerateType(type, in_ns) + " -> 'a) ->";
    return args;
  }

  // TODO(dmitrig): this uses the literal name, which may include long
  // namespaces. Check what other generators do
  std::string GenerateUnionArgs(const r::Type *type) {
    std::string args = "";
    ForAllEnumValues(GetEnum(type), [&](const r::EnumVal *e) {
      args += " ?" + namer_.Variable(e->name()->str());
    });
    args += " ~default";
    return args;
  }

  std::string GenerateDocumentation(
      const flatbuffers::Vector<flatbuffers::Offset<flatbuffers::String>> *docs,
      int level, std::string extra = "") const {
    const std::string indent = Indent(level);
    std::string res = "";
    if (!docs) return res;

    // TODO(dmitrig): proper multiline comments
    res += indent + "(**";
    flatbuffers::ForAllDocumentation(
        docs, [&](const flatbuffers::String *str) { res += str->str(); });
    if (!extra.empty()) res += "\n\n" + indent + "    " + extra;
    res += " *)\n";
    return res;
  }

  std::string Indent(int level, int spaces = 2) const {
    return std::string(spaces * level, ' ');
  }

  struct Node {
    std::string name;
    int level = 0;
    const r::Enum *enum_def = nullptr;
    const r::Object *object = nullptr;
    std::map<std::string, size_t> children;
    std::vector<Node> nodes;
  };

  std::vector<std::string> NamespaceComponents(const std::string &full_name) {
    std::vector<std::string> labels;
    {
      std::string ns, rest = full_name;
      while (rest != "") {
        labels.push_back(namer_.Denamespace(rest, ns));
        rest = ns;
      }
    }

    // TODO(dmitrig): cleanup
    std::reverse(labels.begin(), labels.end());
    return labels;
  }

  std::vector<std::string> NamespaceRelComponents(const std::string &full_name,
                                                  const std::string &ns) {
    std::vector<std::string> name_components = NamespaceComponents(full_name);
    std::vector<std::string> ns_components = NamespaceComponents(ns);

    auto namei = name_components.begin();
    auto nsi = ns_components.begin();
    while (namei != name_components.end() && nsi != ns_components.end() &&
           *namei == *nsi) {
      namei++;
      nsi++;
    }

    return std::vector<std::string>(namei, name_components.end());
  }

  Node *AddNode(const std::string &full_name) {
    std::vector<std::string> labels = NamespaceComponents(full_name);

    // TODO(dmitrig): simplify
    Node *current = &root_node_;
    int level = 0;
    for (const auto &l : labels) {
      if (current->children.find(l) == current->children.end()) {
        Node n;
        n.name = namer_.Namespace(l);
        n.level = level;
        current->nodes.push_back(n);
        current->children[l] = current->nodes.size() - 1;
      }
      level++;
      current = &current->nodes.at(current->children[l]);
    }

    return current;
  }

  void EmitCodeHeader(std::string &intf, std::string &impl) {
    std::string header =
        "(** Automatically generated by the FlatBuffers compiler\n\n";
    auto root_table = schema_->root_table();
    if (root_table != nullptr) {
      std::string root_type = root_table->name()->str();
      std::string root_file = root_table->declaration_file()->str();
      header += "    root type: " + root_type + " (" + root_file + ")\n";
    }
    header += "    flatc version: " + flatc_version_ + "\n*)\n\n";

    intf += header;
    impl += header;

    // runtime library
    impl += "[@@@warning \"-32\"]\n\n";  // turn off unused-value-declaration

    impl += "module Rt = Flatbuffers.Runtime\n\n";

    intf += "module Rt : Flatbuffers.Runtime.Intf\n\n";
  }

  void EmitCode(std::string &intf, std::string &impl) {
    std::set<const Node *> emitted;
    std::vector<const Node *> to_emit;
    for (auto &n : root_node_.nodes) { to_emit.push_back(&n); }

    bool start_level = true;

    while (!to_emit.empty()) {
      const auto &node = *to_emit.back();

      if (!emitted.count(&node)) {
        EmitNodePre(node, intf, impl, start_level);

        for (auto &n : node.nodes) { to_emit.push_back(&n); }
        start_level = !node.children.empty();

        emitted.insert(&node);
      } else {
        EmitNodePost(node, intf, impl);
        to_emit.pop_back();
      }
    }
  }

  void EmitNodePre(const Node &node, std::string &intf, std::string &impl,
                   bool start_level = false) {
    const std::string indent = Indent(node.level);
    std::string full_name;
    std::string declaring_file;

    if (node.enum_def) {
      full_name = node.enum_def->name()->str();
      declaring_file = node.enum_def->declaration_file()->str();
    } else if (node.object) {
      full_name = node.object->name()->str();
      declaring_file = node.object->declaration_file()->str();
    }

    std::string header;
    if (node.enum_def || node.object)
      header = " " + full_name + " (" + declaring_file + ")";
    if (node.enum_def)
      header = (node.enum_def->is_union() ? "Union" : "Enum") + header;
    else if (node.object)
      header = (node.object->is_struct() ? "Struct" : "Table") + header;

    std::string doc;
    if (node.enum_def) {
      doc = GenerateDocumentation(node.enum_def->documentation(), node.level,
                                  /*extra=*/header);
    } else if (node.object) {
      doc = GenerateDocumentation(node.object->documentation(), node.level,
                                  /*extra=*/header);
    }

    // TODO(dmitrig): not worth it?
    if (doc.empty() && !header.empty()) doc = indent + "(* " + header + " *)\n";

    if (start_level) {
      intf += doc + indent + "module rec " + node.name + " : sig\n";
      impl += indent + "module " + node.name + " = struct\n";
    } else {
      intf += "\n" + doc + indent + "and " + node.name + " : sig\n";
      impl += "\n" + indent + "module " + node.name + " = struct\n";
    }

    if (node.enum_def) {
      GenerateEnum(node.enum_def, node.level + 1, intf, impl);
    } else if (node.object) {
      GenerateObject(node.object, node.level + 1, intf, impl);
    }
  }

  void EmitNodePost(const Node &node, std::string &intf, std::string &impl) {
    const std::string indent = Indent(node.level);

    std::string comment;
    if (!(node.enum_def || node.object)) comment = " (* " + node.name + " *)";

    intf += indent + "end" + comment + "\n";
    impl += indent + "end" + comment + "\n";
  }

  void WriteFiles(const std::string &intf, const std::string &impl) {
    // TODO(dmitrig): manually naming output based on root here. Should suffix
    // _generated?
    auto root_table = schema_->root_table();
    std::string root_file =
        root_table ? root_table->declaration_file()->str() : "flatc_output";
    const std::string impl_file =
        namer_.File(StripExtension(StripPath(root_file)));
    const std::string intf_file = impl_file + "i";

    SaveFile(intf_file.c_str(), intf, false);
    SaveFile(impl_file.c_str(), impl, false);

    root_node_ = {};
  }

  Node root_node_;
  int ident_counter_;
  std::map<std::string, std::string> enum_reader_idents_;
  const std::string flatc_version_;
  const BfbsNamer namer_;
};
}  // namespace

std::unique_ptr<CodeGenerator> NewOCamlBfbsGenerator(
    const std::string &flatc_version) {
  return std::unique_ptr<OCamlBfbsGenerator>(
      new OCamlBfbsGenerator(flatc_version));
}

}  // namespace flatbuffers
