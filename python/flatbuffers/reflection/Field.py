# automatically generated by the FlatBuffers compiler, do not modify

# namespace: reflection

import flatbuffers
from flatbuffers.compat import import_numpy
np = import_numpy()

class Field(object):
    __slots__ = ['_tab']

    @classmethod
    def GetRootAs(cls, buf, offset=0):
        n = flatbuffers.encode.Get(flatbuffers.packer.uoffset, buf, offset)
        x = Field()
        x.Init(buf, n + offset)
        return x

    @classmethod
    def GetRootAsField(cls, buf, offset=0):
        """This method is deprecated. Please switch to GetRootAs."""
        return cls.GetRootAs(buf, offset)
    @classmethod
    def FieldBufferHasIdentifier(cls, buf, offset, size_prefixed=False):
        return flatbuffers.util.BufferHasIdentifier(buf, offset, b"\x42\x46\x42\x53", size_prefixed=size_prefixed)

    # Field
    def Init(self, buf, pos):
        self._tab = flatbuffers.table.Table(buf, pos)

    # Field
    def Name(self):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(4))
        if o != 0:
            return self._tab.String(o + self._tab.Pos)
        return None

    # Field
    def Type(self):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(6))
        if o != 0:
            x = self._tab.Indirect(o + self._tab.Pos)
            from reflection.Type import Type
            obj = Type()
            obj.Init(self._tab.Bytes, x)
            return obj
        return None

    # Field
    def Id(self):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(8))
        if o != 0:
            return self._tab.Get(flatbuffers.number_types.Uint16Flags, o + self._tab.Pos)
        return 0

    # Field
    def Offset(self):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(10))
        if o != 0:
            return self._tab.Get(flatbuffers.number_types.Uint16Flags, o + self._tab.Pos)
        return 0

    # Field
    def DefaultInteger(self):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(12))
        if o != 0:
            return self._tab.Get(flatbuffers.number_types.Int64Flags, o + self._tab.Pos)
        return 0

    # Field
    def DefaultReal(self):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(14))
        if o != 0:
            return self._tab.Get(flatbuffers.number_types.Float64Flags, o + self._tab.Pos)
        return 0.0

    # Field
    def Deprecated(self):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(16))
        if o != 0:
            return bool(self._tab.Get(flatbuffers.number_types.BoolFlags, o + self._tab.Pos))
        return False

    # Field
    def Required(self):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(18))
        if o != 0:
            return bool(self._tab.Get(flatbuffers.number_types.BoolFlags, o + self._tab.Pos))
        return False

    # Field
    def Key(self):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(20))
        if o != 0:
            return bool(self._tab.Get(flatbuffers.number_types.BoolFlags, o + self._tab.Pos))
        return False

    # Field
    def Attributes(self, j):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(22))
        if o != 0:
            x = self._tab.Vector(o)
            x += flatbuffers.number_types.UOffsetTFlags.py_type(j) * 4
            x = self._tab.Indirect(x)
            from reflection.KeyValue import KeyValue
            obj = KeyValue()
            obj.Init(self._tab.Bytes, x)
            return obj
        return None

    # Field
    def AttributesLength(self):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(22))
        if o != 0:
            return self._tab.VectorLen(o)
        return 0

    # Field
    def AttributesIsNone(self):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(22))
        return o == 0

    # Field
    def Documentation(self, j):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(24))
        if o != 0:
            a = self._tab.Vector(o)
            return self._tab.String(a + flatbuffers.number_types.UOffsetTFlags.py_type(j * 4))
        return ""

    # Field
    def DocumentationLength(self):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(24))
        if o != 0:
            return self._tab.VectorLen(o)
        return 0

    # Field
    def DocumentationIsNone(self):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(24))
        return o == 0

    # Field
    def Optional(self):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(26))
        if o != 0:
            return bool(self._tab.Get(flatbuffers.number_types.BoolFlags, o + self._tab.Pos))
        return False

    # Number of padding octets to always add after this field. Structs only.
    # Field
    def Padding(self):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(28))
        if o != 0:
            return self._tab.Get(flatbuffers.number_types.Uint16Flags, o + self._tab.Pos)
        return 0

def FieldStart(builder):
    return builder.StartObject(13)

def Start(builder):
    return FieldStart(builder)

def FieldAddName(builder, name):
    return builder.PrependUOffsetTRelativeSlot(0, flatbuffers.number_types.UOffsetTFlags.py_type(name), 0)

def AddName(builder, name):
    return FieldAddName(builder, name)

def FieldAddType(builder, type):
    return builder.PrependUOffsetTRelativeSlot(1, flatbuffers.number_types.UOffsetTFlags.py_type(type), 0)

def AddType(builder, type):
    return FieldAddType(builder, type)

def FieldAddId(builder, id):
    return builder.PrependUint16Slot(2, id, 0)

def AddId(builder, id):
    return FieldAddId(builder, id)

def FieldAddOffset(builder, offset):
    return builder.PrependUint16Slot(3, offset, 0)

def AddOffset(builder, offset):
    return FieldAddOffset(builder, offset)

def FieldAddDefaultInteger(builder, defaultInteger):
    return builder.PrependInt64Slot(4, defaultInteger, 0)

def AddDefaultInteger(builder, defaultInteger):
    return FieldAddDefaultInteger(builder, defaultInteger)

def FieldAddDefaultReal(builder, defaultReal):
    return builder.PrependFloat64Slot(5, defaultReal, 0.0)

def AddDefaultReal(builder, defaultReal):
    return FieldAddDefaultReal(builder, defaultReal)

def FieldAddDeprecated(builder, deprecated):
    return builder.PrependBoolSlot(6, deprecated, 0)

def AddDeprecated(builder, deprecated):
    return FieldAddDeprecated(builder, deprecated)

def FieldAddRequired(builder, required):
    return builder.PrependBoolSlot(7, required, 0)

def AddRequired(builder, required):
    return FieldAddRequired(builder, required)

def FieldAddKey(builder, key):
    return builder.PrependBoolSlot(8, key, 0)

def AddKey(builder, key):
    return FieldAddKey(builder, key)

def FieldAddAttributes(builder, attributes):
    return builder.PrependUOffsetTRelativeSlot(9, flatbuffers.number_types.UOffsetTFlags.py_type(attributes), 0)

def AddAttributes(builder, attributes):
    return FieldAddAttributes(builder, attributes)

def FieldStartAttributesVector(builder, numElems):
    return builder.StartVector(4, numElems, 4)

def StartAttributesVector(builder, numElems):
    return FieldStartAttributesVector(builder, numElems)

def FieldAddDocumentation(builder, documentation):
    return builder.PrependUOffsetTRelativeSlot(10, flatbuffers.number_types.UOffsetTFlags.py_type(documentation), 0)

def AddDocumentation(builder, documentation):
    return FieldAddDocumentation(builder, documentation)

def FieldStartDocumentationVector(builder, numElems):
    return builder.StartVector(4, numElems, 4)

def StartDocumentationVector(builder, numElems):
    return FieldStartDocumentationVector(builder, numElems)

def FieldAddOptional(builder, optional):
    return builder.PrependBoolSlot(11, optional, 0)

def AddOptional(builder, optional):
    return FieldAddOptional(builder, optional)

def FieldAddPadding(builder, padding):
    return builder.PrependUint16Slot(12, padding, 0)

def AddPadding(builder, padding):
    return FieldAddPadding(builder, padding)

def FieldEnd(builder):
    return builder.EndObject()

def End(builder):
    return FieldEnd(builder)
