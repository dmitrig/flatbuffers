# automatically generated by the FlatBuffers compiler, do not modify

# namespace: models

import flatbuffers
from flatbuffers.compat import import_numpy
np = import_numpy()

class HelloReply(object):
    __slots__ = ['_tab']

    @classmethod
    def GetRootAs(cls, buf, offset=0):
        n = flatbuffers.encode.Get(flatbuffers.packer.uoffset, buf, offset)
        x = HelloReply()
        x.Init(buf, n + offset)
        return x

    @classmethod
    def GetRootAsHelloReply(cls, buf, offset=0):
        """This method is deprecated. Please switch to GetRootAs."""
        return cls.GetRootAs(buf, offset)
    # HelloReply
    def Init(self, buf, pos):
        self._tab = flatbuffers.table.Table(buf, pos)

    # HelloReply
    def Message(self):
        o = flatbuffers.number_types.UOffsetTFlags.py_type(self._tab.Offset(4))
        if o != 0:
            return self._tab.String(o + self._tab.Pos)
        return None

def HelloReplyStart(builder):
    return builder.StartObject(1)

def Start(builder):
    return HelloReplyStart(builder)

def HelloReplyAddMessage(builder, message):
    return builder.PrependUOffsetTRelativeSlot(0, flatbuffers.number_types.UOffsetTFlags.py_type(message), 0)

def AddMessage(builder, message):
    return HelloReplyAddMessage(builder, message)

def HelloReplyEnd(builder):
    return builder.EndObject()

def End(builder):
    return HelloReplyEnd(builder)
