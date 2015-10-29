package app.protobuf.serializing

trait Serializing extends BaseProto with GameProto with MessagesProto with ControlProto with BgClientProto

object Serializing extends Serializing
