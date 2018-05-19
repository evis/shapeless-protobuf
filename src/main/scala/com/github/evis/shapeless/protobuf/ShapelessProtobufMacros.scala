package com.github.evis.shapeless.protobuf

import proto.test.LittleFile.MyMessage

import scala.reflect.macros.whitebox

class ShapelessProtobufMacros(val c: whitebox.Context) {
  import c.universe._

  def protobufProduct[T: WeakTypeTag, R: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    if (!(tpe <:< typeOf[MyMessage])) {
      sys.error("not MyMessage")
    }
    val clsName = TypeName(c.freshName("anon$"))
    q"""
      final class $clsName extends _root_.shapeless.Generic[$tpe] {
        type Repr = _root_.shapeless.::[String, _root_.shapeless.::[Int, _root_.shapeless.::[Boolean, _root_.shapeless.HNil]]]
        def to(p: $tpe): Repr = _root_.shapeless.::(p.getMyString, _root_.shapeless.::(p.getMyInt, _root_.shapeless.::(p.getMyBool, _root_.shapeless.HNil)))
        def from(p: Repr): $tpe = p match { case _root_.shapeless.::(s, _root_.shapeless.::(i, _root_.shapeless.::(b, _root_.shapeless.HNil))) => _root_.proto.test.LittleFile.MyMessage.newBuilder().setMyString(s).setMyInt(i).setMyBool(b).build() }
      }
      new $clsName(): _root_.shapeless.Generic.Aux[$tpe, _root_.shapeless.::[String, _root_.shapeless.::[Int, _root_.shapeless.::[Boolean, _root_.shapeless.HNil]]]]
    """
  }
}
