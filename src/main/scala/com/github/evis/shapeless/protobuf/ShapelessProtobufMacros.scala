package com.github.evis.shapeless.protobuf

import shapeless.{::, HNil}

import scala.reflect.macros.whitebox

class ShapelessProtobufMacros(val c: whitebox.Context) {
  import c.universe._

  def protobufProduct[T: WeakTypeTag, R: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    val clsName = TypeName(c.freshName("anon$"))
    q"""
      final class $clsName extends _root_.shapeless.Generic[$tpe] {
        type Repr = ${reprTypTree(tpe)}
        def to(p: $tpe): Repr = _root_.shapeless.::(p.getMyString, _root_.shapeless.::(p.getMyInt, _root_.shapeless.::(p.getMyBool, _root_.shapeless.HNil)))
        def from(p: Repr): $tpe = p match { case _root_.shapeless.::(s, _root_.shapeless.::(i, _root_.shapeless.::(b, _root_.shapeless.HNil))) => _root_.proto.test.LittleFile.MyMessage.newBuilder().setMyString(s).setMyInt(i).setMyBool(b).build() }
      }
      new $clsName(): _root_.shapeless.Generic.Aux[$tpe, ${reprTypTree(tpe)}]
    """
  }

  def hnilTpe = typeOf[HNil]
  def hconsTpe = typeOf[::[_, _]].typeConstructor

  def reprTypTree(tpe: Type): Tree = {
    val fields = fieldsOf(tpe)
    mkCompoundTypTree(hnilTpe, hconsTpe, fields)
  }

  def fieldsOf(tpe: Type): List[Type] = {
    val tpeInterface = tpe.baseClasses.filter {
      _.fullName match {
        case "com.google.protobuf.MessageOrBuilder" => false
        case name => name.endsWith("OrBuilder")
      }
    }.head.asType.toType // TODO head may throw exception
    tpeInterface.decls.sorted.collect {
      case sym: TermSymbol =>
        sym.typeSignatureIn(tpe).finalResultType
    }.filter {
      _.typeSymbol.fullName match {
        case "com.google.protobuf.ByteString" => false
        case _ => true
      }
    }
  }

  def mkAttributedRef(tpe: Type): Tree = {
    // TODO review this! just copy-pasted from shapeless
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    val pre = gTpe.prefix
    val sym = gTpe.typeSymbol
    global.gen.mkAttributedRef(pre, sym).asInstanceOf[Tree]
  }

  def mkAttributedRef(pre: Type, sym: Symbol): Tree = {
    // TODO review this! just copy-pasted from shapeless
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gPre = pre.asInstanceOf[global.Type]
    val gSym = sym.asInstanceOf[global.Symbol]
    global.gen.mkAttributedRef(gPre, gSym).asInstanceOf[Tree]
  }

  def mkCompoundTypTree(nil: Type, cons: Type, items: List[Type]): Tree = {
    // TODO review this! just copy-pasted from shapeless
    items.foldRight(mkAttributedRef(nil): Tree) { case (tpe, acc) =>
      AppliedTypeTree(mkAttributedRef(cons), List(mkTypTree(tpe), acc))
    }
  }

  def mkTypTree(tpe: Type): Tree = {
    // TODO review this! just copy-pasted from shapeless with a bit of change
    tpe match {
      case SingleType(pre @ SingleType(_, _), sym) =>
        SingletonTypeTree(mkAttributedRef(pre, sym))
      case t => tq"$t"
    }
  }
}
