package com.github.evis.shapeless.protobuf

import shapeless.{::, HNil}

import scala.reflect.macros.whitebox

private[protobuf] class ShapelessProtobufMacros(val c: whitebox.Context) {
  import c.universe._

  def protobufProduct[T: WeakTypeTag, R: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    val repr = reprTypTree(tpe)
    val to = mkHListValue(fieldsOf(tpe))
    val from = mkFrom(tpe, fieldsOf(tpe))
    val clsName = TypeName(c.freshName("anon$"))
    q"""
      final class $clsName extends _root_.shapeless.Generic[$tpe] {
        type Repr = $repr
        def to(p: $tpe): Repr = $to
        def from(p: Repr): $tpe = p match { case $from }
      }
      new $clsName(): _root_.shapeless.Generic.Aux[$tpe, $repr]
    """
  }

  def hnilTpe: Type = typeOf[HNil]
  def hconsTpe: Type = typeOf[::[_, _]].typeConstructor

  def reprTypTree(tpe: Type): Tree = {
    val fields = fieldsResultTypesOf(tpe)
    mkCompoundTypTree(hnilTpe, hconsTpe, fields)
  }

  def mkHListValue(fields: List[TermSymbol]): Tree = {
    fields.foldRight(q"_root_.shapeless.HNil": Tree) { (field, acc) =>
      q"_root_.shapeless.::(p.$field, $acc)"
    }
  }

  def mkFrom(tpe: Type, fields: List[TermSymbol]): Tree = {
    val bindings = fields.map { _ => TermName(c.freshName("pat")) }
    val pattern = bindings.foldRight(q"_root_.shapeless.HNil": Tree) { (field, acc) =>
      pq"_root_.shapeless.::($field, $acc)"
    }
    val builder = fields.zip(bindings).foldLeft(q"${tpe.companion}.newBuilder()": Tree) {
      case (acc, (field, binding)) =>
        val setterName = field.name.toString.replaceFirst("get", "set")
        val setter = TermName(setterName)
        q"$acc.$setter($binding)"
    }
    cq" $pattern => $builder.build()"
  }

  def fieldsOf(tpe: Type): List[TermSymbol] = {
    val allSyms = tpe.baseClasses.find {
      _.fullName match {
        case "com.google.protobuf.MessageOrBuilder" => false
        case name => name.endsWith("OrBuilder")
      }
    }.getOrElse(sys.error(s"$tpe isn't protobuf type: ${tpe}OrBuilder type not found"))
      .asType.toType.decls.sorted.collect { case sym: TermSymbol => sym }
    // TODO refactor it
    allSyms.filterNot { sym =>
      val symName = sym.name.toString
      sym.typeSignatureIn(tpe).finalResultType.typeSymbol.fullName match {
        case "com.google.protobuf.ByteString" =>
          // ignore ByteString methods with Bytes suffix, generated for strings
          symName.endsWith("Bytes") &&
            allSyms.exists { sym =>
              sym.typeSignatureIn(tpe).finalResultType.typeSymbol.fullName.toString == "java.lang.String" &&
                sym.name.toString == symName.replaceAll("Bytes$", "")
            }
        case "scala.Int" =>
          // ignore int methods with Value suffix, generated for enums
          symName.endsWith("Value") &&
            allSyms.exists { sym =>
              sym.typeSignatureIn(tpe).finalResultType.typeSymbol.isJavaEnum &&
                sym.name.toString == symName.replaceAll("Value$", "")
            }
        case "scala.Boolean" =>
          // ignore methods for checking, if message contains inner message
          symName.startsWith("has")
        case _ =>
          symName.endsWith("OrBuilder") &&
            allSyms.exists { sym =>
              sym.name.toString == symName.replaceAll("OrBuilder$", "") &&
                sym.typeSignatureIn(tpe).finalResultType.baseClasses.exists {
                  _.fullName.toString == "com.google.protobuf.Message"
                }
            }
      }
    }
  }

  def fieldsResultTypesOf(tpe: Type): List[Type] =
    fieldsOf(tpe).map(_.typeSignatureIn(tpe).finalResultType)

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
