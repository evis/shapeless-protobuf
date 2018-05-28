package com.github.evis.shapeless.protobuf

import shapeless.{::, HNil}

import scala.reflect.macros.whitebox
import scala.sys.error

private[protobuf] class ShapelessProtobufMacros(val c: whitebox.Context) {
  import c.universe._

  def protobufGeneric[T: WeakTypeTag, R: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    val repr = reprTypTree(tpe)
    val to = mkHListValue(tpe, fieldsOf(tpe), q"p")
    val (pattern, builder) = mkFrom(tpe)
    val from = cq" $pattern => $builder.build()"
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

  def mkHListValue(tpe: Type, fields: List[TermSymbol], prefix: Tree): Tree = {
    fields.foldRight(q"_root_.shapeless.HNil": Tree) { (field, acc) =>
      val fieldType = field.typeSignature.finalResultType
      if (isMsg(fieldType)) {
        q"_root_.shapeless.::(${mkHListValue(fieldType, fieldsOf(fieldType), q"$prefix.$field")}, $acc)"
      } else {
        q"_root_.shapeless.::($prefix.$field, $acc)"
      }
    }
  }

  def mkFrom(tpe: Type): (Tree, Tree) = {
    fieldsOf(tpe).foldRight((q"_root_.shapeless.HNil": Tree, q"${tpe.companion}.newBuilder()")) {
      case (field, (patternAcc, builderAcc)) =>
        val setter = TermName(field.name.toString.replaceFirst("get", "set"))
        val fieldType = field.typeSignature.resultType
        if (isMsg(fieldType)) {
          val (msgPattern, msgBuilder) = mkFrom(fieldType)
          val pattern = pq"_root_.shapeless.::(($msgPattern), $patternAcc)"
          val builder = q"$builderAcc.$setter($msgBuilder)"
          (pattern, builder)
        } else {
          val patName = TermName(c.freshName("pat"))
          val pattern = pq"_root_.shapeless.::($patName, $patternAcc)"
          val builder = q"$builderAcc.$setter($patName)"
          (pattern, builder)
        }
    }
  }

  /** Returns list of protobuf field getters for $tpe. */
  def fieldsOf(tpe: Type): List[TermSymbol] = {
    // protobuf fields info without lots of trash are contained in XXXOrBuilder interface
    // we filter methods like getDefaultInstanceForType(), getParserForType(), etc. from tpe class this way
    val allSyms = tpe.baseClasses.find(_.fullName == tpe.typeSymbol + "OrBuilder")
      .getOrElse(error(s"$tpe isn't protobuf type: ${tpe}OrBuilder type not found"))
      .asType.toType.decls.sorted.collect { case sym: TermSymbol => sym }
    // TODO refactor it
    allSyms.filterNot { sym =>
      val symName = sym.name.toString
      sym.typeSignature.finalResultType match {
        case tq"com.google.protobuf.ByteString" =>
          // ignore ByteString methods with Bytes suffix, generated for strings
          symName.endsWith("Bytes") &&
            allSyms.exists { sym =>
              sym.typeSignature.finalResultType.typeSymbol.fullName.toString == "java.lang.String" &&
                sym.name.toString == symName.replaceAll("Bytes$", "")
            }
        case tq"scala.Int" =>
          // ignore int methods with Value suffix, generated for enums
          symName.endsWith("Value") &&
            allSyms.exists { sym =>
              sym.typeSignature.finalResultType.typeSymbol.isJavaEnum &&
                sym.name.toString == symName.replaceAll("Value$", "")
            }
        case tq"scala.Boolean" =>
          // ignore methods for checking, if message contains inner message
          symName.startsWith("has")
        case _ =>
          symName.endsWith("OrBuilder") &&
            allSyms.exists { sym =>
              sym.name.toString == symName.replaceAll("OrBuilder$", "") &&
                isMsg(sym.typeSignature.finalResultType)
            }
      }
    }
  }

  def fieldsResultTypesOf(tpe: Type): List[Type] =
    fieldsOf(tpe).map(_.typeSignature.finalResultType)

  /** Returns true, if tpe is protobuf message; false otherwise. */
  def isMsg(tpe: Type): Boolean =
    tpe.baseClasses.contains(tq"com.google.protobuf.Message")

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
      case t if t.baseClasses.exists { _.fullName.toString == "com.google.protobuf.Message" } =>
        reprTypTree(t)
      case t => tq"$t"
    }
  }
}
