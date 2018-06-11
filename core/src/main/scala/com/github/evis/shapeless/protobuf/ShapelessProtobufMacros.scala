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
    val from = mkFrom(tpe)
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
  def optTpe: Type = typeOf[Option[_]].typeConstructor

  def reprTypTree(tpe: Type): Tree = {
    val fields = fieldsOf(tpe)
    mkCompoundTypTree(hnilTpe, hconsTpe, fields)
  }

  def mkHListValue(tpe: Type, fields: List[Field], prefix: Tree): Tree = {
    fields.foldRight(q"_root_.shapeless.HNil": Tree) { (field, acc) =>
      val fieldSym = field.symbol
      val fieldType = fieldSym.typeSignature.finalResultType
      val value = if (isMsg(fieldType)) {
        q"_root_.shapeless.Generic[$fieldType].to($prefix.$fieldSym)"
      } else {
        q"$prefix.$fieldSym"
      }
      val head = field.kind match {
        case Optional =>
          val hasField = TermName(fieldSym.name.toString.replaceFirst("^get", "has"))
          q"if ($prefix.$hasField) (_root_.scala.Some($value)) else _root_.scala.None"
        case Required =>
          value
      }
      q"_root_.shapeless.::($head, $acc)"
    }
  }

  def mkFrom(tpe: Type): Tree = {
    val b = TermName(c.freshName("b"))
    val (pattern, builder) = fieldsOf(tpe).foldRight((q"_root_.shapeless.HNil": Tree, q"val $b = ${tpe.companion}.newBuilder()": Tree)) {
      case (Field(symbol, kind), (patternAcc, builderAcc)) =>
        val setter = TermName(symbol.name.toString.replaceFirst("get", "set"))
        val fieldType = symbol.typeSignature.resultType
        val patName = TermName(c.freshName("pat"))
        val pattern = pq"_root_.shapeless.::($patName, $patternAcc)"
        val builder = kind match {
          case Optional =>
            if (isMsg(fieldType)) {
              q"..$builderAcc; $patName.foreach(_root_.shapeless.Generic[$fieldType].from _ andThen $b.$setter)"
            } else {
              q"..$builderAcc; $patName.foreach($b.$setter)"
            }
          case Required =>
            if (isMsg(fieldType)) {
              q"..$builderAcc; $b.$setter(_root_.shapeless.Generic[$fieldType].from($patName))"
            } else {
              q"..$builderAcc; $b.$setter($patName)"
            }
        }
        pattern -> builder
    }
    cq" $pattern => ..$builder; $b.build()"
  }

  /** Returns list of protobuf field getters for $tpe. */
  def fieldsOf(tpe: Type): List[Field] = {
    // protobuf fields info without lots of trash are contained in XXXOrBuilder interface
    // we filter methods like getDefaultInstanceForType(), getParserForType(), etc. from tpe class this way
    val allSyms = tpe.baseClasses.find {
      _.fullName match {
        case "com.google.protobuf.MessageOrBuilder" => false
        case name => name.endsWith("OrBuilder") // TODO check properly, with proto class name
      }
    }.getOrElse(error(s"$tpe isn't protobuf type: ${tpe}OrBuilder type not found"))
      .asType.toType.decls.sorted.collect { case sym: TermSymbol => sym }
    // TODO refactor it
    allSyms.filterNot { sym =>
      val symName = sym.name.toString
      sym.typeSignature.finalResultType.typeSymbol.fullName match {
        case "com.google.protobuf.ByteString" =>
          // ignore ByteString methods with Bytes suffix, generated for strings
          symName.endsWith("Bytes") &&
            allSyms.exists { sym =>
              sym.typeSignature.finalResultType.typeSymbol.fullName.toString == "java.lang.String" &&
                sym.name.toString == symName.replaceAll("Bytes$", "")
            }
        case "scala.Int" =>
          // ignore int methods with Value suffix, generated for enums
          symName.endsWith("Value") &&
            allSyms.exists { sym =>
              sym.typeSignature.finalResultType.typeSymbol.isJavaEnum &&
                sym.name.toString == symName.replaceAll("Value$", "")
            }
        case "scala.Boolean" =>
          // ignore methods for checking, if message contains inner message
          symName.startsWith("has")
        case _ =>
          symName.endsWith("OrBuilder") &&
            allSyms.exists { sym =>
              sym.name.toString == symName.replaceAll("OrBuilder$", "") &&
                isMsg(sym.typeSignature.finalResultType)
            }
      }
    }.map(Field(tpe, _))
  }

  /** Returns true, if tpe is protobuf message; false otherwise. */
  def isMsg(tpe: Type): Boolean = {
    tpe.baseClasses.exists {
      _.fullName.toString == "com.google.protobuf.Message"
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

  def mkCompoundTypTree(nil: Type, cons: Type, items: List[Field]): Tree = {
    // TODO review this! just copy-pasted from shapeless
    items.foldRight(mkAttributedRef(nil): Tree) { case (Field(symbol, kind), acc) =>
      val tpe = symbol.typeSignature.finalResultType
      kind match {
        case Optional =>
          val optTypTree = mkOptionTypTree(optTpe, tpe)
          AppliedTypeTree(mkAttributedRef(cons), List(optTypTree, acc))
        case Required =>
          AppliedTypeTree(mkAttributedRef(cons), List(mkTypTree(tpe), acc))
      }
    }
  }

  def mkOptionTypTree(opt: Type, value: Type): Tree = {
    AppliedTypeTree(mkAttributedRef(opt), List(mkTypTree(value)))
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

  sealed trait FieldKind
  final case object Required extends FieldKind
  final case object Optional extends FieldKind

  final case class Field(symbol: TermSymbol, kind: FieldKind)

  final object Field {

    def apply(tpe: Type, symbol: TermSymbol): Field = {
      val kind = {
        if (isOptional(tpe, symbol)) {
          Optional
        } else {
          Required
        }
      }
      Field(symbol, kind)
    }

    private def isOptional(tpe: Type, field: TermSymbol): Boolean =
      hasMethod(tpe, field.name.toString.replaceFirst("^get", "has"))

    private def hasMethod(tpe: Type, method: String): Boolean = {
      tpe.decls.sorted.collect { case sym: TermSymbol => sym }
        .exists(_.name.toString == method)
    }
  }
}
