package com.github.evis.shapeless.protobuf

import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalacheck.Properties
import org.scalacheck.ScalacheckShapeless._
import com.github.evis.shapeless.protobuf.TestMessages._
import shapeless.{::, Generic, HNil}

object ProtobufGenericSpec extends Properties("ProtobufGeneric") {

  property("convert primitives to generic") = {
    forAll { p: Primitives =>
      val repr = implicitly[Generic.Aux[Primitives, String :: Int :: Long :: Boolean :: HNil]].to(p)
      repr == p.getTestString :: p.getTestInt :: p.getTestLong :: p.getTestBool :: HNil
    }
  }

  property("convert generic to primitives") = {
    forAll { repr: (String :: Int :: Long :: Boolean :: HNil) =>
      val p = implicitly[Generic.Aux[Primitives, String :: Int :: Long :: Boolean :: HNil]].from(repr)
      repr match {
        case a :: b :: c :: d :: HNil =>
          p == Primitives.newBuilder().setTestString(a).setTestInt(b).setTestLong(c).setTestBool(d).build()
      }
    }
  }

  property("convert with enum to generic") = {
    forAll { (s: String, e: TestEnum) =>
      // TODO filter unrecognized automatically for protobuf enums
      e != TestEnum.UNRECOGNIZED ==> {
        val p = WithEnum.newBuilder().setTestString(s).setTestEnum(e).build()
        val repr = implicitly[Generic.Aux[WithEnum, String :: TestEnum :: HNil]].to(p)
        repr == p.getTestString :: p.getTestEnum :: HNil
      }
    }
  }
}
