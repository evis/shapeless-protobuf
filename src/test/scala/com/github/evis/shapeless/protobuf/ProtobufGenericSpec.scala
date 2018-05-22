package com.github.evis.shapeless.protobuf

import com.github.evis.shapeless.protobuf.TestMessages._
import com.google.protobuf.ByteString
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties
import org.scalacheck.ScalacheckShapeless._
import shapeless.ops.product.ToHList
import shapeless.{::, Generic, HList, HNil}

object ProtobufGenericSpec extends Properties("ProtobufGeneric") {

  implicit class HListToOps[R <: HList](val hlist: R) extends AnyVal {

    def to[T](implicit gen: Generic.Aux[T, R]): T = gen.from(hlist)
  }

  implicit class ToHListOps[T](val from: T) extends AnyVal {

    def toHList(implicit toHList: ToHList[T]): toHList.Out = toHList(from)
  }

  property("convert primitives to generic") = {
    forAll { p: Primitives =>
      p.toHList == p.getTestString :: p.getTestInt :: p.getTestLong :: p.getTestBool :: HNil
    }
  }

  property("convert generic to primitives") = {
    forAll { repr: (String :: Int :: Long :: Boolean :: HNil) =>
      val p = repr.to[Primitives]
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
        p.toHList == s :: e :: HNil
      }
    }
  }

  property("convert generic to with enum") = {
    forAll { (s: String, e: TestEnum) =>
      // TODO filter unrecognized automatically for protobuf enums
      e != TestEnum.UNRECOGNIZED ==> {
        val repr = s :: e :: HNil
        val p = repr.to[WithEnum]
        p == WithEnum.newBuilder().setTestString(s).setTestEnum(e).build()
      }
    }
  }

  property("convert with bytes to generic") = {
    // TODO arbitrary for ByteString
    forAll { (s: String, b: Array[Byte]) =>
      val bs = ByteString.copyFrom(b)
      val p = WithBytes.newBuilder().setTestString(s).setTestBytes(bs).build()
      p.toHList == s :: bs :: HNil
    }
  }

  property("convert generic to with bytes") = {
    // TODO arbitrary for ByteString
    forAll { (s: String, b: Array[Byte]) =>
      val bs = ByteString.copyFrom(b)
      val repr = s :: bs :: HNil
      val p = repr.to[WithBytes]
      p == WithBytes.newBuilder().setTestString(s).setTestBytes(bs).build()
    }
  }
}
