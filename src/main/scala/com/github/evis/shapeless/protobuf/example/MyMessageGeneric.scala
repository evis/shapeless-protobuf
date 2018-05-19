package com.github.evis.shapeless.protobuf.example

import proto.test.LittleFile.MyMessage
import shapeless.{::, Generic, HNil}

object MyMessageGeneric {

  implicit val myMessageGeneric: Generic.Aux[MyMessage, String :: Int :: Boolean :: HNil] =
    new Generic[MyMessage] {

      override type Repr = String :: Int :: Boolean :: HNil

      override def to(t: MyMessage): Repr = t.getMyString :: t.getMyInt :: t.getMyBool :: HNil

      override def from(r: Repr): MyMessage = r match {
        case s :: i :: b :: HNil =>
          MyMessage.newBuilder().setMyString(s).setMyInt(i).setMyBool(b).build()
      }
    }

  implicitly[Generic[MyMessage]]
}
