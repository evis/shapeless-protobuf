package com.github.evis.shapeless.protobuf.example

import proto.test.LittleFile.MyMessage
import shapeless.{::, Generic, HNil}
import com.github.evis.shapeless.protobuf._
import com.google.protobuf.ByteString
import org.scalacheck.Arbitrary
import org.scalacheck.ScalacheckShapeless._
import proto.test.LittleFile.MyMessage.MyEnum

object Main extends App {

  println {
    implicitly[Generic.Aux[MyMessage, String :: Int :: Boolean :: MyEnum :: ByteString :: Int :: HNil]].from("hello" :: 1234 :: true :: MyEnum.HER_VALUE :: ByteString.copyFrom("hello bytes", "UTF-8") :: 9999 :: HNil)
  }

  println {
    implicitly[Generic.Aux[MyMessage, String :: Int :: Boolean :: MyEnum :: ByteString :: Int :: HNil]].to(MyMessage.newBuilder().setMyString("hello").setMyInt(1234).setMyBool(true).setMyEnum(MyEnum.HER_VALUE).setMyBytes(ByteString.copyFrom("hello bytes", "UTF-8")).setMyValue(9999).build())
  }

  println(implicitly[Arbitrary[MyMessage]].arbitrary.sample.get)
}
