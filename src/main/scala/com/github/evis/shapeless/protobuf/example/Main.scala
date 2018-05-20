package com.github.evis.shapeless.protobuf.example

import proto.test.LittleFile.MyMessage
import shapeless.{::, Generic, HNil}
import com.github.evis.shapeless.protobuf._
import com.google.protobuf.ByteString
import org.scalacheck.Arbitrary
import org.scalacheck.ScalacheckShapeless._
import proto.test.LittleFile.MyMessage.{Inner, MyEnum}

object Main extends App {

  type Aux = Generic.Aux[MyMessage,
    String ::
    Int ::
    Boolean ::
    MyEnum ::
    ByteString ::
    Int ::
    ByteString ::
      (Long :: String :: HNil) ::
    HNil
  ]

  println {
    implicitly[Aux]
      .from(
        "hello" ::
          1234 ::
          true ::
          MyEnum.HER_VALUE ::
          ByteString.copyFrom("hello bytes", "UTF-8") ::
          9999 ::
          ByteString.copyFrom("hello my int bytes", "UTF-8") ::
          (56L :: "inner val" :: HNil) ::
          HNil
      )
  }

  println {
    implicitly[Aux]
      .to(
        MyMessage.newBuilder()
          .setMyString("hello")
          .setMyInt(1234)
          .setMyBool(true)
          .setMyEnum(MyEnum.HER_VALUE)
          .setMyBytes(ByteString.copyFrom("hello bytes", "UTF-8"))
          .setMyValue(9999)
          .setMyIntBytes(ByteString.copyFrom("hello my int bytes", "UTF-8"))
          .setInner(Inner.newBuilder().setLongValue(56L).setKeyValue("inner val"))
          .build()
      )
  }

  // TODO support MyString
  //println(implicitly[Arbitrary[MyMessage]].arbitrary.sample.get)
}
