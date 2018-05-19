package com.github.evis.shapeless.protobuf.example

import proto.test.LittleFile.MyMessage
import shapeless.{Generic, HNil, ::}
import com.github.evis.shapeless.protobuf._

object Main extends App {

  println {
    implicitly[Generic.Aux[MyMessage, String :: Int :: Boolean :: HNil]].from("hello" :: 1234 :: true :: HNil)
  }
}
