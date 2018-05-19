package com.github.evis.shapeless.protobuf.example

import proto.test.LittleFile.MyMessage
import shapeless.{Generic, HNil, ::}
import com.github.evis.shapeless.protobuf._

object Main extends App {

  implicitly[Generic[MyMessage]].from("1234" :: 1234 :: HNil)
}
