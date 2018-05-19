package com.github.evis.shapeless.protobuf

import scala.reflect.macros.whitebox

class ShapelessProtobufMacros(val c: whitebox.Context) {
  import c.universe._

  def protobufProduct[T: WeakTypeTag, R: WeakTypeTag]: Tree = {
    ???
  }
}
