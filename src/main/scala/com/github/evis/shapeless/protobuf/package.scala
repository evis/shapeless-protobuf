package com.github.evis.shapeless

import shapeless.Generic

import scala.language.experimental.macros

package object protobuf {

  implicit def protobufProduct[T, R]: Generic.Aux[T, R] = macro ShapelessProtobufMacros.protobufProduct[T, R]
}
