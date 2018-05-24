package com.github.evis.shapeless

import shapeless.Generic

import scala.language.experimental.macros

package object protobuf {

  implicit def protobufGeneric[T, R]: Generic.Aux[T, R] = macro ShapelessProtobufMacros.protobufGeneric[T, R]
}
