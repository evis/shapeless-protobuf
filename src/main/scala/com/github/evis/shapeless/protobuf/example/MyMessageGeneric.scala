package com.github.evis.shapeless.protobuf.example

import com.google.protobuf.ByteString
import proto.test.LittleFile.MyMessage
import proto.test.LittleFile.MyMessage.{Inner, MyEnum}
import shapeless.{::, Generic, HNil}

object MyMessageGeneric {

  implicit val myMessageGeneric: Generic.Aux[MyMessage, String :: Int :: Boolean :: MyEnum :: ByteString :: Int :: ByteString :: (Long :: String :: Long :: HNil) :: HNil] =
    new Generic[MyMessage] {

      override type Repr = String :: Int :: Boolean :: MyEnum :: ByteString :: Int :: ByteString :: (Long :: String :: Long :: HNil) :: HNil

      override def to(t: MyMessage): Repr = {
        t.getMyString ::
        t.getMyInt ::
        t.getMyBool ::
        t.getMyEnum ::
        t.getMyBytes ::
        t.getMyValue ::
        t.getMyIntBytes ::
          (t.getInner.getLongValue :: t.getInner.getKeyValue :: t.getInner.getSomethingOrBuilder :: HNil) ::
        HNil
      }

      override def from(r: Repr): MyMessage = r match {
        case myString :: myInt :: myBool :: myEnum :: myBytes :: myValue :: myIntBytes ::
          (longValue :: keyValue :: somethingOrBuilder :: HNil) :: HNil =>
          MyMessage.newBuilder()
            .setMyString(myString)
            .setMyInt(myInt)
            .setMyBool(myBool)
            .setMyEnum(myEnum)
            .setMyBytes(myBytes)
            .setMyValue(myValue)
            .setMyIntBytes(myIntBytes)
            .setInner(
              Inner.newBuilder()
                .setLongValue(longValue)
                .setKeyValue(keyValue)
                .setSomethingOrBuilder(somethingOrBuilder)
            ).build()
      }
    }

  implicitly[Generic[MyMessage]]
}
