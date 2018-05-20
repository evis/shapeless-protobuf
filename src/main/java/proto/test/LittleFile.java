// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: vertis/little_file.proto

package proto.test;

public final class LittleFile {
  private LittleFile() {}
  public static void registerAllExtensions(
      com.google.protobuf.ExtensionRegistryLite registry) {
  }

  public static void registerAllExtensions(
      com.google.protobuf.ExtensionRegistry registry) {
    registerAllExtensions(
        (com.google.protobuf.ExtensionRegistryLite) registry);
  }
  public interface MyMessageOrBuilder extends
      // @@protoc_insertion_point(interface_extends:vertis.MyMessage)
      com.google.protobuf.MessageOrBuilder {

    /**
     * <code>string my_string = 1;</code>
     */
    java.lang.String getMyString();
    /**
     * <code>string my_string = 1;</code>
     */
    com.google.protobuf.ByteString
        getMyStringBytes();

    /**
     * <code>int32 my_int = 2;</code>
     */
    int getMyInt();

    /**
     * <code>bool my_bool = 3;</code>
     */
    boolean getMyBool();

    /**
     * <code>.vertis.MyMessage.MyEnum my_enum = 4;</code>
     */
    int getMyEnumValue();
    /**
     * <code>.vertis.MyMessage.MyEnum my_enum = 4;</code>
     */
    proto.test.LittleFile.MyMessage.MyEnum getMyEnum();

    /**
     * <code>bytes my_bytes = 5;</code>
     */
    com.google.protobuf.ByteString getMyBytes();

    /**
     * <code>int32 my_value = 6;</code>
     */
    int getMyValue();

    /**
     * <code>bytes my_int_bytes = 7;</code>
     */
    com.google.protobuf.ByteString getMyIntBytes();
  }
  /**
   * Protobuf type {@code vertis.MyMessage}
   */
  public  static final class MyMessage extends
      com.google.protobuf.GeneratedMessageV3 implements
      // @@protoc_insertion_point(message_implements:vertis.MyMessage)
      MyMessageOrBuilder {
  private static final long serialVersionUID = 0L;
    // Use MyMessage.newBuilder() to construct.
    private MyMessage(com.google.protobuf.GeneratedMessageV3.Builder<?> builder) {
      super(builder);
    }
    private MyMessage() {
      myString_ = "";
      myInt_ = 0;
      myBool_ = false;
      myEnum_ = 0;
      myBytes_ = com.google.protobuf.ByteString.EMPTY;
      myValue_ = 0;
      myIntBytes_ = com.google.protobuf.ByteString.EMPTY;
    }

    @java.lang.Override
    public final com.google.protobuf.UnknownFieldSet
    getUnknownFields() {
      return this.unknownFields;
    }
    private MyMessage(
        com.google.protobuf.CodedInputStream input,
        com.google.protobuf.ExtensionRegistryLite extensionRegistry)
        throws com.google.protobuf.InvalidProtocolBufferException {
      this();
      if (extensionRegistry == null) {
        throw new java.lang.NullPointerException();
      }
      int mutable_bitField0_ = 0;
      com.google.protobuf.UnknownFieldSet.Builder unknownFields =
          com.google.protobuf.UnknownFieldSet.newBuilder();
      try {
        boolean done = false;
        while (!done) {
          int tag = input.readTag();
          switch (tag) {
            case 0:
              done = true;
              break;
            default: {
              if (!parseUnknownFieldProto3(
                  input, unknownFields, extensionRegistry, tag)) {
                done = true;
              }
              break;
            }
            case 10: {
              java.lang.String s = input.readStringRequireUtf8();

              myString_ = s;
              break;
            }
            case 16: {

              myInt_ = input.readInt32();
              break;
            }
            case 24: {

              myBool_ = input.readBool();
              break;
            }
            case 32: {
              int rawValue = input.readEnum();

              myEnum_ = rawValue;
              break;
            }
            case 42: {

              myBytes_ = input.readBytes();
              break;
            }
            case 48: {

              myValue_ = input.readInt32();
              break;
            }
            case 58: {

              myIntBytes_ = input.readBytes();
              break;
            }
          }
        }
      } catch (com.google.protobuf.InvalidProtocolBufferException e) {
        throw e.setUnfinishedMessage(this);
      } catch (java.io.IOException e) {
        throw new com.google.protobuf.InvalidProtocolBufferException(
            e).setUnfinishedMessage(this);
      } finally {
        this.unknownFields = unknownFields.build();
        makeExtensionsImmutable();
      }
    }
    public static final com.google.protobuf.Descriptors.Descriptor
        getDescriptor() {
      return proto.test.LittleFile.internal_static_vertis_MyMessage_descriptor;
    }

    protected com.google.protobuf.GeneratedMessageV3.FieldAccessorTable
        internalGetFieldAccessorTable() {
      return proto.test.LittleFile.internal_static_vertis_MyMessage_fieldAccessorTable
          .ensureFieldAccessorsInitialized(
              proto.test.LittleFile.MyMessage.class, proto.test.LittleFile.MyMessage.Builder.class);
    }

    /**
     * Protobuf enum {@code vertis.MyMessage.MyEnum}
     */
    public enum MyEnum
        implements com.google.protobuf.ProtocolMessageEnum {
      /**
       * <code>DEFAULT = 0;</code>
       */
      DEFAULT(0),
      /**
       * <code>MY_VALUE = 1;</code>
       */
      MY_VALUE(1),
      /**
       * <code>HIS_VALUE = 2;</code>
       */
      HIS_VALUE(2),
      /**
       * <code>HER_VALUE = 3;</code>
       */
      HER_VALUE(3),
      UNRECOGNIZED(-1),
      ;

      /**
       * <code>DEFAULT = 0;</code>
       */
      public static final int DEFAULT_VALUE = 0;
      /**
       * <code>MY_VALUE = 1;</code>
       */
      public static final int MY_VALUE_VALUE = 1;
      /**
       * <code>HIS_VALUE = 2;</code>
       */
      public static final int HIS_VALUE_VALUE = 2;
      /**
       * <code>HER_VALUE = 3;</code>
       */
      public static final int HER_VALUE_VALUE = 3;


      public final int getNumber() {
        if (this == UNRECOGNIZED) {
          throw new java.lang.IllegalArgumentException(
              "Can't get the number of an unknown enum value.");
        }
        return value;
      }

      /**
       * @deprecated Use {@link #forNumber(int)} instead.
       */
      @java.lang.Deprecated
      public static MyEnum valueOf(int value) {
        return forNumber(value);
      }

      public static MyEnum forNumber(int value) {
        switch (value) {
          case 0: return DEFAULT;
          case 1: return MY_VALUE;
          case 2: return HIS_VALUE;
          case 3: return HER_VALUE;
          default: return null;
        }
      }

      public static com.google.protobuf.Internal.EnumLiteMap<MyEnum>
          internalGetValueMap() {
        return internalValueMap;
      }
      private static final com.google.protobuf.Internal.EnumLiteMap<
          MyEnum> internalValueMap =
            new com.google.protobuf.Internal.EnumLiteMap<MyEnum>() {
              public MyEnum findValueByNumber(int number) {
                return MyEnum.forNumber(number);
              }
            };

      public final com.google.protobuf.Descriptors.EnumValueDescriptor
          getValueDescriptor() {
        return getDescriptor().getValues().get(ordinal());
      }
      public final com.google.protobuf.Descriptors.EnumDescriptor
          getDescriptorForType() {
        return getDescriptor();
      }
      public static final com.google.protobuf.Descriptors.EnumDescriptor
          getDescriptor() {
        return proto.test.LittleFile.MyMessage.getDescriptor().getEnumTypes().get(0);
      }

      private static final MyEnum[] VALUES = values();

      public static MyEnum valueOf(
          com.google.protobuf.Descriptors.EnumValueDescriptor desc) {
        if (desc.getType() != getDescriptor()) {
          throw new java.lang.IllegalArgumentException(
            "EnumValueDescriptor is not for this type.");
        }
        if (desc.getIndex() == -1) {
          return UNRECOGNIZED;
        }
        return VALUES[desc.getIndex()];
      }

      private final int value;

      private MyEnum(int value) {
        this.value = value;
      }

      // @@protoc_insertion_point(enum_scope:vertis.MyMessage.MyEnum)
    }

    public static final int MY_STRING_FIELD_NUMBER = 1;
    private volatile java.lang.Object myString_;
    /**
     * <code>string my_string = 1;</code>
     */
    public java.lang.String getMyString() {
      java.lang.Object ref = myString_;
      if (ref instanceof java.lang.String) {
        return (java.lang.String) ref;
      } else {
        com.google.protobuf.ByteString bs = 
            (com.google.protobuf.ByteString) ref;
        java.lang.String s = bs.toStringUtf8();
        myString_ = s;
        return s;
      }
    }
    /**
     * <code>string my_string = 1;</code>
     */
    public com.google.protobuf.ByteString
        getMyStringBytes() {
      java.lang.Object ref = myString_;
      if (ref instanceof java.lang.String) {
        com.google.protobuf.ByteString b = 
            com.google.protobuf.ByteString.copyFromUtf8(
                (java.lang.String) ref);
        myString_ = b;
        return b;
      } else {
        return (com.google.protobuf.ByteString) ref;
      }
    }

    public static final int MY_INT_FIELD_NUMBER = 2;
    private int myInt_;
    /**
     * <code>int32 my_int = 2;</code>
     */
    public int getMyInt() {
      return myInt_;
    }

    public static final int MY_BOOL_FIELD_NUMBER = 3;
    private boolean myBool_;
    /**
     * <code>bool my_bool = 3;</code>
     */
    public boolean getMyBool() {
      return myBool_;
    }

    public static final int MY_ENUM_FIELD_NUMBER = 4;
    private int myEnum_;
    /**
     * <code>.vertis.MyMessage.MyEnum my_enum = 4;</code>
     */
    public int getMyEnumValue() {
      return myEnum_;
    }
    /**
     * <code>.vertis.MyMessage.MyEnum my_enum = 4;</code>
     */
    public proto.test.LittleFile.MyMessage.MyEnum getMyEnum() {
      proto.test.LittleFile.MyMessage.MyEnum result = proto.test.LittleFile.MyMessage.MyEnum.valueOf(myEnum_);
      return result == null ? proto.test.LittleFile.MyMessage.MyEnum.UNRECOGNIZED : result;
    }

    public static final int MY_BYTES_FIELD_NUMBER = 5;
    private com.google.protobuf.ByteString myBytes_;
    /**
     * <code>bytes my_bytes = 5;</code>
     */
    public com.google.protobuf.ByteString getMyBytes() {
      return myBytes_;
    }

    public static final int MY_VALUE_FIELD_NUMBER = 6;
    private int myValue_;
    /**
     * <code>int32 my_value = 6;</code>
     */
    public int getMyValue() {
      return myValue_;
    }

    public static final int MY_INT_BYTES_FIELD_NUMBER = 7;
    private com.google.protobuf.ByteString myIntBytes_;
    /**
     * <code>bytes my_int_bytes = 7;</code>
     */
    public com.google.protobuf.ByteString getMyIntBytes() {
      return myIntBytes_;
    }

    private byte memoizedIsInitialized = -1;
    public final boolean isInitialized() {
      byte isInitialized = memoizedIsInitialized;
      if (isInitialized == 1) return true;
      if (isInitialized == 0) return false;

      memoizedIsInitialized = 1;
      return true;
    }

    public void writeTo(com.google.protobuf.CodedOutputStream output)
                        throws java.io.IOException {
      if (!getMyStringBytes().isEmpty()) {
        com.google.protobuf.GeneratedMessageV3.writeString(output, 1, myString_);
      }
      if (myInt_ != 0) {
        output.writeInt32(2, myInt_);
      }
      if (myBool_ != false) {
        output.writeBool(3, myBool_);
      }
      if (myEnum_ != proto.test.LittleFile.MyMessage.MyEnum.DEFAULT.getNumber()) {
        output.writeEnum(4, myEnum_);
      }
      if (!myBytes_.isEmpty()) {
        output.writeBytes(5, myBytes_);
      }
      if (myValue_ != 0) {
        output.writeInt32(6, myValue_);
      }
      if (!myIntBytes_.isEmpty()) {
        output.writeBytes(7, myIntBytes_);
      }
      unknownFields.writeTo(output);
    }

    public int getSerializedSize() {
      int size = memoizedSize;
      if (size != -1) return size;

      size = 0;
      if (!getMyStringBytes().isEmpty()) {
        size += com.google.protobuf.GeneratedMessageV3.computeStringSize(1, myString_);
      }
      if (myInt_ != 0) {
        size += com.google.protobuf.CodedOutputStream
          .computeInt32Size(2, myInt_);
      }
      if (myBool_ != false) {
        size += com.google.protobuf.CodedOutputStream
          .computeBoolSize(3, myBool_);
      }
      if (myEnum_ != proto.test.LittleFile.MyMessage.MyEnum.DEFAULT.getNumber()) {
        size += com.google.protobuf.CodedOutputStream
          .computeEnumSize(4, myEnum_);
      }
      if (!myBytes_.isEmpty()) {
        size += com.google.protobuf.CodedOutputStream
          .computeBytesSize(5, myBytes_);
      }
      if (myValue_ != 0) {
        size += com.google.protobuf.CodedOutputStream
          .computeInt32Size(6, myValue_);
      }
      if (!myIntBytes_.isEmpty()) {
        size += com.google.protobuf.CodedOutputStream
          .computeBytesSize(7, myIntBytes_);
      }
      size += unknownFields.getSerializedSize();
      memoizedSize = size;
      return size;
    }

    @java.lang.Override
    public boolean equals(final java.lang.Object obj) {
      if (obj == this) {
       return true;
      }
      if (!(obj instanceof proto.test.LittleFile.MyMessage)) {
        return super.equals(obj);
      }
      proto.test.LittleFile.MyMessage other = (proto.test.LittleFile.MyMessage) obj;

      boolean result = true;
      result = result && getMyString()
          .equals(other.getMyString());
      result = result && (getMyInt()
          == other.getMyInt());
      result = result && (getMyBool()
          == other.getMyBool());
      result = result && myEnum_ == other.myEnum_;
      result = result && getMyBytes()
          .equals(other.getMyBytes());
      result = result && (getMyValue()
          == other.getMyValue());
      result = result && getMyIntBytes()
          .equals(other.getMyIntBytes());
      result = result && unknownFields.equals(other.unknownFields);
      return result;
    }

    @java.lang.Override
    public int hashCode() {
      if (memoizedHashCode != 0) {
        return memoizedHashCode;
      }
      int hash = 41;
      hash = (19 * hash) + getDescriptor().hashCode();
      hash = (37 * hash) + MY_STRING_FIELD_NUMBER;
      hash = (53 * hash) + getMyString().hashCode();
      hash = (37 * hash) + MY_INT_FIELD_NUMBER;
      hash = (53 * hash) + getMyInt();
      hash = (37 * hash) + MY_BOOL_FIELD_NUMBER;
      hash = (53 * hash) + com.google.protobuf.Internal.hashBoolean(
          getMyBool());
      hash = (37 * hash) + MY_ENUM_FIELD_NUMBER;
      hash = (53 * hash) + myEnum_;
      hash = (37 * hash) + MY_BYTES_FIELD_NUMBER;
      hash = (53 * hash) + getMyBytes().hashCode();
      hash = (37 * hash) + MY_VALUE_FIELD_NUMBER;
      hash = (53 * hash) + getMyValue();
      hash = (37 * hash) + MY_INT_BYTES_FIELD_NUMBER;
      hash = (53 * hash) + getMyIntBytes().hashCode();
      hash = (29 * hash) + unknownFields.hashCode();
      memoizedHashCode = hash;
      return hash;
    }

    public static proto.test.LittleFile.MyMessage parseFrom(
        java.nio.ByteBuffer data)
        throws com.google.protobuf.InvalidProtocolBufferException {
      return PARSER.parseFrom(data);
    }
    public static proto.test.LittleFile.MyMessage parseFrom(
        java.nio.ByteBuffer data,
        com.google.protobuf.ExtensionRegistryLite extensionRegistry)
        throws com.google.protobuf.InvalidProtocolBufferException {
      return PARSER.parseFrom(data, extensionRegistry);
    }
    public static proto.test.LittleFile.MyMessage parseFrom(
        com.google.protobuf.ByteString data)
        throws com.google.protobuf.InvalidProtocolBufferException {
      return PARSER.parseFrom(data);
    }
    public static proto.test.LittleFile.MyMessage parseFrom(
        com.google.protobuf.ByteString data,
        com.google.protobuf.ExtensionRegistryLite extensionRegistry)
        throws com.google.protobuf.InvalidProtocolBufferException {
      return PARSER.parseFrom(data, extensionRegistry);
    }
    public static proto.test.LittleFile.MyMessage parseFrom(byte[] data)
        throws com.google.protobuf.InvalidProtocolBufferException {
      return PARSER.parseFrom(data);
    }
    public static proto.test.LittleFile.MyMessage parseFrom(
        byte[] data,
        com.google.protobuf.ExtensionRegistryLite extensionRegistry)
        throws com.google.protobuf.InvalidProtocolBufferException {
      return PARSER.parseFrom(data, extensionRegistry);
    }
    public static proto.test.LittleFile.MyMessage parseFrom(java.io.InputStream input)
        throws java.io.IOException {
      return com.google.protobuf.GeneratedMessageV3
          .parseWithIOException(PARSER, input);
    }
    public static proto.test.LittleFile.MyMessage parseFrom(
        java.io.InputStream input,
        com.google.protobuf.ExtensionRegistryLite extensionRegistry)
        throws java.io.IOException {
      return com.google.protobuf.GeneratedMessageV3
          .parseWithIOException(PARSER, input, extensionRegistry);
    }
    public static proto.test.LittleFile.MyMessage parseDelimitedFrom(java.io.InputStream input)
        throws java.io.IOException {
      return com.google.protobuf.GeneratedMessageV3
          .parseDelimitedWithIOException(PARSER, input);
    }
    public static proto.test.LittleFile.MyMessage parseDelimitedFrom(
        java.io.InputStream input,
        com.google.protobuf.ExtensionRegistryLite extensionRegistry)
        throws java.io.IOException {
      return com.google.protobuf.GeneratedMessageV3
          .parseDelimitedWithIOException(PARSER, input, extensionRegistry);
    }
    public static proto.test.LittleFile.MyMessage parseFrom(
        com.google.protobuf.CodedInputStream input)
        throws java.io.IOException {
      return com.google.protobuf.GeneratedMessageV3
          .parseWithIOException(PARSER, input);
    }
    public static proto.test.LittleFile.MyMessage parseFrom(
        com.google.protobuf.CodedInputStream input,
        com.google.protobuf.ExtensionRegistryLite extensionRegistry)
        throws java.io.IOException {
      return com.google.protobuf.GeneratedMessageV3
          .parseWithIOException(PARSER, input, extensionRegistry);
    }

    public Builder newBuilderForType() { return newBuilder(); }
    public static Builder newBuilder() {
      return DEFAULT_INSTANCE.toBuilder();
    }
    public static Builder newBuilder(proto.test.LittleFile.MyMessage prototype) {
      return DEFAULT_INSTANCE.toBuilder().mergeFrom(prototype);
    }
    public Builder toBuilder() {
      return this == DEFAULT_INSTANCE
          ? new Builder() : new Builder().mergeFrom(this);
    }

    @java.lang.Override
    protected Builder newBuilderForType(
        com.google.protobuf.GeneratedMessageV3.BuilderParent parent) {
      Builder builder = new Builder(parent);
      return builder;
    }
    /**
     * Protobuf type {@code vertis.MyMessage}
     */
    public static final class Builder extends
        com.google.protobuf.GeneratedMessageV3.Builder<Builder> implements
        // @@protoc_insertion_point(builder_implements:vertis.MyMessage)
        proto.test.LittleFile.MyMessageOrBuilder {
      public static final com.google.protobuf.Descriptors.Descriptor
          getDescriptor() {
        return proto.test.LittleFile.internal_static_vertis_MyMessage_descriptor;
      }

      protected com.google.protobuf.GeneratedMessageV3.FieldAccessorTable
          internalGetFieldAccessorTable() {
        return proto.test.LittleFile.internal_static_vertis_MyMessage_fieldAccessorTable
            .ensureFieldAccessorsInitialized(
                proto.test.LittleFile.MyMessage.class, proto.test.LittleFile.MyMessage.Builder.class);
      }

      // Construct using proto.test.LittleFile.MyMessage.newBuilder()
      private Builder() {
        maybeForceBuilderInitialization();
      }

      private Builder(
          com.google.protobuf.GeneratedMessageV3.BuilderParent parent) {
        super(parent);
        maybeForceBuilderInitialization();
      }
      private void maybeForceBuilderInitialization() {
        if (com.google.protobuf.GeneratedMessageV3
                .alwaysUseFieldBuilders) {
        }
      }
      public Builder clear() {
        super.clear();
        myString_ = "";

        myInt_ = 0;

        myBool_ = false;

        myEnum_ = 0;

        myBytes_ = com.google.protobuf.ByteString.EMPTY;

        myValue_ = 0;

        myIntBytes_ = com.google.protobuf.ByteString.EMPTY;

        return this;
      }

      public com.google.protobuf.Descriptors.Descriptor
          getDescriptorForType() {
        return proto.test.LittleFile.internal_static_vertis_MyMessage_descriptor;
      }

      public proto.test.LittleFile.MyMessage getDefaultInstanceForType() {
        return proto.test.LittleFile.MyMessage.getDefaultInstance();
      }

      public proto.test.LittleFile.MyMessage build() {
        proto.test.LittleFile.MyMessage result = buildPartial();
        if (!result.isInitialized()) {
          throw newUninitializedMessageException(result);
        }
        return result;
      }

      public proto.test.LittleFile.MyMessage buildPartial() {
        proto.test.LittleFile.MyMessage result = new proto.test.LittleFile.MyMessage(this);
        result.myString_ = myString_;
        result.myInt_ = myInt_;
        result.myBool_ = myBool_;
        result.myEnum_ = myEnum_;
        result.myBytes_ = myBytes_;
        result.myValue_ = myValue_;
        result.myIntBytes_ = myIntBytes_;
        onBuilt();
        return result;
      }

      public Builder clone() {
        return (Builder) super.clone();
      }
      public Builder setField(
          com.google.protobuf.Descriptors.FieldDescriptor field,
          java.lang.Object value) {
        return (Builder) super.setField(field, value);
      }
      public Builder clearField(
          com.google.protobuf.Descriptors.FieldDescriptor field) {
        return (Builder) super.clearField(field);
      }
      public Builder clearOneof(
          com.google.protobuf.Descriptors.OneofDescriptor oneof) {
        return (Builder) super.clearOneof(oneof);
      }
      public Builder setRepeatedField(
          com.google.protobuf.Descriptors.FieldDescriptor field,
          int index, java.lang.Object value) {
        return (Builder) super.setRepeatedField(field, index, value);
      }
      public Builder addRepeatedField(
          com.google.protobuf.Descriptors.FieldDescriptor field,
          java.lang.Object value) {
        return (Builder) super.addRepeatedField(field, value);
      }
      public Builder mergeFrom(com.google.protobuf.Message other) {
        if (other instanceof proto.test.LittleFile.MyMessage) {
          return mergeFrom((proto.test.LittleFile.MyMessage)other);
        } else {
          super.mergeFrom(other);
          return this;
        }
      }

      public Builder mergeFrom(proto.test.LittleFile.MyMessage other) {
        if (other == proto.test.LittleFile.MyMessage.getDefaultInstance()) return this;
        if (!other.getMyString().isEmpty()) {
          myString_ = other.myString_;
          onChanged();
        }
        if (other.getMyInt() != 0) {
          setMyInt(other.getMyInt());
        }
        if (other.getMyBool() != false) {
          setMyBool(other.getMyBool());
        }
        if (other.myEnum_ != 0) {
          setMyEnumValue(other.getMyEnumValue());
        }
        if (other.getMyBytes() != com.google.protobuf.ByteString.EMPTY) {
          setMyBytes(other.getMyBytes());
        }
        if (other.getMyValue() != 0) {
          setMyValue(other.getMyValue());
        }
        if (other.getMyIntBytes() != com.google.protobuf.ByteString.EMPTY) {
          setMyIntBytes(other.getMyIntBytes());
        }
        this.mergeUnknownFields(other.unknownFields);
        onChanged();
        return this;
      }

      public final boolean isInitialized() {
        return true;
      }

      public Builder mergeFrom(
          com.google.protobuf.CodedInputStream input,
          com.google.protobuf.ExtensionRegistryLite extensionRegistry)
          throws java.io.IOException {
        proto.test.LittleFile.MyMessage parsedMessage = null;
        try {
          parsedMessage = PARSER.parsePartialFrom(input, extensionRegistry);
        } catch (com.google.protobuf.InvalidProtocolBufferException e) {
          parsedMessage = (proto.test.LittleFile.MyMessage) e.getUnfinishedMessage();
          throw e.unwrapIOException();
        } finally {
          if (parsedMessage != null) {
            mergeFrom(parsedMessage);
          }
        }
        return this;
      }

      private java.lang.Object myString_ = "";
      /**
       * <code>string my_string = 1;</code>
       */
      public java.lang.String getMyString() {
        java.lang.Object ref = myString_;
        if (!(ref instanceof java.lang.String)) {
          com.google.protobuf.ByteString bs =
              (com.google.protobuf.ByteString) ref;
          java.lang.String s = bs.toStringUtf8();
          myString_ = s;
          return s;
        } else {
          return (java.lang.String) ref;
        }
      }
      /**
       * <code>string my_string = 1;</code>
       */
      public com.google.protobuf.ByteString
          getMyStringBytes() {
        java.lang.Object ref = myString_;
        if (ref instanceof String) {
          com.google.protobuf.ByteString b = 
              com.google.protobuf.ByteString.copyFromUtf8(
                  (java.lang.String) ref);
          myString_ = b;
          return b;
        } else {
          return (com.google.protobuf.ByteString) ref;
        }
      }
      /**
       * <code>string my_string = 1;</code>
       */
      public Builder setMyString(
          java.lang.String value) {
        if (value == null) {
    throw new NullPointerException();
  }
  
        myString_ = value;
        onChanged();
        return this;
      }
      /**
       * <code>string my_string = 1;</code>
       */
      public Builder clearMyString() {
        
        myString_ = getDefaultInstance().getMyString();
        onChanged();
        return this;
      }
      /**
       * <code>string my_string = 1;</code>
       */
      public Builder setMyStringBytes(
          com.google.protobuf.ByteString value) {
        if (value == null) {
    throw new NullPointerException();
  }
  checkByteStringIsUtf8(value);
        
        myString_ = value;
        onChanged();
        return this;
      }

      private int myInt_ ;
      /**
       * <code>int32 my_int = 2;</code>
       */
      public int getMyInt() {
        return myInt_;
      }
      /**
       * <code>int32 my_int = 2;</code>
       */
      public Builder setMyInt(int value) {
        
        myInt_ = value;
        onChanged();
        return this;
      }
      /**
       * <code>int32 my_int = 2;</code>
       */
      public Builder clearMyInt() {
        
        myInt_ = 0;
        onChanged();
        return this;
      }

      private boolean myBool_ ;
      /**
       * <code>bool my_bool = 3;</code>
       */
      public boolean getMyBool() {
        return myBool_;
      }
      /**
       * <code>bool my_bool = 3;</code>
       */
      public Builder setMyBool(boolean value) {
        
        myBool_ = value;
        onChanged();
        return this;
      }
      /**
       * <code>bool my_bool = 3;</code>
       */
      public Builder clearMyBool() {
        
        myBool_ = false;
        onChanged();
        return this;
      }

      private int myEnum_ = 0;
      /**
       * <code>.vertis.MyMessage.MyEnum my_enum = 4;</code>
       */
      public int getMyEnumValue() {
        return myEnum_;
      }
      /**
       * <code>.vertis.MyMessage.MyEnum my_enum = 4;</code>
       */
      public Builder setMyEnumValue(int value) {
        myEnum_ = value;
        onChanged();
        return this;
      }
      /**
       * <code>.vertis.MyMessage.MyEnum my_enum = 4;</code>
       */
      public proto.test.LittleFile.MyMessage.MyEnum getMyEnum() {
        proto.test.LittleFile.MyMessage.MyEnum result = proto.test.LittleFile.MyMessage.MyEnum.valueOf(myEnum_);
        return result == null ? proto.test.LittleFile.MyMessage.MyEnum.UNRECOGNIZED : result;
      }
      /**
       * <code>.vertis.MyMessage.MyEnum my_enum = 4;</code>
       */
      public Builder setMyEnum(proto.test.LittleFile.MyMessage.MyEnum value) {
        if (value == null) {
          throw new NullPointerException();
        }
        
        myEnum_ = value.getNumber();
        onChanged();
        return this;
      }
      /**
       * <code>.vertis.MyMessage.MyEnum my_enum = 4;</code>
       */
      public Builder clearMyEnum() {
        
        myEnum_ = 0;
        onChanged();
        return this;
      }

      private com.google.protobuf.ByteString myBytes_ = com.google.protobuf.ByteString.EMPTY;
      /**
       * <code>bytes my_bytes = 5;</code>
       */
      public com.google.protobuf.ByteString getMyBytes() {
        return myBytes_;
      }
      /**
       * <code>bytes my_bytes = 5;</code>
       */
      public Builder setMyBytes(com.google.protobuf.ByteString value) {
        if (value == null) {
    throw new NullPointerException();
  }
  
        myBytes_ = value;
        onChanged();
        return this;
      }
      /**
       * <code>bytes my_bytes = 5;</code>
       */
      public Builder clearMyBytes() {
        
        myBytes_ = getDefaultInstance().getMyBytes();
        onChanged();
        return this;
      }

      private int myValue_ ;
      /**
       * <code>int32 my_value = 6;</code>
       */
      public int getMyValue() {
        return myValue_;
      }
      /**
       * <code>int32 my_value = 6;</code>
       */
      public Builder setMyValue(int value) {
        
        myValue_ = value;
        onChanged();
        return this;
      }
      /**
       * <code>int32 my_value = 6;</code>
       */
      public Builder clearMyValue() {
        
        myValue_ = 0;
        onChanged();
        return this;
      }

      private com.google.protobuf.ByteString myIntBytes_ = com.google.protobuf.ByteString.EMPTY;
      /**
       * <code>bytes my_int_bytes = 7;</code>
       */
      public com.google.protobuf.ByteString getMyIntBytes() {
        return myIntBytes_;
      }
      /**
       * <code>bytes my_int_bytes = 7;</code>
       */
      public Builder setMyIntBytes(com.google.protobuf.ByteString value) {
        if (value == null) {
    throw new NullPointerException();
  }
  
        myIntBytes_ = value;
        onChanged();
        return this;
      }
      /**
       * <code>bytes my_int_bytes = 7;</code>
       */
      public Builder clearMyIntBytes() {
        
        myIntBytes_ = getDefaultInstance().getMyIntBytes();
        onChanged();
        return this;
      }
      public final Builder setUnknownFields(
          final com.google.protobuf.UnknownFieldSet unknownFields) {
        return super.setUnknownFieldsProto3(unknownFields);
      }

      public final Builder mergeUnknownFields(
          final com.google.protobuf.UnknownFieldSet unknownFields) {
        return super.mergeUnknownFields(unknownFields);
      }


      // @@protoc_insertion_point(builder_scope:vertis.MyMessage)
    }

    // @@protoc_insertion_point(class_scope:vertis.MyMessage)
    private static final proto.test.LittleFile.MyMessage DEFAULT_INSTANCE;
    static {
      DEFAULT_INSTANCE = new proto.test.LittleFile.MyMessage();
    }

    public static proto.test.LittleFile.MyMessage getDefaultInstance() {
      return DEFAULT_INSTANCE;
    }

    private static final com.google.protobuf.Parser<MyMessage>
        PARSER = new com.google.protobuf.AbstractParser<MyMessage>() {
      public MyMessage parsePartialFrom(
          com.google.protobuf.CodedInputStream input,
          com.google.protobuf.ExtensionRegistryLite extensionRegistry)
          throws com.google.protobuf.InvalidProtocolBufferException {
        return new MyMessage(input, extensionRegistry);
      }
    };

    public static com.google.protobuf.Parser<MyMessage> parser() {
      return PARSER;
    }

    @java.lang.Override
    public com.google.protobuf.Parser<MyMessage> getParserForType() {
      return PARSER;
    }

    public proto.test.LittleFile.MyMessage getDefaultInstanceForType() {
      return DEFAULT_INSTANCE;
    }

  }

  private static final com.google.protobuf.Descriptors.Descriptor
    internal_static_vertis_MyMessage_descriptor;
  private static final 
    com.google.protobuf.GeneratedMessageV3.FieldAccessorTable
      internal_static_vertis_MyMessage_fieldAccessorTable;

  public static com.google.protobuf.Descriptors.FileDescriptor
      getDescriptor() {
    return descriptor;
  }
  private static  com.google.protobuf.Descriptors.FileDescriptor
      descriptor;
  static {
    java.lang.String[] descriptorData = {
      "\n\030vertis/little_file.proto\022\006vertis\"\347\001\n\tM" +
      "yMessage\022\021\n\tmy_string\030\001 \001(\t\022\016\n\006my_int\030\002 " +
      "\001(\005\022\017\n\007my_bool\030\003 \001(\010\022)\n\007my_enum\030\004 \001(\0162\030." +
      "vertis.MyMessage.MyEnum\022\020\n\010my_bytes\030\005 \001(" +
      "\014\022\020\n\010my_value\030\006 \001(\005\022\024\n\014my_int_bytes\030\007 \001(" +
      "\014\"A\n\006MyEnum\022\013\n\007DEFAULT\020\000\022\014\n\010MY_VALUE\020\001\022\r" +
      "\n\tHIS_VALUE\020\002\022\r\n\tHER_VALUE\020\003B\014\n\nproto.te" +
      "stb\006proto3"
    };
    com.google.protobuf.Descriptors.FileDescriptor.InternalDescriptorAssigner assigner =
        new com.google.protobuf.Descriptors.FileDescriptor.    InternalDescriptorAssigner() {
          public com.google.protobuf.ExtensionRegistry assignDescriptors(
              com.google.protobuf.Descriptors.FileDescriptor root) {
            descriptor = root;
            return null;
          }
        };
    com.google.protobuf.Descriptors.FileDescriptor
      .internalBuildGeneratedFileFrom(descriptorData,
        new com.google.protobuf.Descriptors.FileDescriptor[] {
        }, assigner);
    internal_static_vertis_MyMessage_descriptor =
      getDescriptor().getMessageTypes().get(0);
    internal_static_vertis_MyMessage_fieldAccessorTable = new
      com.google.protobuf.GeneratedMessageV3.FieldAccessorTable(
        internal_static_vertis_MyMessage_descriptor,
        new java.lang.String[] { "MyString", "MyInt", "MyBool", "MyEnum", "MyBytes", "MyValue", "MyIntBytes", });
  }

  // @@protoc_insertion_point(outer_class_scope)
}
