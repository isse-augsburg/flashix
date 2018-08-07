// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package helpers.scala

import scala.reflect.ClassTag

object Encoding {
  val ENCODED_NAT_SIZE = 4
  val ENCODED_BOOL_SIZE = ENCODED_NAT_SIZE // NOTE: with this flashix NODE_HEADER_SIZE is aligned
  val ENCODED_CHAR_SIZE = 2

  def toUnsignedLong(byte: Byte): Long = byte.toLong & 0xFF
  def toUnsignedInt(byte: Byte): Int = byte.toInt & 0xFF
  def toUnsignedShort(byte: Byte): Short = toUnsignedInt(byte).toShort

  def flashsize_bool(value: Boolean): Int = ENCODED_BOOL_SIZE
  def flashsize_byte(value: Byte): Int = 1
  def flashsize_short(value: Short): Int = 2
  def flashsize_int(value: Int): Int = 4
  def flashsize_nat(value: Int): Int = ENCODED_NAT_SIZE
  def flashsize_long(value: Long): Int = 8
  def flashsize_char(value: Char): Int = ENCODED_CHAR_SIZE
  def flashsize_string(value: String): Int = {
    // NOTE: 4 bytes for the size
    4 + ENCODED_CHAR_SIZE * value.size
  }

  def encode_bool(value: Boolean, index: Int, encoded: ArrayWrapper[Byte], size: Ref[Int], err: Ref[types.error.error]) {
    val nat = if (value) 1 else 0
    encode_nat(nat, index, encoded, size, err)
  }
  def encode_byte(value: Byte, index: Int, encoded: ArrayWrapper[Byte], size: Ref[Int], err: Ref[types.error.error]) {
    encoded(index) = value
    size := 1
    err := types.error.ESUCCESS
  }
  def encode_short(value: Short, index: Int, encoded: ArrayWrapper[Byte], size: Ref[Int], err: Ref[types.error.error]) {
    encoded(index) = value.toByte
    encoded(index + 1) = (value >>> 8).toByte
    size := 2
    err := types.error.ESUCCESS
  }
  def encode_int(value: Int, index: Int, encoded: ArrayWrapper[Byte], size: Ref[Int], err: Ref[types.error.error]) {
    encoded(index) = value.toByte
    encoded(index + 1) = (value >>> 8).toByte
    encoded(index + 2) = (value >>> 16).toByte
    encoded(index + 3) = (value >>> 24).toByte
    size := 4
    err := types.error.ESUCCESS
  }
  def encode_nat(value: Int, index: Int, encoded: ArrayWrapper[Byte], size: Ref[Int], err: Ref[types.error.error]) {
    assert(value >= 0)
    encode_int(value, index, encoded, size, err)
  }
  def encode_long(value: Long, index: Int, encoded: ArrayWrapper[Byte], size: Ref[Int], err: Ref[types.error.error]) {
    encoded(index) = value.toByte
    encoded(index + 1) = (value >>> 8).toByte
    encoded(index + 2) = (value >>> 16).toByte
    encoded(index + 3) = (value >>> 24).toByte
    encoded(index + 4) = (value >>> 32).toByte
    encoded(index + 5) = (value >>> 40).toByte
    encoded(index + 6) = (value >>> 48).toByte
    encoded(index + 7) = (value >>> 56).toByte
    size := 8
    err := types.error.ESUCCESS
  }
  def encode_char(value: Char, index: Int, encoded: ArrayWrapper[Byte], size: Ref[Int], err: Ref[types.error.error]) {
    encode_short(value.toShort, index, encoded, size, err)
  }
  def encode_string(value: String, index: Int, encoded: ArrayWrapper[Byte], size: Ref[Int], err: Ref[types.error.error]) {
    encode_nat(value.size, index, encoded, size, err)
    value.foreach { c =>
      if (err.get == types.error.ESUCCESS) {
        val newsize = new Ref[Int](0)
        encode_char(c, index + size.get, encoded, newsize, err)
        size := size.get + newsize.get
      }
    }
  }

  def decode_bool(index: Int, encoded: ArrayWrapper[Byte], elem: Ref[Boolean], size: Ref[Int], err: Ref[types.error.error]) {
    val nat: Ref[Int] = new Ref(0)
    decode_nat(index, encoded, nat, size, err)
    if (err == types.error.ESUCCESS)
      elem := nat.get != 0
  }
  def decode_byte(index: Int, encoded: ArrayWrapper[Byte], elem: Ref[Byte], size: Ref[Int], err: Ref[types.error.error]) {
    elem := encoded(index)
    size := 1
    err := types.error.ESUCCESS
  }
  def decode_short(index: Int, encoded: ArrayWrapper[Byte], elem: Ref[Short], size: Ref[Int], err: Ref[types.error.error]) {
    elem := (toUnsignedShort(encoded(index)) | (toUnsignedShort(encoded(index + 1)) << 8)).toShort
    size := 2
    err := types.error.ESUCCESS
  }
  def decode_int(index: Int, encoded: ArrayWrapper[Byte], elem: Ref[Int], size: Ref[Int], err: Ref[types.error.error]) {
    elem := toUnsignedInt(encoded(index)) | (toUnsignedInt(encoded(index + 1)) << 8) | (toUnsignedInt(encoded(index + 2)) << 16) | (toUnsignedInt(encoded(index + 3)) << 24)
    size := 4
    err := types.error.ESUCCESS
  }
  def decode_long(index: Int, encoded: ArrayWrapper[Byte], elem: Ref[Long], size: Ref[Int], err: Ref[types.error.error]) {
    elem := toUnsignedLong(encoded(index)) | (toUnsignedLong(encoded(index + 1)) << 8) | (toUnsignedLong(encoded(index + 2)) << 16) | (toUnsignedLong(encoded(index + 3)) << 24) | (toUnsignedLong(encoded(index + 4)) << 32) | (toUnsignedLong(encoded(index + 5)) << 40) | (toUnsignedLong(encoded(index + 6)) << 48) | (toUnsignedLong(encoded(index + 7)) << 56)
    size := 8
    err := types.error.ESUCCESS
  }
  def decode_nat(index: Int, encoded: ArrayWrapper[Byte], elem: Ref[Int], size: Ref[Int], err: Ref[types.error.error]) {
    decode_int(index, encoded, elem, size, err)
    if (elem.get < 0)
      err := types.error.EINVAL
  }
  def decode_char(index: Int, encoded: ArrayWrapper[Byte], elem: Ref[Char], size: Ref[Int], err: Ref[types.error.error]) {
    val elemShort = new Ref[Short](0)
    decode_short(index, encoded, elemShort, size, err)
    elem := elemShort.get.toChar
  }
  def decode_string(index: Int, encoded: ArrayWrapper[Byte], elem: Ref[String], size: Ref[Int], err: Ref[types.error.error]) {
    val strsize = new Ref[Int](0)
    decode_nat(index, encoded, strsize, size, err)
    var done = 0
    val buffer = new StringBuffer()
    while (done < strsize.get && err.get == types.error.ESUCCESS) {
      val char = new Ref[Char](0)
      val newsize = new Ref[Int](0)
      decode_char(index + size.get, encoded, char, newsize, err)
      buffer.append(char.get)
      size := size.get + newsize.get
      done += 1
    }
    elem := buffer.toString()
  }
}
