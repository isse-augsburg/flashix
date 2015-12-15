package helpers.scala

import scala.reflect.ClassTag

/**
 * design decisions:
 * * the array where the data is stored is passed to the encode/decode functions with an index where
 *   en/decoding should start, this allows combining en/decoding functions without array allocations
 * * every decoding function must figure out how many bytes are encoded by itself
 * * encode/decode functions return the new array index
 */
object Encoding {
  def alignUp(value: Int, align: Int): Int = {
    assert(align > 0 && value >= 0)
    if (value % align == 0)
      value
    else
      (value / align + 1) * align
  }
  def isAligned(value: Int, align: Int): Boolean = {
    assert(align > 0 && value >= 0)
    (value % align) == 0
  }
  def toUnsignedLong(byte: Byte): Long = byte.toLong & 0xFF
  def toUnsignedInt(byte: Byte): Int = byte.toInt & 0xFF
  def toUnsignedShort(byte: Byte): Short = toUnsignedInt(byte).toShort

  def flashsize(value: Boolean): Int = 1
  def flashsize(value: Byte): Int = 1
  def flashsize(value: Short): Int = 2
  def flashsize(value: Int): Int = 4
  def flashsize(value: Long): Int = 8
  def flashsize(value: Char): Int = 2
  def flashsize(value: String): Int = {
    // NOTE: 4 bytes for the size, 2 equals flashsize(Char)
    4 + 2 * value.size
  }
  def encode(value: Boolean, encoded: Array[Byte], index: Int): Int = {
    encoded(index) = if (value) 1 else 0
    index + 1
  }
  def encode(value: Byte, encoded: Array[Byte], index: Int): Int = {
    encoded(index) = value
    index + 1
  }
  def encode(value: Short, encoded: Array[Byte], index: Int): Int = {
    encoded(index) = value.toByte
    encoded(index + 1) = (value >>> 8).toByte
    index + 2
  }
  def encode(value: Int, encoded: Array[Byte], index: Int): Int = {
    encoded(index) = value.toByte
    encoded(index + 1) = (value >>> 8).toByte
    encoded(index + 2) = (value >>> 16).toByte
    encoded(index + 3) = (value >>> 24).toByte
    index + 4
  }
  def encode(value: Long, encoded: Array[Byte], index: Int): Int = {
    encoded(index) = value.toByte
    encoded(index + 1) = (value >>> 8).toByte
    encoded(index + 2) = (value >>> 16).toByte
    encoded(index + 3) = (value >>> 24).toByte
    encoded(index + 4) = (value >>> 32).toByte
    encoded(index + 5) = (value >>> 40).toByte
    encoded(index + 6) = (value >>> 48).toByte
    encoded(index + 7) = (value >>> 56).toByte
    index + 8
  }
  def encode(value: Char, encoded: Array[Byte], index: Int): Int = encode(value.toShort, encoded, index)
  def encode(value: String, encoded: Array[Byte], index: Int): Int = {
    var curindex = encode(value.size, encoded, index)
    value.foreach { c =>
      curindex = encode(c, encoded, curindex)
    }
    curindex
  }

  def decodeBoolean(encoded: Array[Byte], index: Int): (Boolean, Int) = {
    (encoded(index) == 1, index + 1)
  }
  def decodeByte(encoded: Array[Byte], index: Int): (Byte, Int) = {
    (encoded(index), index + 1)
  }
  def decodeShort(encoded: Array[Byte], index: Int): (Short, Int) = {
    ((toUnsignedShort(encoded(index)) |
      (toUnsignedShort(encoded(index + 1)) << 8)).toShort, index + 2)
  }
  def decodeInt(encoded: Array[Byte], index: Int): (Int, Int) = {
    (toUnsignedInt(encoded(index)) |
     (toUnsignedInt(encoded(index + 1)) << 8) |
     (toUnsignedInt(encoded(index + 2)) << 16) |
     (toUnsignedInt(encoded(index + 3)) << 24), index + 4)
  }
  def decodeLong(encoded: Array[Byte], index: Int): (Long, Int) = {
    (toUnsignedLong(encoded(index)) |
     (toUnsignedLong(encoded(index + 1)) << 8) |
     (toUnsignedLong(encoded(index + 2)) << 16) |
     (toUnsignedLong(encoded(index + 3)) << 24) |
     (toUnsignedLong(encoded(index + 4)) << 32) |
     (toUnsignedLong(encoded(index + 5)) << 40) |
     (toUnsignedLong(encoded(index + 6)) << 48) |
     (toUnsignedLong(encoded(index + 7)) << 56), index + 8)
  }
  def decodeNat(encoded: Array[Byte], index: Int): (Int, Int) = {
    val (value, newindex) = decodeInt(encoded, index)
    if (value < 0)
      throw DecodeFailure()
    (value, newindex)
  }
  def decodeChar(encoded: Array[Byte], index: Int): (Char, Int) = {
    val (value, newindex) = decodeShort(encoded, index)
    (value.toChar, newindex)
  }
  def decodeString(encoded: Array[Byte], index: Int): (String, Int) = {
    val (size, newIndex) = decodeInt(encoded, index)
    if (size < 0)
      throw DecodeFailure()
    var curIndex = newIndex
    var done = 0
    val buffer = new StringBuffer()
    while (done < size) {
      val (character, newIndex) = decodeChar(encoded, curIndex)
      buffer.append(character)
      curIndex = newIndex
      done += 1
    }
    (buffer.toString(), curIndex)
  }

  /** Calculate how much space the encoded array takes */
  def flashsizeArray[T](array: Array[T], flashsizef: T => Int): Int = {
    // NOTE: 4 byte for the length of the array
    array.array.foldLeft(4)((size, elem) => size + flashsizef(elem))
  }
  /** Encode the array, takes up the space in encoded from "index" to "index + flashsize(array, flashsizef)" */
  def encodeArray[T](array: Array[T], encoded: Array[Byte], index: Int, encodef: (T, Array[Byte], Int) => Int): Int = {
    encode(array.size, encoded, index)
    var curindex = index + 4
    array.foreach(elem => {
      curindex = encodef(elem, encoded, curindex)
    })
    return curindex
  }
  def decodeArray[T: ClassTag](encoded: Array[Byte], index: Int, decodef: (Array[Byte], Int) => (T, Int)): (Array[T], Int) = {
    val (size, newindex) = decodeInt(encoded, index)
    if (size < 0)
      throw DecodeFailure()
    var curindex = newindex
    val array = new Array[T](size)
    var curarrayindex = 0
    while (curarrayindex != size) {
      val (value, newindex) = decodef(encoded, curindex)
      array(curarrayindex) = value
      curindex = newindex
      curarrayindex += 1
    }
    return (array, curindex)
  }

  def flashsizeMap[K, T](map: scala.collection.mutable.Map[K, T], flashsizekeyf: K => Int, flashsizef: T => Int): Int = {
    // NOTE: 4 byte for the number of entries
    map.foldLeft(4)((size, keyelem) => size + flashsizekeyf(keyelem._1) + flashsizef(keyelem._2))
  }
  def encodeMap[K, T](map: scala.collection.mutable.Map[K, T], encoded: Array[Byte], index: Int, encodekeyf: (K, Array[Byte], Int) => Int, encodef: (T, Array[Byte], Int) => Int): Int = {
    encode(map.size, encoded, index)
    var curindex = index + 4
    // TODO: This is not deterministic!
    map.foreach(keyelem => {
      curindex = encodekeyf(keyelem._1, encoded, curindex)
      curindex = encodef(keyelem._2, encoded, curindex)
    })
    return curindex
  }
  def decodeMap[K, T](encoded: Array[Byte], index: Int, decodekeyf: (Array[Byte], Int) => (K, Int), decodef: (Array[Byte], Int) => (T, Int)): (scala.collection.mutable.Map[K, T], Int) = {
    val (size, newindex) = decodeInt(encoded, index)
    if (size < 0)
      throw DecodeFailure()
    var curindex = newindex
    val map = scala.collection.mutable.Map[K, T]()
    var done = 0
    while (done != size) {
      val (key, newindex) = decodekeyf(encoded, curindex)
      val (value, newindex2) = decodef(encoded, newindex)
      map += ((key, value))
      curindex = newindex2
      done += 1
    }
    return (map, curindex)
  }

  /** Calculate how much space the encoded set takes */
  def flashsizeSet[T](set: collection.Set[T], flashsizef: T => Int): Int = {
    // NOTE: 4 byte for the size of the set
    set.foldLeft(4)((size, elem) => size + flashsizef(elem))
  }
  def flashsizeSet[T](set: SetWrapper[T], flashsizef: T => Int): Int = flashsizeSet[T](set.set, flashsizef)

  /**
   * Encode the set, takes up the space in encoded from "index" to "index + flashsize(set, flashsizef)"
   *  @note this is not necessarily functional, but who cares?
   */
  def encodeSet[T](set: collection.Set[T], encoded: Array[Byte], index: Int, encodef: (T, Array[Byte], Int) => Int): Int = {
    encode(set.size, encoded, index)
    var curindex = index + 4
    set.foreach(elem => {
      curindex = encodef(elem, encoded, curindex)
    })
    return curindex
  }
  def encodeSet[T](set: SetWrapper[T], encoded: Array[Byte], index: Int, encodef: (T, Array[Byte], Int) => Int): Int = encodeSet[T](set.set, encoded, index, encodef)
  def decodeSet[T: ClassTag](encoded: Array[Byte], index: Int, decodef: (Array[Byte], Int) => (T, Int)): (Set[T], Int) = {
    val (size, newindex) = decodeInt(encoded, index)
    if (size < 0)
      throw DecodeFailure()
    var curindex = newindex
    var set = scala.collection.immutable.Set[T]()
    var curcount = 0
    while (curcount != size) {
      val (value, newindex) = decodef(encoded, curindex)
      set += value
      curindex = newindex
      curcount += 1
    }
    return (set, curindex)
  }
  def decodeSetWrapper[T: ClassTag](encoded: Array[Byte], index: Int, decodef: (Array[Byte], Int) => (T, Int)): (SetWrapper[T], Int) = {
    val (set, newIndex) = decodeSet[T](encoded, index, decodef)
    (new SetWrapper[T](set), newIndex)
  }

  def flashsizeArrayWrapper[T](array: ArrayWrapper[T], flashsizef: T => Int): Int = flashsizeArray(array.array, flashsizef)
  def flashsizeArrayWrapperDeep[T <: DeepCopyable[T]](array: ArrayWrapperDeep[T], flashsizef: T => Int): Int = flashsizeArray(array.array, flashsizef)
  def encodeArrayWrapper[T](array: ArrayWrapper[T], encoded: Array[Byte], index: Int, encodef: (T, Array[Byte], Int) => Int): Int = encodeArray(array.array, encoded, index, encodef)
  def encodeArrayWrapperDeep[T <: DeepCopyable[T]](array: ArrayWrapperDeep[T], encoded: Array[Byte], index: Int, encodef: (T, Array[Byte], Int) => Int): Int = encodeArray(array.array, encoded, index, encodef)
  def decodeArrayWrapper[T: ClassTag](encoded: Array[Byte], index: Int, decodef: (Array[Byte], Int) => (T, Int)): (ArrayWrapper[T], Int) = {
    val (array, newindex) = decodeArray(encoded, index, decodef)
    return (new ArrayWrapper[T](array), newindex)
  }
  def decodeArrayWrapperDeep[T <: DeepCopyable[T]: ClassTag](encoded: Array[Byte], index: Int, decodef: (Array[Byte], Int) => (T, Int)): (ArrayWrapperDeep[T], Int) = {
    val (array, newindex) = decodeArray(encoded, index, decodef)
    return (new ArrayWrapperDeep[T](array), newindex)
  }

  def flashsizeMapWrapper[K, T](map: MapWrapper[K, T], flashsizekeyf: K => Int, flashsizedataf: T => Int): Int = flashsizeMap(map.map, flashsizekeyf, flashsizedataf)
  def flashsizeMapWrapperDeep[K, T <: DeepCopyable[T]](map: MapWrapperDeep[K, T], flashsizekeyf: K => Int, flashsizedataf: T => Int): Int = flashsizeMap(map.map, flashsizekeyf, flashsizedataf)
  def encodeMapWrapper[K, T](map: MapWrapper[K, T], encoded: Array[Byte], index: Int, encodekeyf: (K, Array[Byte], Int) => Int, encodedataf: (T, Array[Byte], Int) => Int): Int = encodeMap(map.map, encoded, index, encodekeyf, encodedataf)
  def encodeMapWrapperDeep[K, T <: DeepCopyable[T]](map: MapWrapperDeep[K, T], encoded: Array[Byte], index: Int, encodekeyf: (K, Array[Byte], Int) => Int, encodedataf: (T, Array[Byte], Int) => Int): Int = encodeMap(map.map, encoded, index, encodekeyf, encodedataf)
  def decodeMapWrapper[K, T: ClassTag](encoded: Array[Byte], index: Int, decodekeyf: (Array[Byte], Int) => (K, Int), decodedataf: (Array[Byte], Int) => (T, Int)): (MapWrapper[K, T], Int) = {
    val (map, newindex) = decodeMap(encoded, index, decodekeyf, decodedataf)
    return (new MapWrapper[K, T](map), newindex)
  }
  def decodeMapWrapperDeep[K, T <: DeepCopyable[T]: ClassTag](encoded: Array[Byte], index: Int, decodekeyf: (Array[Byte], Int) => (K, Int), decodedataf: (Array[Byte], Int) => (T, Int)): (MapWrapperDeep[K, T], Int) = {
    val (map, newindex) = decodeMap(encoded, index, decodekeyf, decodedataf)
    return (new MapWrapperDeep[K, T](map), newindex)
  }
}
