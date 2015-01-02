package utils

import akka.util.ByteString

/**
 * Created by arturas on 2015-01-02.
 */
object Hex {
  val hexArray = "0123456789ABCDEF".toCharArray

  def bytesToHex(bytes: collection.IndexedSeq[Byte]) = {
    val hexChars = new Array[Byte](bytes.size * 2)
    var j = 0
    while (j < bytes.size) {
      val v = bytes(j) & 0xFF
      hexChars(j * 2) = hexArray(v >>> 4).toByte
      hexChars(j * 2 + 1) = hexArray(v & 0x0F).toByte
      j += 1
    }
    new String(hexChars)
  }
}
