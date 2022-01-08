package co.blocke.scala_reflection

import munit.*
import java.nio.ByteBuffer

class Serialization extends munit.FunSuite:

  test("Serialize/Derialize basic case class") {
    val rtype = RType.of[Person]
    val buffer = ByteBuffer.allocate(BUFFER_MAX)
    rtype.toBytes(buffer)
    val str = java.util.Base64.getEncoder().encodeToString(buffer.array.slice(0, buffer.position))
    assertEquals(RType.deserialize(str), rtype)
  }

  test("Serialize/Deserialize self-referencing case class") {
    val rtype = RType.of[Person2]
    val buffer = ByteBuffer.allocate(BUFFER_MAX)
    rtype.toBytes(buffer)
    val str = java.util.Base64.getEncoder().encodeToString(buffer.array.slice(0, buffer.position))
    assertEquals(RType.deserialize(str), rtype)
  }