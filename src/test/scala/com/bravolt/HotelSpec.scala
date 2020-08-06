package com.bravolt

import org.scalatest.flatspec._

class HotelSpec extends AnyFlatSpec {
  behavior of "Our reservation system"

  it should "have size 0" in {
    assert(Set.empty.size === 0)
  }
}
