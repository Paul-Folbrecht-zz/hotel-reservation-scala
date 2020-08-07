package com.bravolt

import java.text.SimpleDateFormat

import org.scalatest.flatspec._

class HotelSpec extends AnyFlatSpec {
  private val dateFormatter = new SimpleDateFormat("dd/MM/yyyy")
  private val rooms = Seq(
    Room(1, 1),
    Room(2, 1),
    Room(2, 1),
    Room(2, 1),
    Room(3, 2),
    Room(3, 2)
  )

  behavior of "Our reservation system"

  it should "" in {
    assert(Set.empty.size === 0)
  }

  it should "allow valid reservations" in {
    val existingRequests = Seq(
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/5/2020")),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/10/2020"))
    )
    val request = ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"))
    assert(new Hotel(rooms).reserve(request, existingRequests))
  }

  it should "allow valid reservations corner case" in {
    val existingRequests = Seq(
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/5/2020"))
    )
    val request = ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"))
    assert(new Hotel(rooms).reserve(request, existingRequests))
  }

  it should "reject overbooking reservations" in {
    val existingRequests = Seq(
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/5/2020")),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/5/2020"))
    )
    val request = ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/8/2020"))
    assert(!new Hotel(rooms).reserve(request, existingRequests))
  }

  it should "respect occupancy requirements" in {
    val existingRequests = Seq(
      ReservationRequest(3, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(3, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(3, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/5/2020"))
    )
    val request = ReservationRequest(3, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/8/2020"))
    assert(!new Hotel(rooms).reserve(request, existingRequests))
  }

  it should "respect occupancy requirements 2" in {
    val existingRequests = Seq()
    val request = ReservationRequest(4, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/8/2020"))
    assert(!new Hotel(rooms).reserve(request, existingRequests))
  }

  it should "respect pet requirements" in {
    val existingRequests = Seq(
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), 1),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), 1),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), 1),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), 1)
    )
    val request = ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/8/2020"), 1)
    assert(!new Hotel(rooms).reserve(request, existingRequests))
  }

  it should "respect max pet requirements" in {
    val existingRequests = Seq()
    val request = ReservationRequest(4, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/8/2020"), 3)
    assert(!new Hotel(rooms).reserve(request, existingRequests))
  }

  it should "respect handicap access requirements" in {
    val existingRequests = Seq(
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), 0, handicapAccessible = true),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), 0, handicapAccessible = true),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), 0, handicapAccessible = true),
      ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), 0, handicapAccessible = true)
    )
    val request = ReservationRequest(1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/8/2020"), 0, handicapAccessible = true)
    assert(!new Hotel(rooms).reserve(request, existingRequests))
  }
}
