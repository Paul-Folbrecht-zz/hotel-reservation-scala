package com.bravolt

import java.text.SimpleDateFormat

import org.scalatest.flatspec._

class HotelSpec extends AnyFlatSpec {
  private val dateFormatter = new SimpleDateFormat("dd/MM/yyyy")
  private val rooms = Seq(
    Room(101, 1, 1),
    Room(102, 2, 1),
    Room(103, 2, 1),
    Room(104, 2, 1),
    Room(201, 3, 2),
    Room(202, 3, 2)
  )

  behavior of "Our reservation system"

  it should "allow valid reservations" in {
    val existingRequests = Seq(
      ReservationRequest(100, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(101, 2, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/5/2020")),
      ReservationRequest(102, 2, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/10/2020"))
    )
    val request = ReservationRequest(1000, 2, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"))
    val (cost, error) = new Hotel(rooms).reserve(request, existingRequests)
    assert(cost == 75.0)
    assert(error.isEmpty)
  }

  it should "allow valid reservations corner case" in {
    val existingRequests = Seq(
      ReservationRequest(101, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(102, 2, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(103, 2, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(104, 2, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(105, 3, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/5/2020"))
    )
    val request = ReservationRequest(1000, 3, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"))
    val (cost, error) = new Hotel(rooms).reserve(request, existingRequests)
    assert(cost == 90.0)
    assert(error.isEmpty)
  }

  it should "reject overbooking reservations" in {
    val existingRequests = Seq(
      ReservationRequest(101, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(102, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(103, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(104, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(105, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(106, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/5/2020"))
    )
    val request = ReservationRequest(1000, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/8/2020"))
    val (_, error) = new Hotel(rooms).reserve(request, existingRequests)
    assert(error.isDefined)
  }

  it should "reject overbooking reservations 2" in {
    val existingRequests = Seq(
      ReservationRequest(101, 3, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(102, 3, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"))
    )
    val request = ReservationRequest(1000, 3, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/8/2020"))
    val (_, error) = new Hotel(rooms).reserve(request, existingRequests)
    assert(error.isDefined)
  }

  it should "reject overbooking reservations based on handicap accessibility" in {
    val existingRequests = Seq(
      ReservationRequest(101, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), handicapAccessible = true),
      ReservationRequest(102, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), handicapAccessible = true),
      ReservationRequest(103, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), handicapAccessible = true),
      ReservationRequest(104, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), handicapAccessible = true)
    )
    val request = ReservationRequest(1000, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/8/2020"), handicapAccessible = true)
    val (_, error) = new Hotel(rooms).reserve(request, existingRequests)
    assert(error.isDefined)
  }

  it should "respect occupancy requirements" in {
    val existingRequests = Seq(
      ReservationRequest(101, 3, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020")),
      ReservationRequest(102, 3, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/5/2020"))
    )
    val request = ReservationRequest(1000, 3, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/8/2020"))
    val (_, error) = new Hotel(rooms).reserve(request, existingRequests)
    assert(error.isDefined)
  }

  it should "respect occupancy requirements 2" in {
    val existingRequests = Seq()
    val request = ReservationRequest(1000, 4, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/8/2020"))
    val (_, error) = new Hotel(rooms).reserve(request, existingRequests)
    assert(error.isDefined)
  }

  it should "respect pet requirements" in {
    val existingRequests = Seq(
      ReservationRequest(100, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), 1),
      ReservationRequest(101, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), 1),
      ReservationRequest(102, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), 1),
      ReservationRequest(103, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), 1)
    )
    val request = ReservationRequest(1000, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/8/2020"), 1)
    val (_, error) = new Hotel(rooms).reserve(request, existingRequests)
    assert(error.isDefined)
  }

  it should "respect max pet requirements" in {
    val existingRequests = Seq()
    val request = ReservationRequest(1000, 4, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/8/2020"), 3)
    val (_, error) = new Hotel(rooms).reserve(request, existingRequests)
    assert(error.isDefined)
  }

  it should "respect handicap access requirements" in {
    val existingRequests = Seq(
      ReservationRequest(100, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), 0, handicapAccessible = true),
      ReservationRequest(101, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), 0, handicapAccessible = true),
      ReservationRequest(102, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), 0, handicapAccessible = true),
      ReservationRequest(103, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/3/2020"), 0, handicapAccessible = true)
    )
    val request = ReservationRequest(1000, 1, dateFormatter.parse("1/1/2020"), dateFormatter.parse("1/8/2020"), 0, handicapAccessible = true)
    val (_, error) = new Hotel(rooms).reserve(request, existingRequests)
    assert(error.isDefined)
  }
}
