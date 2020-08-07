package com.bravolt

import java.util.Date

case class Room(beds: Int, floor: Int)

case class ReservationRequest(beds: Int,
                              arrival: Date,
                              departure: Date,
                              pets: Int = 0,
                              handicapAccessible: Boolean = false)

class Hotel(private val rooms: Seq[Room]) {

  private object EventType extends Enumeration {
    val Arrival: Value = Value("Arrival")
    val Departure: Value = Value("Departure")
  }

  // Make a seq of (date, Event (Arrival/Departure)) tuples, sorted by date
  // Iterate up to arrival date of incoming reservation, count + 1 for arrivals, -1 for departures
  // eligableRooms = all rooms filtered on reservation criteria
  // If eligableRooms.size > occupancyCount, success
  def reserve(request: ReservationRequest,
              existingRequests: Seq[ReservationRequest]): Boolean = {
    // Should an existing reservation "count against" the request? It should if the existing reservation requires a room
    // with the same criteria.
    def requestPredicate(existingRequest: ReservationRequest): Boolean = {
      val floor = if (requiresFirstFloor(existingRequest)) 1 else 2 // @todo Use a placeholder of sorts for floors other than 1
      roomPredicate(Room(existingRequest.beds, floor))
    }

    def roomPredicate(room: Room): Boolean = {
      // has pet or handicap, room fl 1 == (!true || true) = true
      // has pet or handicap, room fl 2 == (!true || false) = false
      // no pet or handicap, room fl 1  == (!false || true) = true
      // no pet or handicap, room fl 2  == (!false || false) = true
      if (request.pets > 2) false
      else request.beds <= room.beds && (!requiresFirstFloor(request) || room.floor == 1)
    }

    val events: Seq[(Date, EventType.Value)] = existingRequests
      .filter(requestPredicate)
      .foldLeft(Seq[(Date, EventType.Value)]()) { case (seq, request) =>
        seq ++ Seq((request.arrival, EventType.Arrival), (request.departure, EventType.Departure))
      }.filter(existingRequest => coincident(existingRequest._1, request.arrival)).sortBy(_._1)
    val occupancyCount: Int = events.foldLeft(0) { case (count, event) =>
      if (event._2 == EventType.Arrival) count + 1 else count - 1
    }
    val eligableRooms = rooms.filter(roomPredicate)
    eligableRooms.size > occupancyCount
  }

  private def requiresFirstFloor(request: ReservationRequest): Boolean = request.pets > 0 || request.handicapAccessible

  private def coincident(one: Date, two: Date): Boolean = (one.before(two) || one.equals(two))
}
