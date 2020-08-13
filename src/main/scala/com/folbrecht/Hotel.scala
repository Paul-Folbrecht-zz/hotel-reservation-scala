package com.folbrecht

import java.util.Date

case class Room(number: Int,
                beds: Int,
                floor: Int)

case class ReservationRequest(id: Int,
                              beds: Int,
                              arrival: Date,
                              departure: Date,
                              pets: Int = 0,
                              handicapAccessible: Boolean = false)

class Hotel(private val rooms: Seq[Room]) {

  private object EventType extends Enumeration {
    val Arrival: Value = Value("Arrival")
    val Departure: Value = Value("Departure")
  }

  def reserve(request: ReservationRequest,
              existingRequests: Seq[ReservationRequest]): (Double, Option[String]) = {
    // Pre-assigning rooms to reservations has negatives, so our model will not work that way: We'll determine the set of
    // rooms relevant for occupancy dynamically. Furthermore, determining validity of a reservation will involve running a
    // simulation of arrivals and departures to ensure that there will be a suitable room available on every day of the stay.
    val eligibleRooms = rooms.filter(room => roomPredicate(request, room))
    if (eligibleRooms.isEmpty) return (0, Option("No eligible rooms")) // Area for improvement: Specific error messages

    // Create a sequence of arrival/departure events
    val events: Seq[(Date, EventType.Value, ReservationRequest)] = existingRequests
      .foldLeft(Seq[(Date, EventType.Value, ReservationRequest)]()) { case (seq, request) =>
        seq ++ Seq((request.arrival, EventType.Arrival, request), (request.departure, EventType.Departure, request))
      }
      .filter(event => coincident(event._1, request.departure))
      .sortBy(_._1)

    // Run the simulation
    var availableRooms = rooms.toSet
    var occupancies: Map[ReservationRequest, Room] = Map()
    events.foreach { event =>
      if (event._2 == EventType.Arrival) {
        getRoom(event._3, availableRooms) match {
          case None => return (0, Option("Overbooked"))
          case Some(room) =>
            occupancies += event._3 -> room
            availableRooms = availableRooms.filterNot(_ == room)
        }
      } else {
        availableRooms += occupancies(event._3)
        occupancies -= event._3
      }

      // Now check for the request
      getRoom(request, availableRooms) match {
        case None => return (0, Option("Overbooked"))
        case Some(_) =>
      }
    }

    (cost(request), None)
  }

  def roomPredicate(request: ReservationRequest, room: Room): Boolean = {
    if (request.pets > 2) false
    else {
      val firstFloorRequirement = (requiresFirstFloor(request) && room.floor == 1) || (!requiresFirstFloor(request) && room.floor > 1)
      request.beds == room.beds && firstFloorRequirement
    }
  }

  private def getRoom(request: ReservationRequest, rooms: Set[Room]): Option[Room] = rooms.find(room => roomPredicate(request, room))

  private def requiresFirstFloor(request: ReservationRequest): Boolean = request.pets > 0 || request.handicapAccessible

  private def coincident(one: Date, two: Date): Boolean = (one.before(two) || one.equals(two))

  private def cost(request: ReservationRequest): Double = {
    val base = {
      if (request.beds == 1) 50
      else if (request.beds == 2) 75
      else 90
    }
    base + (request.pets * 20)
  }
}
