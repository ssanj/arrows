package net.ssanj.arrow

import scala.collection.immutable.Range
import net.ssanj.model._

object Functions {

  type UserData = Map[String, List[ItemId]]
  type ItemData = Map[Long, ItemDetail]

  val userData = Map[String, List[ItemId]](
    "1000" -> List(ItemId(1001), ItemId(1002), ItemId(1003), ItemId(1007), ItemId(1004)),
    "2000" -> List(ItemId(2001), ItemId(2002))
  )

  val itemData = Map[Long, ItemDetail](
    1001L -> ItemDetail(1001, 2000.00,  "Couch"),
    1002L -> ItemDetail(1002, 100.00,   "Apple TV"),
    1003L -> ItemDetail(1003, 75000.00, "Luxury Car"),
    1004L -> ItemDetail(1004, 3000,     "Laptop"),
    2001L -> ItemDetail(2001, 1500.00,  "Coffee Machine"),
    2002L -> ItemDetail(2002, 500.00,   "DLSR")
  )

  val getSavedItems: User => UserData => List[ItemId] = user => data => data.getOrElse(user.id, Nil)

  val idToDesc: User => ItemId => ItemDescReq = user => itemId => ItemDescReq(itemId.id, user.id)

  val getDetails: ItemDescReq => ItemData => Either[String, ItemDetail] = itemDescReq => data =>
    data.get(itemDescReq.itemId).toRight(s"could not find item with id: ${itemDescReq.itemId}")

  val isExpensive: Range => ItemDetail => Boolean = range => item => range.contains(item.value)

  val valuableItemsResponse : Tuple2[Range, Range] => List[ItemDetail] => ValuableItemsResponse = prices => items =>
    ValuableItemsResponse(items.filter(isExpensive(prices._1)), items.filter(isExpensive(prices._2)))

  val valuableItemsResponseString: ValuableItemsResponse => String = items => {
    s"expensive:${itemDetailString(items.expensive)},veryExpensive:${itemDetailString(items.veryExpensive)}"
  }

  val itemDetailString: List[ItemDetail] => String = _.map(id => s"${id.desc}=$$${id.value}").mkString(",")

  val errorString: List[Either[String, ItemDetail]] => String = itemsE =>
    itemsE.collect { case Left(error) => error } mkString("\n")
}