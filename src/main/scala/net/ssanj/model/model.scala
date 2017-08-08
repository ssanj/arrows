package net.ssanj.model

final case class Name(first: String, last: String)

final case class Age(age: Int)

final case class Person(name: Name, age: Age)

final case class Config(name: String, age: Int)

final case class User(name: String, id: String)

final case class ItemId(id: Long)

final case class ItemDescReq(itemId: Long, userId: String)

final case class ItemDetail(itemId: Long, value: Double, desc: String)

final case class ValuableItemsResponse(expensive: List[ItemDetail], veryExpensive: List[ItemDetail])

final case class Price(price: Double)