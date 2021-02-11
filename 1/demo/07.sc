// todo: pattern matching

"12" match {
  case "123" => 123
  case _ => 0
}

123 match {
  case x if x % 2 == 0 => 1
  case x if x % 2 != 0 => 2
}

//todo: match everywhere
List(1, 2, 3) match {
  case Nil => 1
  case x :: xs => 2
  case _ =>
}

val (x :: xs) = List(1,2,3)

//todo: name shadowing

