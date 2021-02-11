// todo: "try/catch"

try {
  throw new RuntimeException
} catch {
  case _: IllegalArgumentException =>
  case e: RuntimeException => println(s"${e.getMessage()}")
}