//create Array
val a: Array[String] = new Array[String](3)

//fill a, apply
a(0) = "asdf"
a(1) = "123"

//print a, update
a.update(0, "sadf")
println(a(0))