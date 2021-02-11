import java.io.File

//Local functions
def foo(): Unit = {
  def bar(): Unit = {

  }
  bar()
}

//More about functions:

//todo: repeated parameters
def foo1(a: Int*) = a

foo1(1,2,3)
foo1()

//todo: named parameters

//todo: default arguments

//todo: by-name parameters
def foo2(a: =>String) {
  println("foo2")
  println(a)
}

def str() = {
  println("str")
  "foo"
}

foo2(str)

//todo: currying

def withResources(f: File)(body: => Unit)

val func = withResources(???)(_)
