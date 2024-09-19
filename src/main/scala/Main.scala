@main
def hello(): Unit =
  println("Hello world!")
  println(msg)
  foobar(5)

def foobar(x: Int): Unit = {
  println(x)
}

def msg = "I was compiled by Scala 3. :)"
