class HashTable[T] (var length: Int, threshold: Float=0.75) {
  type HashCell = Option[T]

  private var values: Array[HashCell] = Array.fill(length)(None: HashCell)
  private var used: Int = 0 


  private def insert(index: Int, value: T): Unit = {
    values.update(index, Option(value))
    used += 1
  }

  private def rehash(): Unit = {
    var oldValues = values

    length = length * 2
    used = 0
    values =  Array.fill(length)(None: HashCell)

    for (value <- oldValues) {
      if (value.nonEmpty) {
        insert(value.get)
      }
    }
  }

  private def insert_after(index: Int, value: T): Unit = {
    var i = (index + 1) % length
    while (true) {
      if (values(i).isEmpty) {
        insert(i, value)
        return
      }
      i = (i + 1) % length
    }
  }

  def hash(a: T): Int = {
    return a.hashCode % length
  }

  def insert(value: T): Unit = {
    if ((used.toFloat / length.toFloat) >= threshold) {
      rehash()
    }

    val index = hash(value)
    if (values(index) != None) {
      insert_after(index, value)
    } else {
      insert(index, value)
    }
  }

  def get(key: T): Option[T] = {
    val startIndex = hash(key)
    var index = startIndex
    
    while  {
      if (values(index).get == key) {
        return values(index)
      }
      index = (index + 1) % length
      index != startIndex
    } do ()

    return Option.empty
  }

  def print(): Unit = {
    println(s"==================")

    for ((value, i) <- values.zipWithIndex) {
      if (value.nonEmpty) {
        println(s"$i : $value")
      }
    }
    println(s"Length: $length")
    println(s"Used: $used")
    println(s"Thresh: $threshold")
    println(s"==================")
  }
}

@main def hello: Unit = 
  println("Test starting..")
  val values = Array(10, 20, 30, 40, 50, 10, 10, 10, 10, 10, 11, 12, 13, 14, 15, 16, 20)
  val hs = HashTable[Int](8, 1.0)

  for (value <- values) {
    hs.insert(value)
  }

  hs.print()

  println("- Get values")
  for (value <- values) {
    println(s"Value: ${hs.get(value)}")
  }


def msg = "I was compiled by Scala 3. :)"
