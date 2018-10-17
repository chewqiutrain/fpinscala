package basicapp


class helloworldprinter(start: Int) {
  def myloop() = {
    var i = start
    while (i < 100){
      println(s"Current i = $i")
      i = i + 1
      Thread.sleep(1000)
    }
  }
}
