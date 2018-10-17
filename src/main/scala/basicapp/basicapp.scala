package basicapp

import java.io.File


object basicapp extends App {
  override def main(args: Array[String]): Unit = {
    println("\nHello World\n")
    val app1 = new helloworldprinter(95)
    val file = new File("symbiosis_failed_marker")
    app1.myloop()
    file.createNewFile()
  }

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
}

