package fs2_play

import java.io.{File, InputStream, OutputStream, PrintWriter}
import java.nio.file.{CopyOption, Files}

import fs2._
import cats.effect.IO

import scala.concurrent.Future
import scala.io.{BufferedSource, Source, StdIn}
import com.Ostermiller.util.CircularByteBuffer
import com.github.tototoshi.csv.CSVWriter

object helloworld {
  def main(args: Array[String]): Unit = {
    println("Hello world!")
//    val outstr = "the quick brown fox"
    val x = new helloworld
//    x.mywriter(outstr)

    val row0 = List("a1", "a2", "a3")
    val row1 = List("b1", "b2", "b3")
    val row2 = List("c1", "c2", "c3")
    val row3 = List("d1", "d2", "d3")
    val rowsIn: List[List[String]] = List(row0, row1, row2, row3)

    x.writeCircByteBuffer(rowsIn)
  }
}

class helloworld {
  def mywriter(mystr: String): Unit = {
    // PrintWriter
    import java.io._
    val pw = new PrintWriter(new File("hello.txt" ))
    pw.write(mystr)
    pw.close()
  }

  def writeCircByteBuffer(in: List[List[String]]): Unit = {
    println("Opening CircularByteBuffer\n")
    val buffer: CircularByteBuffer = new CircularByteBuffer(CircularByteBuffer.INFINITE_SIZE)
    println("Acquiring output stream\n")
    val outputStream: OutputStream = buffer.getOutputStream
    println("Creating writer\n")
    val writer: CSVWriter = CSVWriter.open(outputStream)
    println("Writing rows to buffer\n")
    in.foreach(writer.writeRow)

    println("Acquiring input stream\n")
    val inputStream: InputStream = buffer.getInputStream
    outputStream.close()


    // Write from inputstream to a file.. this section differs from codebase
    val fd = new File("CircByteBufferWrite999999999.txt")

    val out: PrintWriter = new PrintWriter(fd)

    println("Converting inputStream to BufferedSource\n")
    val in2: BufferedSource = scala.io.Source.fromInputStream(inputStream)

    println("Writing from BufferedSource to file")
    try {
      in2.getLines().foreach(out.println(_))
    } finally {
      out.close()
    }

    in2.close()

    writer.close()
    println("Complete")
  }
}
