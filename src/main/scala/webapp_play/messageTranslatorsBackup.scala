/***
package cota.bidness.integration

import java.io.{ File, InputStream, InputStreamReader, PrintWriter }

import cats.effect.{ IO, LiftIO }
import cats.implicits._
import com.Ostermiller.util.CircularByteBuffer
import com.amazonaws.services.s3.model.S3ObjectInputStream
import com.github.tototoshi.csv.{ CSVReader, CSVWriter }
import cota.model.integration.document._
import cota.model.integration.errors._
import cota.model.integration.source._
import cota.model.integration.storage._
import cota.organs.aws.s3.{ S3Client, S3Location }
import cota.organs.fs2._
import cota.organs.logging.LoggingIO._
import cota.organs.pdf.textToPDF
import fs2._
import mouse.boolean._

object MessageTranslators {
  //begin to delete
  private def localWrite(input1: InputStream): Unit = {
    val fd = new File("COTA_OnBase_test_staging_123abc_fuckall.csv")
    val out = new PrintWriter(fd)
    val in2 = scala.io.Source.fromInputStream(input1)
    try {
      in2.getLines().foreach(out.println(_))
    } finally {
      out.close()
    }
    in2.close()
  }
  //end to delete

  private val logF = LiftIO[IO]

  implicit class CsvTranslateMessageOps(val message: CsvTranslateMessage) extends AnyVal {

    private def translateCsv(s3: S3Client): IO[MultipleMessage] = {
      val source = message.location
      val destination: S3Location = source.withModifiedFilename(_.replaceAll(".txt", ".csv"))

      //begin to remove
      val row0 = List("a1", "a2", "a3")
      val row1 = List("b1", "b2", "b3")
      val row2 = List("c1", "c2", "c3")
      val row3 = List("d1", "d2", "d3")
      val mytext: List[List[String]] = List(row0, row1, row2, row3)
      val destination2 = source.withModifiedFilename(_.replaceAll(".txt", "_12345abcdef.csv"))
      //end to remove

      for {
        mapper <- MetadataMapping.forSource(message.translation_scheme)
        stream <- IO.fromEither(s3.getObject(source)).map(o => new InputStreamReader(o.getObjectContent))
        _ <- logF.info(s"Fetched manifest to translate [${message.translation_scheme}|$source]")
        //        _ <- logF.info(" ******** CHEW THROWING NOW ********* ")
        //        _ <- IO.raiseError(new Exception(" ***********  CHEW THROWING *********** "))

        reader <- IO(CSVReader.open(stream))
        result = mapper match {
          case m: MappingWithHeaders    => reader.toFS2StreamWithHeaders[IO].map(m.from(message.entity)(_))
          case m: MappingWithoutHeaders => reader.toFS2Stream[IO].map(m.from(message.entity)(_))
        }

        rows <- result.compile.toList.flatMap(_.sequence[IO, Manifest])
        _ <- logF.info(s"Translated manifest [${message.translation_scheme}|$source] with ${rows.length} rows")
        buffer <- IO(new CircularByteBuffer(CircularByteBuffer.INFINITE_SIZE))
        _ <- logF.info(s"Opened buffer for [${message.translation_scheme}|$source]")
        out <- IO(buffer.getOutputStream)
        writer <- IO(CSVWriter.open(out))
        _ <- logF.info(s"Created writer for [${message.translation_scheme}|$source]")

        _ <- IO(rows.map(_.values).foreach(writer.writeRow))
        _ <- logF.info(s"Wrote ${rows.length} rows to CSV for [${message.translation_scheme}|$source]")
        in <- IO(buffer.getInputStream)
        _ <- IO(out.close())
        _ <- logF.info(s"Acquired input stream for [${message.translation_scheme}|$source], sending to ${destination.url}")
        _ <- s3.putStream(destination, in)
        //        _ <- IO(localWrite(in))

        _ <- logF.info(s"Wrote translated manifest to ${destination.url}")
        _ <- IO(in.close())

        result = MultipleMessage(message.entity, destination)

        //begin to remove
        //        _ <- logF.info("***** CHEW: Circular byte buffer test *****\n")
        //        buffer2 <- IO(new CircularByteBuffer(CircularByteBuffer.INFINITE_SIZE))
        //        out2 <- IO(buffer2.getOutputStream)
        //        writer2 <- IO(CSVWriter.open(out2))
        //        _ <- logF.info("***** CHEW: Circular byte buffer test: writing to BUFFER *****\n")
        //        _ <- IO(mytext.foreach(writer2.writeRow))
        //        in2 <- IO(buffer2.getInputStream)
        //        _ <- IO(out2.close())
        //        //        _ <- logF.info(s"***** CHEW: Circular byte buffer test: writing to S3 object: ${destination2.url} *****\n")
        //        //        _ <- s3.putStream(destination2, in2)
        //        _ <- logF.info("***** CHEW: Circular byte buffer test: writing to disk")
        //        _ <- IO(localWrite(in2))
        //        _ <- IO(in2.close())
        //end to remove
      } yield result
    }

    def processCsv(s3: S3Client): Stream[IO, MultipleMessage] = Stream.eval(translateCsv(s3))
  }

  implicit class MultipleMessageOps(val message: MultipleMessage) extends AnyVal {

    def toSingularStream(s3: S3Client): S3ObjectInputStream => Stream[IO, SingularMessage] =
      (input: S3ObjectInputStream) => {

        def asCsv: fs2.Pipe[IO, S3ObjectInputStream, List[String]] =
          in => in.flatMap { in =>
            val io =
              for {
                reader <- IO(new InputStreamReader(input))
                csv <- IO(CSVReader.open(reader))
                output <- csv.toFS2Stream[IO].pure[IO]
              } yield output

            io.unsafeRunSync()
          }

        def asMetadata: fs2.Pipe[IO, List[String], Manifest] =
          in => in.flatMap { columns =>
            Stream
              .eval(logF.info(s"Parsing metadata[${message.location.bucket}|${message.location.key}]"))
              .evalMap(_ => Manifest.from(columns))
          }

        def asSingular: fs2.Pipe[IO, Manifest, SingularMessage] =
          in => in.flatMap { metadata =>

            val createMessage: String => IO[SingularMessage] = (name: String) => for {
              source <- IO(S3Location(message.location.bucket, name))
              _ <- logF.info(s"Create singular message for ${source.url}")
              exists <- IO(s3.exists(source.bucket, source.key))
              _ <- logF.trace(s"Validated ${source.url} exists")
              destination = {
                val bucketProps = BucketProps(source.bucket)
                S3Location(bucketProps.toBucketName, S3DestinationKey.forDoc(source.keyFilename))
              }
              message <- IO.fromEither(exists.either(SourceMissing(source), SingularMessage(message.entity, metadata, source, destination)))
            } yield message

            (metadata.file_name, metadata.content) match {
              case (None, Some(content)) => Stream.eval {
                for {
                  _ <- logF.debug(s"Converting text to PDF [${message.location.url}|${metadata.source_id}]")
                  name = ""
                  pdf <- textToPDF(metadata.document_name, content)
                  _ <- logF.debug(s"PDF successfully converted [${message.location.url}|${metadata.source_id}]")
                  _ <- s3.putStream(message.location.withSiblingFilename(name), pdf.getInputStream)

                  _ <- logF.info(s"Composing singular message [${message.location.url}|${metadata.source_id}]")
                  message <- createMessage(name)

                } yield SingularMessage(message.entity, metadata, message.srcLocation, message.dstLocation)
              }

              case (Some(name), None) => Stream.eval {
                for {
                  _ <- logF.info(s"Composing singular message [${message.location.url}|${metadata.source_id}]")
                  message <- createMessage(name)
                  _ <- logF.info(s"Message created[$message]")
                } yield message
              }

              case (_, _) =>
                Stream.raiseError(InvalidMetadata(metadata.values))
            }
          }

        Stream
          .eval(input.pure[IO])
          .through(asCsv)
          .through(asMetadata)
          .through(asSingular)
      }
  }
}
*/