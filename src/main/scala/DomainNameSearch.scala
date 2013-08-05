import dispatch._
import dispatch.Defaults._
import scala.xml._
import pickling._
import pickling.json._
import sys.process._
import org.apache.commons.io.FileUtils
import java.io.File
import sys.process._
import org.rogach.scallop._
import java.io.File

class Conf(args: Seq[String]) extends ScallopConf(args) {
  val topLevelDomains = opt[List[String]](
    default = Some(Nil))

  val dataRoot = opt[String](
    required = true) map (new File(_))

  val sleepSeconds = opt[Int](
    required = true)
}

class DownloadFailedException extends Exception

class LookupQuotaExceededException extends Exception

object GatherData {
  lazy val words = {
    val url =
      getClass.getResource("brit-a-z.txt")
    val file = new File(url.getFile)
    val string = FileUtils.readFileToString(file)
//    println(string.take(100))
    string.split("[ \r\n]").filter(_.size > 0).map(_.toLowerCase).toList
  }

  def gatherSingleWhois(topLevelDomain: String, word: String, sleepSeconds: Int): String = {
    def makeDomain(word: String): String =
      word.dropRight(topLevelDomain.size) ++ "." ++ topLevelDomain

    val domain = makeDomain(word)
    println(s"Running whois on: $domain")
    val whois = s"torsocks whois ${domain}" !!

    println(whois)

    if (whois.size <= 3) throw new DownloadFailedException

    if (whois.toLowerCase.contains("exceeded")) throw new LookupQuotaExceededException

    Thread.sleep(sleepSeconds * 1000)

    whois
  }

  def gatherWhois(topLevelDomain: String, dataRoot: File, sleepSeconds: Int) {
    val regex = (".*" ++ topLevelDomain ++ "$").r

    val matches = for (
      word <- words;
      if (regex.pattern.matcher(word).matches)
    ) yield {
      word
    }

    val domainRoot = new File(dataRoot, topLevelDomain)
    if (!domainRoot.exists) {
      domainRoot.mkdir
    }

    for (word <- matches) {
      val savePath = new File(domainRoot, word)
      if (!savePath.exists) {
        try {
          val whois = gatherSingleWhois(topLevelDomain, word, sleepSeconds)
          FileUtils.writeStringToFile(savePath, whois)
        } catch {
          case _: DownloadFailedException =>
            println("download failed")
            Thread.sleep(10 * 1000 * 60)
            gatherWhois(topLevelDomain, dataRoot, sleepSeconds)
          case _: LookupQuotaExceededException =>
            println("lookup quota exceeded")
            Thread.sleep(10 * 1000 * 60)
            gatherWhois(topLevelDomain, dataRoot, sleepSeconds)
        }
      }
    }
  }
}

object DomainNameSearch {
  def main(unparsedArgs: Array[String]) {
    val args = new Conf(unparsedArgs)
    println(args.summary)

    for (topLevelDomain <- args.topLevelDomains()) {
      println(s"Processing $topLevelDomain")

      GatherData.gatherWhois(topLevelDomain, args.dataRoot(), args.sleepSeconds())
      
      println(s"Finished $topLevelDomain")
    }
  }
}
