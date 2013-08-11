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
import scala.util.{ Try, Success, Failure }
import scala.concurrent._

class Conf(args: Seq[String]) extends ScallopConf(args) {
  val topLevelDomains = opt[List[String]](
    default = Some(Nil))

  val dataRoot = opt[String](
    required = true) map (new File(_))

  val sleepSeconds = opt[Int](
    required = true)

  val ccTLDs = opt[Boolean](
    default = Some(false))
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
    if (domain == "." + topLevelDomain) {
      println("short domain, skipping")
      "invalid"
    } else {
      println(s"Running whois on: $domain")
      val whois = s"torsocks whois ${domain}" !!

      println(whois)

      if (whois.size <= 3) throw new DownloadFailedException

      if (whois.toLowerCase.contains("exceeded") || whois.toLowerCase.contains("denied")) throw new LookupQuotaExceededException

      Thread.sleep(sleepSeconds * 1000)

      whois
    }
  }

  def rawPath(dataRoot: File, topLevelDomain: String) =
    new File(dataRoot, "rawWhois/" + topLevelDomain)

  def gatherWhois(topLevelDomain: String, dataRoot: File, sleepSeconds: Int) {
    val regex = (".*" ++ topLevelDomain ++ "$").r

    val matches = for (
      word <- words;
      if (regex.pattern.matcher(word).matches)
    ) yield {
      word
    }

    val domainRoot = rawPath(dataRoot, topLevelDomain)
    if (!domainRoot.exists) {
      domainRoot.mkdir
    }

    for (word <- matches.filter(!badWords.contains(_))) {
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

            Try("killall tor" !)

            Thread.sleep(1000)

            future { "tor" ! }

            Thread.sleep(7 * 1000)

            println("restarted tor")

            //            Thread.sleep(10 * 1000 * 60)
            gatherWhois(topLevelDomain, dataRoot, sleepSeconds)
        }
      }
    }
  }

  def classifyWhois(whois: String): Option[Boolean] = {
    def has(string: String): Boolean = whois.toLowerCase.contains(string)

    if (has("not registered") ||
      has("not found") ||
      has("no entries found") ||
      has("no data found") ||
      has("domain status: available") ||
      has("no match") ||
      has("nothing found") ||
      has("no records matching") ||
      has("do not have an entry") ||
      has("no contact information") ||
      has("no such domain") ||
      has("is available")) Some(true)
    else if (has("registrant") ||
      has("expiry") ||
      has("domain reserved") ||
      has("you aren't allowed") ||
      has("owner contact") ||
      has("domain holder") ||
      has("is not supported") ||
      has("invalid") ||
      has("found 1 record") ||
      has("found 2 record") ||
      has("found 3 record") ||
      has("found 4 record") ||
      has("found 5 record") ||
      has("found 6 record") ||
      has("found 30 record") ||
      has("found 94 record") ||
      has("found 75 record") ||
      has("found 247 record") ||
      has("found 230 record") ||
      has("syntax error") ||
      has("fecha de registro") ||
      has("solo acepta consultas con dominios .bo") ||
      has("reg-excluded") ||
      has("reg-reserved") ||
      has("status: active") ||
      has("holder of domain") ||
      has("incorrect domain name") ||
      has("banned") ||
      has("administrative contact") ||
      has("expires") ||
      has("created") ||
      has("not available")) Some(false)
    else None
  }

  val badWords = "reformist"

  lazy val ccTLDs = {
    val badTopLevelDomains = "ai al an ao aq ar au aw bm br bv ca cm cn cs dd de eh er eu fr gb ge gn hu ie it jp ky lb mo mk mo my no om pm re sa sk sm ss sz td tf tz ua va wf yt yu bb bi bs bt ci ck do es et gh gi gu hm hn id il kh lc lk ls mm mp nc ne ng ni pa ph py sn so sy tc th ye za".split(" ")

    val url =
      getClass.getResource("ccTLDs.txt")
    val file = new File(url.getFile)
    val string = FileUtils.readFileToString(file)
    val list = string.split("[ \r\n]").filter(_.size > 0).toList
    val all = list.map(_.replace("[", "").replace("]", "").replace(".", ""))
    all.filter(!badTopLevelDomains.contains(_))
  }
}

object DomainNameSearch {
  def main(unparsedArgs: Array[String]) {
    val args = new Conf(unparsedArgs)
    println(args.summary)

    val topLevelDomains = {
      val extraTopLevelDomains = if (args.ccTLDs()) {
        GatherData.ccTLDs
      } else Nil

      args.topLevelDomains() ++ extraTopLevelDomains
    }

    if (topLevelDomains.size > 0) {
      Try("killall tor" !)

      Thread.sleep(1000)

      future { "tor" ! }

      Thread.sleep(7 * 1000)

      println("restarted tor")
    }

    for (topLevelDomain <- topLevelDomains) {
      println(s"Processing $topLevelDomain")

      GatherData.gatherWhois(topLevelDomain, args.dataRoot(), args.sleepSeconds())

      println(s"Finished $topLevelDomain")
    }

    s"rm -rf ${args.dataRoot()}/true" !
    
    s"rm -rf ${args.dataRoot()}/maybe" !
    
    s"rm -rf ${args.dataRoot()}/false" !
    
    for (
      domainRoot <- new File(args.dataRoot(), "rawWhois").listFiles;
      if domainRoot.isDirectory;
      whoisFile <- domainRoot.listFiles;
      if whoisFile.isFile
    ) {
      val whois = FileUtils.readFileToString(whoisFile)

      def write(result: String) {
        val outputFile = new File(whoisFile.getPath.replace("rawWhois", result))
        if (!outputFile.getParentFile.exists) outputFile.getParentFile.mkdir
        FileUtils.writeStringToFile(outputFile, whois)
      }

      GatherData.classifyWhois(whois) match {
        case Some(true) => write("true")
        case Some(false) => write("false")
        case None => write("maybe")
      }
    }

    println("all done")
  }
}
