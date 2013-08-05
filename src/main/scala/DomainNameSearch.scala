import dispatch._
import dispatch.Defaults._
import scala.xml._
import pickling._
import pickling.json._
import sys.process._
import org.apache.commons.io.FileUtils
import java.io.File
import sys.process._

object DomainNameSearch extends App {
  val words = {
    val url =
      getClass.getResource("brit-a-z.txt")
    val file = new File(url.getFile)
    val string = FileUtils.readFileToString(file)
    println(string.take(100))
    string.split("[ \r\n]").filter(_.size > 0).map(_.toLowerCase).toList
  }

  var freeDomains =
    collection.mutable.Map[String, collection.immutable.TreeSet[String]]()

  def findDomains(superDomain: String) {

    //  val regex = """.*me$""".r
    //    val superDomain = "al"
    //    val proxy = new com.ning.http.client.ProxyServer("127.0.0.1", 9151)

    val regex = (".*" ++ superDomain ++ "$").r

    val matches = for (
      word <- words;
      if (regex.pattern.matcher(word).matches)
    ) yield {
      word
    }

    def makeDomain(word: String): String =
      word.dropRight(superDomain.size) ++ "." ++ superDomain

    def domainIsFree(domain: String): Boolean = {
      val whois = s"torsocks whois ${domain}" !!

      println(whois)

      assert(whois.size > 3, whois.size)
      assert(
        !whois.toLowerCase.contains("exceeded"),
        s"Lookup quota exceeded: ${domain}")

      (!whois.toLowerCase.contains("registrant")) &&
        (!whois.toLowerCase.contains("incorrect")) &&
        (!whois.toLowerCase.contains("live")) &&
        (!whois.toLowerCase.contains("not available")) &&
        (!whois.toLowerCase.contains("suspended")) &&
        (!whois.toLowerCase.contains("expiration")) &&
        (!whois.toLowerCase.contains("banned")) &&
        (!whois.toLowerCase.contains("registrant")) &&
        (!whois.toLowerCase.contains("owner")) &&
        (!whois.toLowerCase.contains("reserved")) &&
        (!whois.toLowerCase.contains("active"))

    }

    for (word <- matches) {
      val domain = makeDomain(word)

      val lastWord = ""

      if (word >= lastWord) {
        val isFree = domainIsFree(domain)
        println(s"$domain is free: ${isFree}")

        if (isFree) {
          val newSet =
            freeDomains.getOrElse(
              superDomain,
              collection.immutable.TreeSet[String]()) + domain
          freeDomains += superDomain -> newSet
          println(freeDomains)
        }
        
//        Thread.sleep(60000)
      }
    }

    println(freeDomains)
    println(s"finished searching for $superDomain")
  }

  val genericTLDs = IndexedSeq(
    //    "me",
    //    "info",
    //    "org",
    //    "com",
    //    "co",
    //    "name",
    "net")

  val googleGenericCCTLDs = IndexedSeq(
    //    "ad",
//    "as" 
      //    "bz",
    //    "cc",
    //    "cd",
    //    "co",
    //    "dj",
    //    "fm",
    //    "io"
    //    "la"
        "me"
    //    "ms",
    //    "nu",
    //    "sc",
    //    "sr",
    //    "su",
    //    "tv",
    //    "tk",
    //    "ws"
    )

  val inList = IndexedSeq(
    "ac",
    "ae",
    "af",
    "ag",
    "ai",
    "al",
    "am",
    "ao",
    "ar",
    "as",
    "at",
    "ax",
    "az",
    "ba",
    "bb",
    "bd",
    "be",
    "bf",
    "bg",
    "bh",
    "bi",
    "bj",
    "bn",
    "bo",
    "bs",
    "bt",
    "bw",
    "by",
    "bz",
    "cc",
    "cd",
    "cf",
    "cg",
    "ch",
    "ci",
    "ck",
    "cl",
    "cr",
    "cu",
    "cv",
    "cx",
    "cy",
    "cz")

  val inString = "dj dk dm do dz ec ee eg eh es et fj fk fm fo ga gd gf gg gh gi gl gm gq gr gs gt gu gw gy hm hn hr ht id " +
    "il im io iq ir is je jm jo ke kg kh ki km kn kp kr kw kz la lc li lk lr ls lt lu lv ly ma md me mg mh ml mm mn"

  val domainsToCheck = googleGenericCCTLDs
  //genericTLDs ++ googleGenericCCTLDs 
  //    inList ++ inString.split(" ")

  domainsToCheck foreach (findDomains)
}
