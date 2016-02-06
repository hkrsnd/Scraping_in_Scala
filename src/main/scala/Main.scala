import collection.JavaConverters._
import com.flyberrycapital.slack.SlackClient
import java.io.PrintWriter
import org.jsoup._
import org.jsoup.nodes._
import scala.io.Source
import scala.util.matching._

object Main {
  var max_id = 0
  val baseurl = "http://www.lancers.jp"
  val doc = Jsoup.connect("http://www.lancers.jp/work/search/system/development?completed=0&money_max=100000&sort=Work.started&direction=desc")
      .userAgent("Mozilla/5.0 (Windows; U; WindowsNT 5.1; en-US; rv1.8.1.6) Gecko/20070725 Firefox/2.0.0.6").get()

  def main(args: Array[String]): Unit = {
    var id = 0
    var new_project_url = ""
    var new_project_text = ""
    var post_text = ""
    var new_project_prices = ("", "")

    max_id = getMaxId()
    id = doc.getElementsByTag("p").attr("id").drop(5).toInt // id = work_877762 先頭5文字drop
    if (id > max_id) {
      writeMaxId(id)
      new_project_url = getNewProjectUrl()
      new_project_text = getNewProjectText(new_project_url)
      new_project_prices = getProjectPrice(new_project_url)
      post_text = new_project_url ++ "\n" ++ new_project_prices._1 ++ "¥" ++ " ~ " ++ new_project_prices._2 ++ "¥" ++ "\n" ++ new_project_text
      if (!isContainingNG(post_text)) {
        println(new_project_url)
        println(new_project_text)
        Slack.post(post_text)
      } else {
        println("containing NG")
      }
    } else {
      println("nope " ++ max_id.toString)
    }
  }

  def getNewProjectUrl(): String = {
    val new_project_url = baseurl ++ doc.getElementsByClass("work_title").asScala.map(x => x.getElementsByTag("a").attr("href")).head
    new_project_url
  }

  def getNewProjectText(new_project_url: String): String = {
    val new_doc = Jsoup.connect(new_project_url)
      .userAgent("Mozilla/5.0 (Windows; U; WindowsNT 5.1; en-US; rv1.8.1.6) Gecko/20070725 Firefox/2.0.0.6").get()
    val new_project_title = try {
      new_doc.getElementsByClass("suggestion").first.text
    } catch { case e: java.lang.NullPointerException => "限定公開のしごと"}
    var new_project_comment = try {
      new_doc.getElementsByClass("comment").text.foldLeft("")((l, r) => l.toString ++ r.toString)
    } catch { case e: java.lang.NullPointerException => ""}

     //if (new_project_comment == "") {
     //val new_project_comment = new_doc.getElementsByClass("pad_l30").text.foldLeft("")((l, r) => l.toString ++ r.toString).replace("<.*>", "")
      //}
    new_project_title ++ "\n" ++ new_project_comment
  }

  def getProjectPrice(new_project_url: String): (String, String) = {
    val new_doc = Jsoup.connect(new_project_url)
      .userAgent("Mozilla/5.0 (Windows; U; WindowsNT 5.1; en-US; rv1.8.1.6) Gecko/20070725 Firefox/2.0.0.6").get()
    val price_span = new_doc.getElementsByClass("price-number").asScala.map(x => x.text) 
    try {
      (price_span.head , price_span.tail.head)
    } catch { case e: java.util.NoSuchElementException => ("?", "?")}
  }

  def isContainingNG(contents: String): Boolean = {
    val p1 = "常駐"
    val r1 = p1.r

		if (r1.findFirstIn(contents) != None) true
    else false
  }

  def getMaxId(): Int = {
    val s = Source.fromFile("maxid.txt")
    try {
      val id = for (i <- s.getLines) yield {
        i
      }
      id.toList.head.toInt
    } finally {
      s.close
    }
  }

  def writeMaxId(new_max_id: Int): Unit = {
    val file = new PrintWriter("maxid.txt")
    file.write(new_max_id.toString)
    file.close
  }
}

object Slack {
  val s = new SlackClient("xoxp-19896831492-19900049315-20237606272-31cff2a970")
  s.connTimeout(1000)
  s.readTimeout(5000)
  
  def post(new_project_url: String): Unit = {
    s.chat.postMessage("#piimemo", new_project_url)
  }
}
