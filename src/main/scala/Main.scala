import collection.JavaConverters._
import org.jsoup._
import org.jsoup.nodes._
import scala.util.matching._

object Main {
  var max_id = 0
  val baseurl = "http://www.lancers.jp"
  val doc = Jsoup.connect("http://www.lancers.jp/work/search/system?completed=1&money_min=100&money_max=100000&sort=Work.budget_from&direction=asc&")
      .userAgent("Mozilla/5.0 (Windows; U; WindowsNT 5.1; en-US; rv1.8.1.6) Gecko/20070725 Firefox/2.0.0.6").get()

  def main(args: Array[String]): Unit = {
    val id = doc.getElementsByTag("p").attr("id").drop(5).toInt // id = work_877762 先頭5文字drop
    var new_project_url = ""
    var new_project_text = ""
    var post_text = ""
    var new_project_prices = ("", "")

    if (id > max_id) {
      new_project_url = getNewProjectUrl()
      new_project_text = getNewProjectText(new_project_url)
      new_project_prices = getProjectPrice(new_project_url)
      post_text = new_project_url ++ "\n" ++ new_project_prices._1 ++ " ~ " ++ new_project_prices._2 ++ "\n" ++ new_project_text
      if (!isContainingNG(post_text)) {
        println(new_project_url)
        println(new_project_text)
        Slack.post(post_text)
      }
    }
  }

  def getNewProjectUrl(): String = {
    val new_project_url = baseurl ++ doc.getElementsByClass("work_title").asScala.map(x => x.getElementsByTag("a").attr("href")).tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head
    new_project_url
  }

  def getNewProjectText(new_project_url: String): String = {
    val new_doc = Jsoup.connect(new_project_url)
      .userAgent("Mozilla/5.0 (Windows; U; WindowsNT 5.1; en-US; rv1.8.1.6) Gecko/20070725 Firefox/2.0.0.6").get()
    val new_project_title = new_doc.getElementsByClass("suggestion").first.text
    var new_project_comment = new_doc.getElementsByClass("comment").text.foldLeft("")((l, r) => l.toString ++ r.toString)

     //if (new_project_comment == "") {
     //val new_project_comment = new_doc.getElementsByClass("pad_l30").text.foldLeft("")((l, r) => l.toString ++ r.toString).replace("<.*>", "")
      //}
    new_project_title ++ "\n" ++ new_project_comment
  }

  def getProjectPrice(new_project_url: String): (String, String) = {
    val new_doc = Jsoup.connect(new_project_url)
      .userAgent("Mozilla/5.0 (Windows; U; WindowsNT 5.1; en-US; rv1.8.1.6) Gecko/20070725 Firefox/2.0.0.6").get()
      val price_span = new_doc.getElementsByClass("price-number").asScala.map(x => x.text)

    (price_span.head ++ "¥", price_span.tail.head ++ "¥")
  }

  def isContainingNG(contents: String): Boolean = {
    val p1 = "常駐"
    val r1 = p1.r

		if (r1.findFirstIn(contents) != None) true
    else false
  }
}

import com.flyberrycapital.slack.SlackClient
object Slack {
  val s = new SlackClient("xoxp-19896831492-19900049315-20237606272-31cff2a970")
  s.connTimeout(1000)
  s.readTimeout(5000)
  
  def post(new_project_url: String): Unit = {
    s.chat.postMessage("#piimemo", new_project_url)
  }
}
