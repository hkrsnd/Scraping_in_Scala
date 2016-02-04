import collection.JavaConverters._
import org.jsoup._
import org.jsoup.nodes._

object Main {
  var max_id = 0
  val baseurl = "http://www.lancers.jp"
  val doc = Jsoup.connect("http://www.lancers.jp/work/search/system/development?completed=0&sort=Work.started&direction=desc&")
      .userAgent("Mozilla/5.0 (Windows; U; WindowsNT 5.1; en-US; rv1.8.1.6) Gecko/20070725 Firefox/2.0.0.6").get()

  def main(args: Array[String]): Unit = {
    val id = doc.getElementsByTag("p").attr("id").drop(5).toInt // id = work_877762 先頭5文字drop
    var new_project_url = "null"
    var new_project_text = "null"

    if (id > max_id) {
      new_project_url = getNewProjectUrl()
      new_project_text = getNewProjectText(new_project_url)

      println(new_project_url)
      println(new_project_text)
      Slack.post(new_project_url ++ "\n" ++ new_project_text)
    }
    println("nope")
  }

  def getNewProjectUrl(): String = {
    val new_project_url = baseurl ++ doc.getElementsByClass("work_title").asScala.map(x => x.getElementsByTag("a").attr("href")).head
    new_project_url
  }

  def getNewProjectText(new_project_url: String): String = {
  val new_doc = Jsoup.connect(new_project_url)
      .userAgent("Mozilla/5.0 (Windows; U; WindowsNT 5.1; en-US; rv1.8.1.6) Gecko/20070725 Firefox/2.0.0.6").get()
      val new_project_title = new_doc.getElementsByClass("suggestion").first.text
      val new_project_comment = new_doc.getElementsByClass("comment").asScala.foldLeft("")((l, r) => l.toString ++ r.toString).replaceAll("<[a-z]*.*>", "")

      new_project_title ++ "\n" ++ new_project_comment
  }
}

import com.flyberrycapital.slack.SlackClient
object Slack {
  val s = new SlackClient("xoxp-19896831492-19900049315-20237606272-31cff2a970")
  s.connTimeout(1000)
  s.readTimeout(5000)
  
  def post(new_project_url: String): Unit = {
    s.chat.postMessage("#lancers", new_project_url)
  }
}
