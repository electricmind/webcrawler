package ru.wordmetrix.webcrawler

import java.net.URI

import akka.actor.ActorSystem
import akka.testkit.{DefaultTimeout, ImplicitSender, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import ru.wordmetrix.utils.CFG
import ru.wordmetrix.webcrawler.Gather.GatherPage

class TestWebGet
  extends TestKit(ActorSystem("TestKitUsageSpec"))
  with Tools
  with DefaultTimeout with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    system.shutdown()
  }

  import SeedQueue._
  import WebGet._

  val cfg = CFG()

  "A webget" should {
    "get a page" in {

      val seedqueue = TestProbe()
      val gather = TestProbe()

      val webget = system.actorOf(
        WebGet.props(cfg),
        "WebGetTest_1")

      watch(webget)

      val uri = new URI("http://example.org/")
      seedqueue.send(webget, WebGetRequest(uri, gather.ref))
      seedqueue.expectMsg(SeedQueueGet)
      seedqueue.send(webget, SeedQueueEmpty)

      gather.expectMsgPF() {
        case msg@GatherPage(u, _) if u == uri => msg
      }

      expectTerminated(webget)
    }
  }
}
