package shuffleActor

import akka.actor.ActorSystem

import akka.testkit.{TestKit, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import shuffleActor._

class ShuffleActorSpec() extends TestKit(ActorSystem("ShuffleActorSpec"))
  with ImplicitSender
  with AnyWordSpecLike
  with Matchers
  with BeforeAndAfterAll {

  import CardT._
  import ShuffleActor._
  import DeckGenerator._

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A Shuffler actor" should {
    "correctly shuffle the deck with in-shuffle and out-shuffle" in {
      val shuffler = system.actorOf(ShufflerActor.props())
      val testProbe = TestProbe()

      val deck = generateDeck
      val n = 3 // Number of times to shuffle

      println(s"Original deck: $deck")

      (1 to n).foreach { i =>
        shuffler.tell(ShuffleReq(Deck(deck), i, true), testProbe.ref) // out-shuffle
        testProbe.expectMsgPF() {
          case Deck(shuffled) =>
            println(s"Out-Shuffle $i: $shuffled")
        }

        shuffler.tell(ShuffleReq(Deck(deck), i, false), testProbe.ref) // in-shuffle
        testProbe.expectMsgPF() {
          case Deck(shuffled) =>
            println(s"In-Shuffle $i: $shuffled")
        }
      }
    }
  }
}

