/*
 * The shuffler shuffles with the help of two other actors 
 *  -- splitter and faroShuffler.  
 *
 *  The shuffler sends a message to the splitter 
 *    containing the deck of cards, and the name of faroShuffler.  
 *  The shuffler also sends a message to faroShuffler with 
 *    a boolean indicating whether in-shuffle or out-shuffle is required. 
 *  On receiving the deck, 
 *    the splitter evenly splits the deck of cards into two lists, 
 *    and sends the two lists to faroShuffler, one at a time.  
 *
 *  Once faroShuffler has received the two lists, 
 *    and once it knows what type of shuffle is to be carried out, 
 *    it first tells a cardCollector actor the shuffler's name 
 *    and the number of cards to expect to receive; 
 *  Next, it begins sending the cards from 
 *    its two decks to the cardCollector actor 
 *    -- one card at a time, 
 *  alternating between the two lists, 
 *    beginning with the list required for the correct type of shuffle. 
 *  The cardCollector actor simply collects the cards in the order 
 *    in which they are received in a list, 
 *  and when it has received all of them, 
 *  it sends the list of cards to shuffler. 
 *  (producer-consumer)
 *  Only the shuffler keeps track of how many shuffles have been completed, 
 *    and when the required number of shuffles are done, 
 *    it sends the shuffled deck to the actor requesting it. 
 * */

/* Faro Shuffles (with akka) :
 * 
 * Messages:
 *   Card: - Any object.
 *   Deck: 
 *      List[Cards] of even length.
 *   IsOut: boolean, True -> outshuffle; false-> inshuffle
 *   ShuffleReq: 
 *      - Deck : List[Cards] of even length.
 *      - STime: Int. Number of times the deck is to be shuffled.
 *      - isOut: Boolean. True -> outshuffle; false -> inshuffle.
 *   SplitterReq:
 *      - Deck: List[Cards] of even length
 *      - fsName: Reference of the faro shuffler to send the deck to.
 *   FSReqHeader:
 *      - sName: Reference of the shuffler
 *      - nCards: Int. Number of cards going to come.
 *   
 * Actors:
 *   Shuffler:
 *     Receives: ShuffleReq
 *     Sends   : 
 *       - Deck shuffled (to client)
 *       - SplitterReq (to Splitter)
 *       - IsOut: Boolean (to faroShuffler)
 *       
 *   Splitter:
 *     Receives: SplitterReq (From Shuffler)
 *     Sends   : (to faroShuffler - ordered)
 *       - Deck 1
 *       - Deck 2
 *
 *   FaroShuffler:
 *     Receives: 
 *       - IsOut: Boolean (from Shuffler)
 *       - Deck 1 (from splitter) -> Needs ack
 *       - Deck 2 (from splitter) -> Needs ack
 *     Sends: (to cardCollector - ordered)
 *       - FSReqHeader
 *       - Card (N, ordered)
 *
 *   CardCollector:
 *     Receives:
 *       - FSReqHeader
 *       - Card (N, needs ack for each)
 *     Sends:
 *       - Deck (to shuffler)
 *
 *  Note 1:
 *  Some operations needs to ensure the order:
 *    Splitter -> FaroShuffler 
 *      (2nd Deck blocked by 1st Ack)
 *    FaroShuffler -> CardCollector
 *      1st card blocked by 0th Ack (header)
 *      nth card blocked by (n-1)th Ack
 *
 *  Note 2:
 *  We don't want to kill the actors and then restart them.
 *  A flag in FaroShuffler so we don't redo CC.
 *  
 */

package shuffleActor

import akka.actor._
import scala.sys.Prop
import akka.event.Logging
import com.typesafe.config.ConfigFactory


object CardT {
  case class Card(card: Any)
  case class Deck(deck: List[Card])
  case class Ack(seq: Int)
}

object ShuffleActor {
  import CardT.Card
  import CardT.Deck
  import FaroShufflerActor.StoFSReq
  def props(): Props = Props(new ShuffleActor())
  case class ShuffleReq(deck: List[Card], sTime: Int, isOut: Boolean)
}

object SplitterActor {
  import CardT._
  def props(): Props = Props(new SplitterActor())
  case class SplitterReq(deck: List[Card], fsName: ActorRef)
}

object FaroShufflerActor {
  import CardT._
  def props(): Props = Props(new FaroShufflerActor())
  case class FStoCCHeader(sName: ActorRef, nCards: Int)
  case class StoFSReq(isOut: Boolean)
}

object CardCollectorActor {
  import CardT._
  import FaroShufflerActor.FStoCCHeader
  def props(): Props = Props(new CardCollectorActor())
}


class ShuffleActor extends Actor {
/* ShuffleActor:
 * Init all things, and only actor that
 *   tracks the number of shuffles done.
 * */
  import ShuffleActor._
  import CardT._
  
  var splitter: ActorRef = null
  var faroShuffler: ActorRef = null
  var cardCollector: ActorRef = null
  var client: ActorRef = null
  var isOut: Boolean = false
  val log = Logging(context.system, this)
  var nshuffles: Int = -1
  override def preStart() = {
    log.debug("Starting, I am at [{}]", self.path)
  }
  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.error(reason, "Restarting due to [{}] when processing [{}]", 
      reason.getMessage, message.getOrElse(""))
  }
  
  def receive: PartialFunction[Any, Unit] = {
    /* Two cases:
     * 1. ShuffleReq: 
     *  - new request from client (including init)
     * 2. Deck:
     *  - when CardCollector sends the shuffled deck
     */
    case ShuffleReq(deck, sTime, isOut) =>
      log.debug("{} Received ShuffleReq from {}", self.path, sender.path)
      client = sender
      nshuffles = sTime
      this.isOut = isOut
      splitter = context.actorOf(SplitterActor.props())
      faroShuffler = context.actorOf(FaroShufflerActor.props())
      /* init all the actors */
      splitter ! SplitterActor.SplitterReq(deck, faroShuffler)
      faroShuffler ! FaroShufflerActor.StoFSReq(isOut)
    
    case Deck(deck) =>
      log.debug("{} Received Deck from {}", self.path, sender.path)
      nshuffles -= 1
      if (nshuffles == 0) {
        client ! Deck(deck)
      } else {
        splitter ! SplitterActor.SplitterReq(deck, faroShuffler)
        faroShuffler ! FaroShufflerActor.StoFSReq(this.isOut)
      }
    case _ =>
      log.error("ShuffleActor: wtf")
  }
}

class SplitterActor extends Actor {
  /* SplitterActor:
   * Splits the deck into two and sends to faroShuffler.
   * */
  var d1: List[CardT.Card] = Nil
  var d2: List[CardT.Card] = Nil
  var fsName: ActorRef = null
  val log = Logging(context.system, this)
  override def preStart() = {
    log.debug("Starting, I am at [{}]", self.path)
  }
  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.error(reason, "Restarting due to [{}] when processing [{}]", 
      reason.getMessage, message.getOrElse(""))
  }

}
