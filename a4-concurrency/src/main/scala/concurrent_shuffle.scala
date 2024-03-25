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
  case class ShuffleReq(deck: Deck, sTime: Int, isOut: Boolean)
}

object SplitterActor {
  import CardT.Deck
  def props(): Props = Props(new SplitterActor())
  case class SplitterReq(deck: Deck, fsName: ActorRef)
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
    
    case Deck(cards) =>
      log.debug("{} Received Deck from {}", self.path, sender.path)
      log.debug("now the deck is {}", cards)
      nshuffles -= 1
      if (nshuffles == 0) {
        client ! Deck(cards)
      } else {
        splitter ! SplitterActor.SplitterReq(Deck(cards), faroShuffler)
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
  import CardT._
  var hasAckDeck1: Boolean = false
  var secondHalfDeck : Option[Deck] = None
  var fsName: ActorRef = null
  val log = Logging(context.system, this)
  override def preStart() = {
    log.debug("Starting, I am at [{}]", self.path)
  }
  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.error(reason, "Restarting due to [{}] when processing [{}]", 
      reason.getMessage, message.getOrElse(""))
  }
  
  def receive: PartialFunction[Any, Unit] = {
    case SplitterActor.SplitterReq(deck, fsName) =>
      log.debug("{} Received SplitterReq from {}", self.path, sender.path)
      this.fsName = fsName
      hasAckDeck1 = false /* reset */ 
      val (d1, d2) = deck.deck.splitAt(deck.deck.length/2)
      /* send the 1st half, wait for the ack */
      fsName ! d1
      secondHalfDeck = Some(Deck(d2))

    case Ack(seq) if seq == 7 =>
      log.debug("{} Received Ack from {}", self.path, sender.path)
      hasAckDeck1 = true
      if (secondHalfDeck.isDefined) {
        fsName ! secondHalfDeck.get
        secondHalfDeck = None /* reset */ 
      }
      
    case Ack(seq) if seq != 7 =>
      log.error("SplitterActor: corrupted ack, wtf. Sender is {}", sender.path)
    case _ =>
      log.error("SplitterActor: wtf")
  }
}

class FaroShufflerActor extends Actor {
  /* FaroShufflerActor:
   * Shuffles the deck and sends to CardCollector.
   * */
  import CardT._
  import FaroShufflerActor._
  var isOut: Boolean = false
  var cardCollector: ActorRef = null
  var sName: ActorRef = null
  var nCards: Int = -1
  var deck1: Option[Deck] = None
  var deck2: Option[Deck] = None
  var hasDeck1: Boolean = false /* set after ACK 1st deck */
  var hasInitCC: Boolean = false /* set after init CardCollector */
  var hasOrder: Boolean = false /* set after received from shuffler */
  var hasBothDecks: Boolean = false /* set after received both decks */
  var nextDeckOne: Boolean = false /* alternate between decks */
  val log = Logging(context.system, this)
  override def preStart() = {
    log.debug("Starting, I am at [{}]", self.path)
  }
  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.error(reason, "Restarting due to [{}] when processing [{}]", 
      reason.getMessage, message.getOrElse(""))
  }
  def sendHeader() = {
    if (hasInitCC) {
      cardCollector ! FStoCCHeader(sName, nCards)
      hasOrder = false
      hasBothDecks = false
      hasDeck1 = false /* reset */
    } else {
      log.error("FaroShufflerActor: wtf, no init CC")
    }
  }
  def sendCard() : Unit = {
    if (nextDeckOne) {
      deck1 match {
        case Some(Deck(cards)) =>
          if (cards.isEmpty) {
            log.error("FaroShufflerActor: wtf, empty deck1")
          } else {
            cardCollector ! Card(cards.head)
            deck1 = Some(Deck(cards.tail))
          }
        case None =>
          log.error("FaroShufflerActor: wtf, deck1 is None")
        case _ =>
          log.error("FaroShufflerActor: wtf, deck1 is corrupted")
      }
    }
    else {
      deck2 match {
        case Some(Deck(cards)) =>
          if (cards.isEmpty) {
            log.error("FaroShufflerActor: wtf, empty deck2")
          } else {
            cardCollector ! Card(cards.head)
            deck2 = Some(Deck(cards.tail))
          }
        case None =>
          log.error("FaroShufflerActor: wtf, deck2 is None")
        case _ =>
          log.error("FaroShufflerActor: wtf, deck2 is corrupted")
      }
    }
    nextDeckOne = !nextDeckOne
  }

  def receive: PartialFunction[Any, Unit] = {
    case StoFSReq(isOut) =>
      log.debug("{} Received StoFSReq from {}", self.path, sender.path)
      this.isOut = isOut
      this.nextDeckOne = isOut /* outshuffle -> starting from D1 */
      this.hasOrder = true
      if (!hasInitCC) {
        cardCollector = context.actorOf(CardCollectorActor.props())
        hasInitCC = true }
      /* if have both decks, ready to send then */ 
      if (hasBothDecks) {
        sendHeader()
      }
      
    case Deck(cards) if !hasDeck1 =>
      /* this is 1st message from Splitter.
       * needs to send ack to Splitter */
      this.hasDeck1 = true
      this.deck1 = Some(Deck(cards))
      sender ! Ack(7)

    case Deck(cards) if (hasDeck1 && !hasBothDecks) =>
      /* this is 2nd message from Splitter.
      * if we have the order, can get ready to send to CC */
      this.deck2 = Some(Deck(cards))
      this.hasBothDecks = true
      if (hasOrder) { /* send header to CC */
        sendHeader()
      }

    case Ack(seq) if seq == 0 => /* on ack header */
      log.debug("{} Received Ack from {}", self.path, sender.path)
      /* now send */
      sendCard()

    case Ack(seq) if 0 < seq && seq < nCards =>
      log.debug("{} Received Ack from {}", self.path, sender.path)
      sendCard()

    case Ack(seq) if seq == nCards =>
      log.debug("{} Received Ack from {}", self.path, sender.path)
      if (deck1.get.deck.isEmpty && deck2.get.deck.isEmpty) {
      log.debug("All cards sent to CC")
      } else {
      log.error("FaroShufflerActor: wtf, not all cards sent to CC")
      }

    case Ack(seq) if seq < 0 || seq > nCards =>
      log.error("FaroShufflerActor: wtf, corrupted ack. Sender is {}", sender.path)

    case _ =>
      log.error("FaroShufflerActor: wtf")
  }
}


class CardCollectorActor extends Actor {
  /* CardCollectorActor:
   * Collects the shuffled deck and sends to shuffler.
   * */
  import CardT._
  import FaroShufflerActor.FStoCCHeader
  var sName: ActorRef = null
  var nCards: Int = -1
  var cards: List[Card] = Nil
  var hasHeader: Boolean = false
  var ackSeq: Int = 0
  val log = Logging(context.system, this)
  override def preStart() = {
    log.debug("Starting, I am at [{}]", self.path)
  }
  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.error(reason, "Restarting due to [{}] when processing [{}]", 
      reason.getMessage, message.getOrElse(""))
  }

  def receive: PartialFunction[Any, Unit] = {
    case FStoCCHeader(sName, nCards) =>
      log.debug("{} Received FStoCCHeader from {}",
        self.path, sender.path)
      this.sName = sName
      this.nCards = nCards
      hasHeader = true
      ackSeq = 0
      sender ! Ack(ackSeq)

    case Card(card) if hasHeader =>
      log.debug("{} Received Card from {}", self.path, sender.path)
      cards = Card(card) :: cards
      ackSeq += 1
      if (ackSeq == nCards) {
        sName ! Deck(cards)
      } 
      sender ! Ack(ackSeq)

    case Card(card) if !hasHeader =>
      log.error("CardCollectorActor: wtf, cards before header")

    case _ =>
      log.error("CardCollectorActor: wtf")
  }
}

object DeckGenerator {
  val suits = List("Hearts", "Diamonds", "Clubs", "Spades")
  val ranks = List("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")

  import CardT.Card
  def generateDeck: List[Card] = for {
    suit <- suits
    rank <- ranks
  } yield Card(s"$rank of $suit")
}
