
/*
 * Problem 1 (50 Points): Fibonacci Numbers.  
 *
 * Develop an actor program for computing Fibonacci numbers concurrently. 
 * -> create an actor which: receives a request 
 *    for a particular Fibonacci number from a client actor.  
 *  If it is one of the base cases, 
 *  the actor replies with a message containing the answer; 
 *
 * -> otherwise, it breaks down the problem into two sub-problems, 
 *    sends the smaller of the two to a new Fibonacci actor 
 *    and sends the larger one to itself.  
 *
 * -> When it receives the two replies, 
 *  it adds them to produce its result, 
 *  and sends it back to the computation’s client.
 * */



/*
  Design:

  1. Types
  need to know if the message is a request or a response,
  so assign different types to each to differentiate between them.

  -> Req: Message when need a fib number. Req(x) -> want xth fib number.
  -> Res: Message when have the fib number. Res(x) -> x is the value of kth fib

  
  Actor class for calculating Fib numbers.
  F.

  Such F can do following:
  - create new actor
  - create Req 
    to send to new actor or itself
  - create Res
    to send to its sender.

  Each F requires 3 local mutables:
  -> For collecting the results (sum)
    1. accu: Int.
      Holds the intermediate sum of the fib numbers.
      (either one of the fib(k-1) or fib(k-2))
    2. hasFirstRes: Boolean.
      Flag if the actor has received the first response before or not.
      If set: this Res is the second response, and ready to sum and send back.
      If not set: this Res is the first response, need to put it in accu.
    3.1. senderStack: ActorRef.
      Since F itself would also expecting a message from F itself,
      it needs to know who to send the response back to.
      Just like a turing machine, need to keep track of the sender (ret addr)

    3.2: Stackless implementation.
      Although we don't know how deep the call stack would be, 
      we at least know something: for stack with depth = k
      the stack would contain (k-1) reference to itself on the top,
      then the deepest contains the foreign caller.
      Hence, for the first (k-1) responses, 
      it always sends the response back to itself.
      We do not need to store EACH of the (k-1) references to itself.
      We just need to store the last one, and keep count of how "deep" we are.
      (fake the stack)
      3.2.1. stackDepth: Int.
        How many times the actor has sent a message to itself.
        (how many times it has been called recursively)
      3.2.2. foreignSender: ActorRef.
        The foreign caller, who is the real sender of the message.
        The one who is actually waiting for the response.
        (the one who is at the bottom of the stack)
        Only reply to him when the stackDepth == 0.
    
3. firstRes:
    
  2. Base cases:
  - for all k < 2, fib(k) = k.

  3. Non-base cases:
  - fib(k) = fib(k-1) + fib(k-2)
  on receive Req(x):
  if stackDepth == 0:
    foreignSender = sender
  -> if x < 2, send Res(x) back.
  -> if x >= 2,
    send Req(x-1) to new actor.
    send Req(x-2) to itself.
      add 1 to stackDepth.

  on receive Res(x):
    add x to accu.
    if hasFirstRes:
      send Res(accu) to foreignSender or self,
      depending on stackDepth.
    reset accu, hasFirstRes.
    else:
      hasFirstRes = true.
*/
package fibActor

import akka.actor._
import scala.sys.Prop
import akka.event.Logging
import com.typesafe.config.ConfigFactory

object FibActor {
  def props(): Props = Props(new FibActor())
  case class Req(x: Int)
  case class Res(x: Int)
  /* different types of wrappers */
}

class FibActor extends Actor {
  import FibActor._
  /* Thread-local mutable variables:
   * 1. accu: Int.
   * 2. hasFirstRes: Boolean.
   * 3. stackDepth: Int. 
   * 4. foreignSender: ActorRef.
   * */
  var accu: Int = 0
  var hasFirstRes: Boolean = false
  var stackDepth: Int = 0
  var foreignSender: ActorRef = null
  val log = Logging(context.system, this)
  override def preStart() = {
    log.debug("Starting, I am at [{}]", self.path)
  }
  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.error(reason, "Restarting due to [{}] when processing [{}]", reason.getMessage, message.getOrElse(""))
  }
  def receive: PartialFunction[Any, Unit] = {
    case Req(x) =>
      log.debug("{} Received Req({}) from {} at stackDepth {}",
        self.path, x, sender.path, stackDepth)
      
      /* first initialized, being called by foreign  */
      if (stackDepth == 0) foreignSender = sender
      if (x < 2) {
        /* base case */
        stackDepth -= 1
        sender ! Res(x) /* reply to sender directly */
        log.debug("{}: Base case: sent Res({}) to {}", self.path, x, sender.path)
      } else {
        /* non-base case:
         * request (x-2) from new actor (create one) 
         * request (x-1) from itself
         * */
        context.actorOf(FibActor.props()) ! Req(x-2)
        log.debug("{}: sent Req({}) to new actor", self.path, x-2)
        self ! Req(x-1)
        log.debug("{}: sent Req({}) to myself", self.path, x-1)
        stackDepth += 1 /* since called itself, 
        add 1 to depth */
      }
    case Res(x) =>
      accu += x
      log.debug("{}, accu: {}, hasFirstRes: {} at stackDepth {}",
        self.path + ": Received Res(" + x + ") from " + sender.path, 
        accu, hasFirstRes, 
        stackDepth)
      if (hasFirstRes) {
        /* now ready to reply with accu, who to reply to?
         * -> if stackDepth=0, reply to foreign
         * -> otherwise, reply to self */
        if (stackDepth == 0) {foreignSender ! Res(accu)
          log.debug("{}: sent Res({}) to {}", 
            self.path, accu, foreignSender.path)}
        else { self ! Res(accu)
          log.debug("{}: sent Res({}) to {}", 
            self.path, accu, self.path)
              stackDepth -= 1 }
        accu = 0 /* reset */
      }
      hasFirstRes = !hasFirstRes /* toggle */
  }
}
