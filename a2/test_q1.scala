package runtime
import q1.Q1

object Test {
  val q1Instance = new Q1()
  val shuffle: (List[Any], List[Any]) => List[Any] = q1Instance.shuffle

  def testShuffle(): Unit = {
    // Test case 1: Empty lists
    val input1 = (List(), List())
    val expected1 = List()
    val result1 = shuffle(input1._1, input1._2)
    assert(result1 == expected1, s"Test case 1 failed: Expected $expected1, but got $result1")

    // Test case 2: One empty list
    val input2 = (List(1, 2, 3), List())
    val expected2 = List(1, 2, 3)
    val result2 = shuffle(input2._1, input2._2)
    assert(result2 == expected2, s"Test case 2 failed: Expected $expected2, but got $result2")

    // Test case 3: Lists with equal lengths
    val input3 = (List(1, 2, 3), List('a', 'b', 'c'))
    val expected3 = List(1, 'a', 2, 'b', 3, 'c')
    val result3 = shuffle(input3._1, input3._2)
    assert(result3 == expected3, s"Test case 3 failed: Expected $expected3, but got $result3")

    // Test case 4: First list is longer
    val input4 = (List(1, 2, 3, 4, 5), List('a', 'b', 'c'))
    val expected4 = List(1, 'a', 2, 'b', 3, 'c', 4, 5)
    val result4 = shuffle(input4._1, input4._2)
    assert(result4 == expected4, s"Test case 4 failed: Expected $expected4, but got $result4")

    // Test case 5: Second list is longer
    val input5 = (List(1, 2, 3), List('a', 'b', 'c', 'd', 'e'))
    val expected5 = List(1, 'a', 2, 'b', 3, 'c', 'd', 'e')
    val result5 = shuffle(input5._1, input5._2)
    assert(result5 == expected5, s"Test case 5 failed: Expected $expected5, but got $result5")

    println("All shuffle test cases passed!")
  }

  testShuffle()

  val split: (List[Any], Int) => List[List[Any]] = q1Instance.split

  def testSplit(): Unit = {
    // Test case 1: Empty list
    val input1 = (List(), 0)
    val expected1 = List(List(), List())
    val result1 = split(input1._1, input1._2)
    assert(result1 == expected1, s"Test case 1 failed: Expected $expected1, but got $result1")

    // Test case 2: Split at index 0
    val input2 = (List(1, 2, 3), 0)
    val expected2 = List(List(), List(1, 2, 3))
    val result2 = split(input2._1, input2._2)
    assert(result2 == expected2, s"Test case 2 failed: Expected $expected2, but got $result2")

    // Test case 3: Split at index 1
    val input3 = (List(1, 2, 3), 1)
    val expected3 = List(List(1), List(2, 3))
    val result3 = split(input3._1, input3._2)
    assert(result3 == expected3, s"Test case 3 failed: Expected $expected3, but got $result3")

    // Test case 4: Split at index 2
    val input4 = (List(1, 2, 3), 2)
    val expected4 = List(List(1, 2), List(3))
    val result4 = split(input4._1, input4._2)
    assert(result4 == expected4, s"Test case 4 failed: Expected $expected4, but got $result4")

    // Test case 5: Split at index 3
    val input5 = (List(1, 2, 3), 3)
    val expected5 = List(List(1, 2, 3), List())
    val result5 = split(input5._1, input5._2)
    assert(result5 == expected5, s"Test case 5 failed: Expected $expected5, but got $result5")

    println("All split test cases passed!")
  }

  testSplit()

  /* codes giving us lists and decks */
  val gennL: (Int) => List[Int] = q1Instance.gennL
/*    def genSuitN(symbo: Symbol, n: Int, accu: List[(Int, Symbol)] = Nil):  */
  val genSuitN: (Symbol, Int) => List[(Int, Symbol)] = (symbol, n) => q1Instance.genSuitN(symbol, n)
  val genDeckGeneric: (List[Symbol], Int) => List[(Int, Symbol)] = q1Instance.genDeckGeneric
  
  def testGenCards(): Unit = {
    // Test case 1: 0 cards
    val input1 = 0
    val expected1 = List()
    val result1 = gennL(input1)
    assert(result1 == expected1, s"Test case 1 failed: Expected $expected1, but got $result1")

    // Test case 2: 1 card
    val input2 = 1
    val expected2 = List(1)
    val result2 = gennL(input2)
    assert(result2 == expected2, s"Test case 2 failed: Expected $expected2, but got $result2")

    // Test case 3: 5 cards
    val input3 = 5
    val expected3 = List(1, 2, 3, 4, 5)
    val result3 = gennL(input3)
    assert(result3 == expected3, s"Test case 3 failed: Expected $expected3, but got $result3")

    // Test case 4: 10 cards
    val input4 = 10
    val expected4 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val result4 = gennL(input4)
    assert(result4 == expected4, s"Test case 4 failed: Expected $expected4, but got $result4")

    println("All genCardL test cases passed!")

    /* defining some suits here */
    val suits = List(Symbol("hearts"), Symbol("diamonds"), Symbol("clubs"), Symbol("spades"))
    
    /* Try defining a suit with n cards */
    val suit = Symbol("hearts")
    val n = 5
    val expected5 = List((1, suit), (2, suit), (3, suit), (4, suit), (5, suit))
    val result5 = genSuitN(suit, n)
    assert(result5 == expected5, s"Test case 5 failed: Expected $expected5, but got $result5")

    /* Try defining a deck with n cards */
    val expected6 = List((1, Symbol("hearts")), (2, Symbol("hearts")), (3, Symbol("hearts")), (4, Symbol("hearts")), (5, Symbol("hearts")), 
                        (1, Symbol("diamonds")), (2, Symbol("diamonds")), (3, Symbol("diamonds")), (4, Symbol("diamonds")), (5, Symbol("diamonds")), 
                        (1, Symbol("clubs")), (2, Symbol("clubs")), (3, Symbol("clubs")), (4, Symbol("clubs")), (5, Symbol("clubs")), 
                        (1, Symbol("spades")), (2, Symbol("spades")), (3, Symbol("spades")), (4, Symbol("spades")), (5, Symbol("spades")))
    val result6 = genDeckGeneric(suits, n)
    assert(result6 == expected6, s"Test case 6 failed: Expected $expected6, but got $result6")

    println("All genSuitN and genDeckGeneric test cases passed!")
    println("A deck of 13 cards for each suit: ")
    println(genDeckGeneric(suits, 13))
  }


  val outshuffle: (List[Any]) => List[Any] = q1Instance.outshuffle
  val inshuffle: (List[Any]) => List[Any] = q1Instance.inshuffle
  /* nshuffle takes in a shuffle function, a number and a list */
  val nshuffle: (List[Any] => List[Any], 
                  Int, List[Any]) => List[Any] = q1Instance.nshuffle
  
  def showFaro(): Unit = {
    val deck = genDeckGeneric(List(Symbol("hearts"), Symbol("diamonds"), Symbol("clubs"), Symbol("spades")), 13)
    println("Original deck: ")
    println(deck)
    val outShuffledDeck = outshuffle(deck)
    println("Outshuffled deck: ")
    println(outShuffledDeck)
    val inShuffledDeck = inshuffle(deck)
    println("Inshuffled deck: ")
    println(inShuffledDeck)
    val nShuffledDeck = nshuffle(outshuffle, 3, deck)
    println("3 outshuffles: ")
    println(nShuffledDeck)
    val nShuffledDeck2 = nshuffle(inshuffle, 3, deck)
    println("3 inshuffles: ")
    println(nShuffledDeck2)
    val nShuffleDeck3 = nshuffle(outshuffle, 8, deck)
    println("8 outshuffles: ")
    println(nShuffleDeck3)
    val nShuffleDeck4 = nshuffle(inshuffle, 52, deck)
    println("52 inshuffles: ")
    println(nShuffleDeck4)
  }
  
  /* finds the number of outshuffle same order, and inshuffle get reverse */
  val outshuffleResCard: (List[Any]) => (Int, List[Any]) = q1Instance.outshuffleResCard
  val inshuffleRevCard: (List[Any]) => (Int, List[Any]) = q1Instance.inshuffleRevCard
  def showOutInShuffleResRev(): Unit = {
    val deck = genDeckGeneric(List(Symbol("hearts"), Symbol("diamonds"), Symbol("clubs"), Symbol("spades")), 13)
    val (outRes, outResDeck) = outshuffleResCard(deck)
    println("Outshuffle result: ")
    println(outRes)
    println(outResDeck)
    val (inRes, inResDeck) = inshuffleRevCard(deck)
    println("Inshuffle result: ")
    println(inRes)
    println(inResDeck)
  }
  
}