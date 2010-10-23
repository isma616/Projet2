/**
 * Template for project 2: Huffman trees
 *
 * This is a code template for the second mini project.
 * It contains pre-defined functions that help you
 * solve the given problems.
 */
package project2

object huffman {

  abstract class CodeTree {
    // these methods cache the weight and the chars list for a given Fork or Leaf
    // hence they're faster than huffman.weight() and huffman.chars()
    def weight: Int
    def chars: List[Char]
  }

  case class Fork(left: CodeTree, right: CodeTree,
                  chars: List[Char], weight: Int)
       extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree {
    override def chars = List(char)
  }

  //// Part 0: Basics ////

  def weight(tree: CodeTree): Int = tree match {
    case f: Fork => weight(f.left) + weight(f.right)
    case l: Leaf => l.weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case f: Fork => chars(f.left) ::: chars(f.right)
    case l: Leaf => List(l.char)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


  //// Part 1: Generating Huffman trees ////

  def string2Chars(str: String): List[Char] =
    str.toList

  /**
   * Returns a list of the unique characters in a list of characters together with
   * the number of times they occur.
   */
  def times(chars: List[Char]): List[(Char, Int)] = {
    def times0(sub: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = sub match {
      case Nil	   => acc
      case x :: xs => times0(xs.filter(_ != x), acc :+ (x, 1 + xs.count(_ == x)))
    }
    times0(chars, List())
  }

  def makeLeafList(freqs: List[(Char, Int)]): List[CodeTree] = {
    def make0(acc: List[CodeTree], sub: List[(Char, Int)]): List[CodeTree] = sub match {
      case Nil     => acc
      case x :: xs => make0(acc :+ Leaf(x._1, x._2), xs)
    }
    make0(List(), freqs.sortWith((a, b) => a._2 < b._2))
  }

  /**
   * Checks whether <code>trees</code> contains only a single tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case Nil => List()
    case a :: Nil => List(a)
    case a :: b :: xs  => (xs :+ makeCodeTree(a, b)).sortWith((a, b) => a.weight < b.weight)
  }

  def until(condition: List[CodeTree] => Boolean, action: List[CodeTree] => List[CodeTree]): List[CodeTree] => List[CodeTree] = list => {
    if(condition(list)) list else until(condition, action)(action(list))
  }

  def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeLeafList(times(chars))).head


  //// Part 2: Decoding ////

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def decode0(tree0: CodeTree, bits0: List[Bit], accu: List[Char]): List[Char] = bits0 match {
	case Nil => tree0 match {
	    case l: Leaf => accu :+ l.char
	    case f: Fork => println("Invalid encoding!"); accu
	}
	case x :: xs => tree0 match {
	    case l: Leaf => decode0(tree, bits0, accu :+ l.char)
	    case f: Fork => decode0(if (x == 0) f.left else f.right, xs, accu)
	}
    }
    decode0(tree, bits, List())
  }

  // A Huffman coding tree for the French language.
  // Generated from the data given at
  // http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais

  val frenchCode = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  // What does the secret message say? Can you decode it?
  //
  // Hint: for the decoding use the `frenchCode' Huffman tree
  // defined above.

  val secret = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)


  //// Part 3a: Encoding using Huffman tree ////

  def encode(tree: CodeTree): List[Char] => List[Bit] = {
    def encode0(tree0: CodeTree, chars0: List[Char], accu: List[Bit]): List[Bit] = chars0 match {
      case Nil => accu
      case x :: xs => tree0 match {
	case l: Leaf => encode0(tree, xs, accu)
        case f: Fork => val isLeft = f.left.chars.contains(x)
			encode0(if(isLeft) f.left else f.right, chars0, accu :+ (if(isLeft) 0 else 1))
      }
    }
    return (c: List[Char]) => encode0(tree, c, List())
  }


  //// Part 3b: Encoding using code table ////

  type CodeTable = List[(Char, List[Bit])]

  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
    case Nil => println("Can't encode character " + char); List()
    case x :: xs => if(x._1 == char) x._2 else codeBits(xs)(char)
  }

  def convert(t: CodeTree): CodeTable = {
    def convert0(t0: CodeTree, accu: CodeTable, accuBits: List[Bit]): CodeTable = t0 match {
      case l: Leaf => accu :+ (l.char, accuBits)
      case f: Fork => mergeCodeTables(convert0(f.left, accu, accuBits :+ 0), convert0(f.right, accu, accuBits :+ 1))
    }
    convert0(t, List(), List())
  }

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    // What's the point of using map here anyway?
    //a.map(a => a) ::: b.map(b => b)

    // This works just as well
    a ::: b
  }

  def quickEncode(tree: CodeTree)(chars: List[Char]): List[Bit] = {
    def quickEncode0(table: CodeTable, chars0: List[Char], accu: List[Bit]): List[Bit] = chars0 match {
      case Nil => accu
      case x :: xs => quickEncode0(table, xs, accu ::: codeBits(table)(x))
    }
    return quickEncode0(convert(tree), chars, List())
  }

  def main(args : Array[String]) : Unit = {
    val toto = "Je crois qu'il profita, pour son évasion, d'une migration d'oiseaux sauvages. Au matin du départ il mit sa planète bien en ordre. Il ramona soigneusement ses volcans en activité. Il possédait deux volcans en activité. Et c'était bien commode pour faire chauffer le petit déjeuner du matin. Il possédait aussi un volcan éteint. Mais, comme il disait, \"On ne sait jamais!\" Il ramona donc également le volcan éteint. S'ils sont bien ramonés, les volcans brûlent doucement et régulièrement, sans éruptions. Les éruptions volcaniques sont comme des feux de cheminée. Evidemment sur notre terre nous sommes beaucoup trop petits pour ramoner nos volcans. C'est pourquoi ils nous causent tant d'ennuis."
    val fullCombo = createCodeTree(toto.toList)

    val totoBits = encode(fullCombo)(toto.toList)
    val totoDeco = decode(fullCombo, totoBits)
    println("Toto decoded = " + totoDeco.mkString("","",""))

    val secretDeco = decode(frenchCode, secret)
    println("secretDeco = " + secretDeco.mkString("", "", ""))

    val totoTable = convert(fullCombo)

    val totoBits2 = quickEncode(fullCombo)(toto.toList)
    val totoDeco2 = decode(fullCombo, totoBits2)
    println("Toto decoded = " + totoDeco2.mkString("", "", ""))

    val before = toto.getBytes("UTF-8").length * 8
    val after  = totoBits.length
    println("compression ratio = " + after + " / " + before + " = " + (after / before.asInstanceOf[Double]))
  }
  
}
