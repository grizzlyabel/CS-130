import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import java.io.PrintWriter
import Lines._

object Words {

  def apply(file: String) : Iterator[String] =  
  {
    val f = new java.io.File(file)
    
    try {
      val print = scala.io.Source.fromFile(f).getLines() // get lines to variable
      for(x <- print) yield x.toLowerCase() // for all elements in the line, print lowercase form
    } catch { // As seen on lecture.
      case e: java.io.FileNotFoundException => {
        println("No such file exists.")
        Iterator()
      }
    }
  }
  def groupFreq[A,B](xs: Iterator[A], f: A => B): HashMap[B, Int] = 
  {
    var groupMap = new HashMap[B, Int]
    for( x <- xs ){
      //if( !(groupMap contains f(x)) ){ // Check to see if not mapped
        // If f(x) is contained, add one to B, else add 1 to A
        groupMap += (f(x) -> (groupMap.getOrElse(f(x), 0) + 1)) 
      //}
      //else groupMap += (f(x) -> (groupMap(f(x)) + 1)) // if mapped then add one to B
    }
    groupMap
  }

  val inti = List(1,21,5,39,12,7,92)

  def isEven(x: Int): String =
    if ((x % 2) == 0) { "Even" } else { "Odd" } 

  def sizeFreq(file: String): HashMap[Int, Int] = 
  {
    val f = new java.io.File(file)
    val lines = scala.io.Source.fromFile(f).getLines() // get lines to variable
    groupFreq(lines, (str:String) => str.size) // get size for each string  
  }

  def charFreq(file: String): HashMap[Char, Int] = 
  {
    val chars = for(words <- apply(file); c <- words) yield c // store the chars
    val grouper = (x:Char) => x // Cast to a char
    groupFreq(chars, grouper) // Takes care of the counting
  }

  def wordsOfSize(file: String, size: Int) : Iterator[String] = 
  {
    for(words <- apply(file); if (words.length == size)) yield words
  }

  def wordsWithAllVowels(file: String): Iterator[String] = {
    val vowelSet = "aeiou"
    val words = apply(file)
    words.filter(wordInWords => (vowelSet.filter(vowelInVowelSet => (!wordInWords.contains(vowelInVowelSet))).isEmpty))  
    //for( w <- words; if w.hasAllVowels ) yield w
    //for(words <- apply(file); vowels <- words; if vowelSet.contains(vowels) ) yield words//words.filter(c => (vowelSet.hasAllVowels(c))) //yield words
  }
  def wordsWithNoVowels(file: String): Iterator[String] = {
    val vowelSet = "aeiou"
    val words = apply(file)
    words.filter(wordInWords => (vowelSet.filter(vowelInVowelSet => (wordInWords.contains(vowelInVowelSet))).isEmpty))
  }

  def wordsMatchingRegexp(file: String, re: Regex): Iterator[String] = {
    val words = apply(file)
    //for( words <- apply(file) ) words.filter(f => !(re findAllIn f))
    // find all the strings that match the regular expressions FROM words
    // Check if the list returned is empty, if so then that list of words is returned
    words.filter(wordsInWordFile => !(re findAllIn wordsInWordFile).isEmpty)
  }

}

// vim: set ts=2 sw=2 et:

