import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import Crypt._
import java.io.PrintWriter

case class Entry ( account   : String
                 , password  : String
                 , uid       : Int
                 , gid       : Int
                 , gecos     : String
                 , directory : String
                 , shell     : String
                 )

object Entry {
  def apply(line: String) : Entry = {
    val splitLines = line.split(":")
    new Entry(splitLines(0), splitLines(1), splitLines(2).toInt, splitLines(3).toInt, splitLines(4), splitLines(5), splitLines(6))
  }
}

object Crack {
  def transformReverse(w: String) : Iterator[String] = {
    Iterator(w,w.reverse);
  }
  
  def transformCapitalize(w: String) : Iterator[String] = {
    if (w == "")  
      Iterator(w) 
    else
    for(hd <- Iterator(w.substring(0,1).toLowerCase, w.substring(0,1).toUpperCase); 
        tl <- transformCapitalize(w.substring(1))) 
          yield (hd + tl)
  }
  def characterToDigit(c: String) : Iterator[String] = {
    c match {
      case "o" => Iterator("o", "0")
      case "O" => Iterator("O", "0")
      case "z" => Iterator("z", "2")
      case "Z" => Iterator("Z", "2")
      case "a" => Iterator("a", "4")
      case "A" => Iterator("A", "4")
      case "b" => Iterator("b", "6", "8")
      case "B" => Iterator("B", "6", "8")
      case "g" => Iterator("g", "9")
      case "G" => Iterator("G", "9")
      case "q" => Iterator("q", "9")
      case "Q" => Iterator("Q", "9")
      case "i" => Iterator("i", "1")
      case "I" => Iterator("I", "1")
      case "l" => Iterator("l", "1")
      case "L" => Iterator("L", "1")
      case "e" => Iterator("e", "3")
      case "E" => Iterator("E", "3")
      case "s" => Iterator("s", "5")
      case "S" => Iterator("S", "5")
      case "t" => Iterator("t", "7")
      case "T" => Iterator("T", "7")
      case _   => Iterator(c)
    }
  }

  def transformDigits(w:String) : Iterator[String] = {
    if (w == "")  
      Iterator(w) 
    else
    for(hd <- characterToDigit(w.substring(0,1)); 
        tl <- transformDigits(w.substring(1)))
          yield (hd + tl)
  }

  // compares plain and enc if plain encrypts to enc return true.
  def checkPassword(plain: String, enc: String) : Boolean = 
    Crypt.crypt(enc, plain) == enc
 
  def candidateWords(file: String) =
    Words.wordsMatchingRegexp(file, new Regex("""^.{6,8}$"""))

  // scala> Crack("passwd", "words", "soln.1")
  // goto> scala Crack passwd words soln.1
  def apply(pwdFile: String, wordsFile: String, outFile: String) : Unit = {
    
      val pwd = Lines.iterator(pwdFile).map(Entry.apply)
      val canFile = candidateWords(wordsFile).toList.sortBy(_.length)
      val output = new java.io.PrintWriter(new java.io.File(outFile))
      var saved = List[String]()

      for(i <- pwd)
      {
        for(word <- canFile)
        {
          // Untransformed words, should be 6
          if(checkPassword(word, i.password))
          {
            saved ::= (word)
            //println(i.account + "=========" + word + "\n")
            output.write(i.account + "=" + word + "\n")
            output.flush()
          }
          for(t <- transformReverse(word); if (!saved.contains(word)))
          {
            if(checkPassword(t, i.password)) 
            {
              //println(i.account + "=" + word + "\n")
              output.write(i.account + "=" + word + "\n")
              output.flush()
            }
          }
          /*for( t <- transformDigits(word); if(!saved.contains(word)) )
          {
            if(checkPassword(t, i.password)){
              println(i.account + "=~~~~~~~~~~~" + word + "\n")
              output.write(i.account + "=" + word + "\n")
              output.flush()
            }
          }*/ 
        }
      }
      output.close()
  }

  def main(args: Array[String]) = { 
    println("Begin: Cracking Passwords")
    apply(args(0), args(1), args(2))
    println("Done: Cracking Passwords")
  }
}

// vim: set ts=2 sw=2 et:

