object Decorators {
    
  object profile { 
    
    private var cm: Map[String, Int] =  Map().withDefault(_ => 0)

    def count(name: String) = 
      cm(name)
   
    def reset(name: String) = 
      cm += name -> 0

    def apply[A, B](name: String)(f: A => B) = new Function1[A, B] {
      def apply(x: A) = {
        val n = cm(name)
        cm += name -> (1 + n)
        f(x)
      }
    }
  }
 
  object trace {
    
      // You may add more fields here
    def apply[A, B](name: String)(f: A => B) : Function1[A, B] = new Function1[A, B] {
      // You may add more fields here

      def apply (x: A): B = sys.error("TO BE DONE")
    }
  } 
  
  
  object memo {
      // You may add more fields here
    def apply[A, B](f: A => B) : Function1[A, B] = new Function1[A, B] {
      // You may add more fields here
      def apply (x: A): B = sys.error("TO BE DONE")
    }
  }

}


