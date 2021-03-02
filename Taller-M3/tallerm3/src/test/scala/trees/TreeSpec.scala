package	trees

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class sizeSpec extends AnyFlatSpec with Matchers {
 "El size de Leaf(10)" should "debe dar 1" in {
   Tree.size(Leaf(10)) shouldEqual 1
  }

 "El size de Branch(Leaf(10),Leaf(20))" should "debe dar 3" in {
   Tree.size(Branch(Leaf(10),Leaf(20))) shouldEqual 3
  }

 "El size de Branch(Branch(Leaf(10),Leaf(20)),Leaf(30))" should "debe dar 5" in {
   Tree.size(Branch(Branch(Leaf(10),Leaf(20)),Leaf(30))) shouldEqual 5
  }
}

class depthSpec extends AnyFlatSpec with Matchers {
 "El depth de Leaf(10)" should "debe dar 1" in {
   Tree.depth(Leaf(10)) shouldEqual 1
  }

 "El depth de Branch(Leaf(10),Leaf(20))" should "debe dar 2" in {
   Tree.depth(Branch(Leaf(10),Leaf(20))) shouldEqual 2
  }

 "El depth de Branch(Branch(Leaf(10),Leaf(20)),Leaf(30))" should "debe dar 3" in {
   Tree.depth(Branch(Branch(Leaf(10),Leaf(20)),Leaf(30))) shouldEqual 3
  }

 "El depth de Branch(Branch(Leaf(10),Leaf(20)),Branch(Leaf(30),Leaf(40)))" should "debe dar 3" in {
   Tree.depth(Branch(Branch(Leaf(10),Leaf(20)),Branch(Leaf(30),Leaf(40)))) shouldEqual 3
  }
}


