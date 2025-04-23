package nlp4s.english

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import nlp4s.demo.EnglishDemo

class ParserSpec extends AnyFlatSpec with Matchers {
  // Dummy rule to make scalatest work nicely with generated test cases
  "An English parser" should "dummy" in {
    true shouldBe true
  }

  for(testCase  <- TestCase.all) {
    if(!testCase.ignore) {
      it should s"parse '${testCase.text}' to ${testCase.parses} distinct parse tree(s)" in {
        val demo = new EnglishDemo

        if(testCase.parses > 0) {
          demo.parseString(testCase.text).map(_.length) shouldBe Right(testCase.parses)
        } else {
          demo.parseString(testCase.text).isLeft shouldBe true
        }
      }
    }
  }
}

