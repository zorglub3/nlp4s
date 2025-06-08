package nlp4s.english

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import nlp4s.demo.EnglishDemo

class RoundtripSpec extends AnyFlatSpec with Matchers {
  // Dummy rule to make scalatest work nicely with generated test cases
  "An English realiser connected to an interpreter" should "dummy" in {
    true shouldBe true
  }

  def makeString(ss: List[String]): String = {
    ss.filter(! _.contains('.'))
      .mkString(" ")
      .toLowerCase
      .replace("_", " ")
  }

  for(testCase <- TestCase.all) {
    if(!testCase.ignore && testCase.parses > 0) {
      it should s"do correct roundtrip for '${testCase.text}'" in {
        val demo = new EnglishDemo

        val res = demo.roundTrip(testCase.text).map(makeString)

        res shouldBe Right(testCase.text.toLowerCase)
      }
    }
  }
}
