package nlp4s.english

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import nlp4s.demo.EnglishDemo
import nlp4s.base.Mode
import nlp4s.mrs.MRS
import nlp4s.mrs.Relation

class ModeSpec extends AnyFlatSpec with Matchers {
  // Dummy rule to make scalatest work nicely with generated test cases
  "An English interpreter" should "dummy" in {
    true shouldBe true
  }

  for(testCase <- TestCase.all) {
    if(!testCase.ignore && testCase.parses > 0) {
      it should s"interpret mode of '${testCase.text}' to ${testCase.mode} (in all parses)" in {
        val demo = new EnglishDemo

        val m = demo.interpretString(testCase.text).map(_.map(x => Some(x.mode())))

        val r = List.fill(testCase.parses)(Some(testCase.mode))

        m shouldBe Right(r)
      }
    }
  }
}
