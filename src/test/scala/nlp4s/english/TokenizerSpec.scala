package nlp4s.english

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import nlp4s.demo.EnglishDemo

class TokenizerSpec extends AnyFlatSpec with Matchers {
  // Dummy rule to make scalatest work nicely with generated test cases
  "An English tokenizer" should "dummy" in {
    true shouldBe true 
  }

  for(testCase <- TestCase.all) {
    if(!testCase.ignore) {
      it should s"Check that '${testCase.text}' has ${testCase.tokens} token(s)" in {
        val demo = new EnglishDemo

        demo.tokenizer.run(testCase.text).map(_.size) shouldBe Right(testCase.tokens + 1)
      }
    }
  }
}
