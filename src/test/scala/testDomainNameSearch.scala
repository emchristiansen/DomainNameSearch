import org.scalatest.fixture
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scalatestextra._

////////////////////////////////////////////////////////////////////////////////

@RunWith(classOf[JUnitRunner])
class TestFunGeneratorSuite extends FunGeneratorSuite {
  test("a vanilla unit test", InstantTest) {
    val x = 1
    assert(x == 1)
  }

  test("a generator driven test", InstantTest) {
    val evenInts = for (n <- Gen.choose(-1000, 1000)) yield 2 * n
    forAll(evenInts) { x =>
      assert(x % 2 == 0)
    }
  }
}

@RunWith(classOf[JUnitRunner])
class TestFunGeneratorConfigSuite extends FunGeneratorConfigSuite {
  test("make sure the ConfigMap exists for a vanilla unit test") { configMap =>
    assert(configMap.toString.contains("Map"))
  }

  test("make sure the ConfigMap exists for a generator driven test") {
    configMap =>
      val evenInts = for (n <- Gen.choose(-1000, 1000)) yield 2 * n
      forAll(evenInts) { x =>
        assert(x % 2 == 0)
        assert(configMap.toString.contains("Map"))
      }
  }
}
