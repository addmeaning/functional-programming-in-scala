import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll

object IntSpec extends Properties("SomeName"){

  property("startsWith") = forAll(Gen.const("1"), Gen.const("2")){ (a: String, b: String) =>
    (a+b).startsWith(a)
  }

  property("listSum") = forAll(Gen.listOf(Gen.const(1))){ x => x.sum == x.size}

}
