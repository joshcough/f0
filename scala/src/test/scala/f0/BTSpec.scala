package f0

import f0.Formats._
import f0.Writers._
import f0.Readers._
import org.scalacheck.Prop._
import org.scalacheck.Properties

object BTSpec extends Properties("BT"){

  type BTF = FixF[S2[StringF,P2[SelfF,SelfF]]]
  implicit val btf: BTF = fixF(s2F(stringF, p2F(selfF, selfF)))

  sealed abstract class BT
  case class Bin(left : BT, right : BT) extends BT
  case class Leaf(label : String) extends BT

  type BTFn[x] = StringF | (x & x)

  def btW = fixFW[BT,BTFn](self => s2W(
    stringW cmap ((bt: Leaf) => bt.label),
    p2W(self, self)(g => (bt: Bin) => g(bt.left, bt.right)))((leaf,branch) =>
      (_:BT) match {
        case l@Leaf(_) => leaf(l)
        case b@Bin(_,_) => branch(b)
      }))

  lazy val btR2: Reader[BT,DynamicF] = (for {
    b <- byteR
    bt <- b match {
      case 0 => stringR map (Leaf)
      case 1 => p2R(btR2, btR2)(Bin)
    }
  } yield bt) erase

  val btR3 = fixR[BT,DynamicF](self => union2R(
    stringR map (Leaf),
    p2R(self, self)(Bin)
  ) erase)

  lazy val btW2: Writer[BT,DynamicF] = Writer {
    implicit s => {
      case Leaf(l)  => byteW(0); stringW(l) erase
      case Bin(l,r) => byteW(1); Write.p2(btW2(l), btW2(r)) erase
    }
  }

  val btW3: Writer[BT,DynamicF] = fixW[BT,DynamicF](self => s2W(
    stringW cmap ((l: Leaf) => l.label),
    p2W(self, self)(g => (bt: Bin) => g(bt.left, bt.right)))((leaf,branch) =>
      (_:BT) match {
        case l@Leaf(_) => leaf(l)
        case b@Bin(_,_) => branch(b)
      }) erase)

  def btR = fixFR[BT,BTFn](self =>
    stringR.map(Leaf) |
    p2R(self,self)(Bin))

  property("bt") = secure {
    val t = Bin(Leaf("hi"), Leaf("a;sldkfj"))
    btR(btW.toByteArray(t)) == t &&
    btR(btW2.toByteArray(t)) == t
  }
}
