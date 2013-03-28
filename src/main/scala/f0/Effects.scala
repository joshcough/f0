package f0

case class EffectW[+F]() { def erase: EffectW[Nothing] = this.asInstanceOf[EffectW[Nothing]] }
case class EffectR[-F]() { def erase: EffectR[Any] = this.asInstanceOf[EffectR[Any]] }

object Effects {
  private val _effectW = EffectW()
  private val _effectR = EffectR()
  def effectW[F] = _effectW.asInstanceOf[EffectW[F]]
  def effectR[F] = _effectR.asInstanceOf[EffectR[F]]
}
