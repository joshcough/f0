namespace f0

type EffectW<'f> = | EffectW
type EffectR<'f> = | EffectR

module Effects =

    let ew = EffectW
    let er = EffectR
    let effectW<'f>() = ew
    let effectR<'f>() = er
