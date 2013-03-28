namespace f0

open ForceFormat
    
type S2<'a0,'a1>(a0: 'a0, a1: 'a1) =
  interface Format with member f.Translate l =
    l.TypeApplication("S2", [forceFormat(a0).Translate l; forceFormat(a1).Translate l])

type S3<'a0,'a1,'a2>(a0: 'a0, a1: 'a1, a2: 'a2) =
  interface Format with member f.Translate l =
    l.TypeApplication("S3", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l])

type S4<'a0,'a1,'a2,'a3>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3) =
  interface Format with member f.Translate l =
    l.TypeApplication("S4", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l])

type S5<'a0,'a1,'a2,'a3,'a4>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4) =
  interface Format with member f.Translate l =
    l.TypeApplication("S5", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l])

type S6<'a0,'a1,'a2,'a3,'a4,'a5>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5) =
  interface Format with member f.Translate l =
    l.TypeApplication("S6", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l])

type S7<'a0,'a1,'a2,'a3,'a4,'a5,'a6>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6) =
  interface Format with member f.Translate l =
    l.TypeApplication("S7", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l; forceFormat(a6).Translate l])

type S8<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7) =
  interface Format with member f.Translate l =
    l.TypeApplication("S8", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l; forceFormat(a6).Translate l; forceFormat(a7).Translate l])

type S9<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8) =
  interface Format with member f.Translate l =
    l.TypeApplication("S9", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l; forceFormat(a6).Translate l; forceFormat(a7).Translate l; forceFormat(a8).Translate l])

type S10<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9) =
  interface Format with member f.Translate l =
    l.TypeApplication("S10", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l; forceFormat(a6).Translate l; forceFormat(a7).Translate l; forceFormat(a8).Translate l; forceFormat(a9).Translate l])

type S11<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10) =
  interface Format with member f.Translate l =
    l.TypeApplication("S11", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l; forceFormat(a6).Translate l; forceFormat(a7).Translate l; forceFormat(a8).Translate l; forceFormat(a9).Translate l; forceFormat(a10).Translate l])

type S12<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11) =
  interface Format with member f.Translate l =
    l.TypeApplication("S12", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l; forceFormat(a6).Translate l; forceFormat(a7).Translate l; forceFormat(a8).Translate l; forceFormat(a9).Translate l; forceFormat(a10).Translate l; forceFormat(a11).Translate l])

type S13<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12) =
  interface Format with member f.Translate l =
    l.TypeApplication("S13", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l; forceFormat(a6).Translate l; forceFormat(a7).Translate l; forceFormat(a8).Translate l; forceFormat(a9).Translate l; forceFormat(a10).Translate l; forceFormat(a11).Translate l; forceFormat(a12).Translate l])

type S14<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13) =
  interface Format with member f.Translate l =
    l.TypeApplication("S14", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l; forceFormat(a6).Translate l; forceFormat(a7).Translate l; forceFormat(a8).Translate l; forceFormat(a9).Translate l; forceFormat(a10).Translate l; forceFormat(a11).Translate l; forceFormat(a12).Translate l; forceFormat(a13).Translate l])

type S15<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13,'a14>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13, a14: 'a14) =
  interface Format with member f.Translate l =
    l.TypeApplication("S15", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l; forceFormat(a6).Translate l; forceFormat(a7).Translate l; forceFormat(a8).Translate l; forceFormat(a9).Translate l; forceFormat(a10).Translate l; forceFormat(a11).Translate l; forceFormat(a12).Translate l; forceFormat(a13).Translate l; forceFormat(a14).Translate l])

type S16<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13,'a14,'a15>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13, a14: 'a14, a15: 'a15) =
  interface Format with member f.Translate l =
    l.TypeApplication("S16", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l; forceFormat(a6).Translate l; forceFormat(a7).Translate l; forceFormat(a8).Translate l; forceFormat(a9).Translate l; forceFormat(a10).Translate l; forceFormat(a11).Translate l; forceFormat(a12).Translate l; forceFormat(a13).Translate l; forceFormat(a14).Translate l; forceFormat(a15).Translate l])

type S17<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13,'a14,'a15,'a16>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13, a14: 'a14, a15: 'a15, a16: 'a16) =
  interface Format with member f.Translate l =
    l.TypeApplication("S17", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l; forceFormat(a6).Translate l; forceFormat(a7).Translate l; forceFormat(a8).Translate l; forceFormat(a9).Translate l; forceFormat(a10).Translate l; forceFormat(a11).Translate l; forceFormat(a12).Translate l; forceFormat(a13).Translate l; forceFormat(a14).Translate l; forceFormat(a15).Translate l; forceFormat(a16).Translate l])

type S18<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13,'a14,'a15,'a16,'a17>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13, a14: 'a14, a15: 'a15, a16: 'a16, a17: 'a17) =
  interface Format with member f.Translate l =
    l.TypeApplication("S18", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l; forceFormat(a6).Translate l; forceFormat(a7).Translate l; forceFormat(a8).Translate l; forceFormat(a9).Translate l; forceFormat(a10).Translate l; forceFormat(a11).Translate l; forceFormat(a12).Translate l; forceFormat(a13).Translate l; forceFormat(a14).Translate l; forceFormat(a15).Translate l; forceFormat(a16).Translate l; forceFormat(a17).Translate l])

type S19<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13,'a14,'a15,'a16,'a17,'a18>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13, a14: 'a14, a15: 'a15, a16: 'a16, a17: 'a17, a18: 'a18) =
  interface Format with member f.Translate l =
    l.TypeApplication("S19", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l; forceFormat(a6).Translate l; forceFormat(a7).Translate l; forceFormat(a8).Translate l; forceFormat(a9).Translate l; forceFormat(a10).Translate l; forceFormat(a11).Translate l; forceFormat(a12).Translate l; forceFormat(a13).Translate l; forceFormat(a14).Translate l; forceFormat(a15).Translate l; forceFormat(a16).Translate l; forceFormat(a17).Translate l; forceFormat(a18).Translate l])

type S20<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13,'a14,'a15,'a16,'a17,'a18,'a19>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13, a14: 'a14, a15: 'a15, a16: 'a16, a17: 'a17, a18: 'a18, a19: 'a19) =
  interface Format with member f.Translate l =
    l.TypeApplication("S20", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l; forceFormat(a6).Translate l; forceFormat(a7).Translate l; forceFormat(a8).Translate l; forceFormat(a9).Translate l; forceFormat(a10).Translate l; forceFormat(a11).Translate l; forceFormat(a12).Translate l; forceFormat(a13).Translate l; forceFormat(a14).Translate l; forceFormat(a15).Translate l; forceFormat(a16).Translate l; forceFormat(a17).Translate l; forceFormat(a18).Translate l; forceFormat(a19).Translate l])

type S21<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13,'a14,'a15,'a16,'a17,'a18,'a19,'a20>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13, a14: 'a14, a15: 'a15, a16: 'a16, a17: 'a17, a18: 'a18, a19: 'a19, a20: 'a20) =
  interface Format with member f.Translate l =
    l.TypeApplication("S21", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l; forceFormat(a6).Translate l; forceFormat(a7).Translate l; forceFormat(a8).Translate l; forceFormat(a9).Translate l; forceFormat(a10).Translate l; forceFormat(a11).Translate l; forceFormat(a12).Translate l; forceFormat(a13).Translate l; forceFormat(a14).Translate l; forceFormat(a15).Translate l; forceFormat(a16).Translate l; forceFormat(a17).Translate l; forceFormat(a18).Translate l; forceFormat(a19).Translate l; forceFormat(a20).Translate l])

type S22<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13,'a14,'a15,'a16,'a17,'a18,'a19,'a20,'a21>(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13, a14: 'a14, a15: 'a15, a16: 'a16, a17: 'a17, a18: 'a18, a19: 'a19, a20: 'a20, a21: 'a21) =
  interface Format with member f.Translate l =
    l.TypeApplication("S22", [forceFormat(a0).Translate l; forceFormat(a1).Translate l; forceFormat(a2).Translate l; forceFormat(a3).Translate l; forceFormat(a4).Translate l; forceFormat(a5).Translate l; forceFormat(a6).Translate l; forceFormat(a7).Translate l; forceFormat(a8).Translate l; forceFormat(a9).Translate l; forceFormat(a10).Translate l; forceFormat(a11).Translate l; forceFormat(a12).Translate l; forceFormat(a13).Translate l; forceFormat(a14).Translate l; forceFormat(a15).Translate l; forceFormat(a16).Translate l; forceFormat(a17).Translate l; forceFormat(a18).Translate l; forceFormat(a19).Translate l; forceFormat(a20).Translate l; forceFormat(a21).Translate l])