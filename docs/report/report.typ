// $ cal(S)(s,m) &  =    "PSD"(s, f,  2,12)in["PSD"_(1_"min"),"PSD"_(1_"max")] $
// $ and "PSD"(s, f, 12,18)in["PSD"_(2_"min"),"PSD"_(2_"max")] $
// $ and "PSD"(s, f, 18,35)in["PSD"_(3_"min"),"PSD"_(3_"max")] $
// $ and "Energy"(s)       in[E_"min", E_"max"] $
// $ and "Max_Dist"(s)    in[D_(M_"min"), D_(M_"max")] $
// $ and exists P_j in P: "DTW"(s, P_j) in [0, d_("max",c)] $


#set heading(numbering: "1.")
#show heading.where(level: 1): it => {
  pagebreak(weak: true)
  it
}
#include "1-introducción.typ"
#include "2-estado-del-arte.typ"
#include "3-análisis.typ"
#include "4-diseño.typ"
#include "5-implementación-y-despliegue.typ"
#include "6-validación-verificación-y-evaluación.typ"
#include "7-planificación-y-análisis-de-costes.typ"
#include "8-marco-regulatorio.typ"
#include "9-conclusiones-y-trabajo-futuro.typ"
