* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.
CORRELATIONS
  /VARIABLES=pain1 pain2 pain3 pain4
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

VARSTOCASES
  /MAKE pain FROM pain1 pain2 pain3 pain4
  /INDEX=day(4) 
  /KEEP=ID sex age STAI_trait pain_cat cortisol_serum cortisol_saliva mindfulness weight IQ 
    household_income
  /NULL=KEEP.

DATASET ACTIVATE DataSet1.
SPSSINC CREATE DUMMIES VARIABLE=sex 
ROOTNAME1=sex 
/OPTIONS ORDER=A USEVALUELABELS=YES USEML=YES OMITFIRST=NO.

MIXED pain BY Is_female WITH age STAI_trait pain_cat cortisol_serum mindfulness day
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1) 
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)    
  /FIXED=Is_female age STAI_trait pain_cat cortisol_serum mindfulness day | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT | SUBJECT(ID) COVTYPE(VC).

MIXED pain BY Is_female WITH age STAI_trait pain_cat cortisol_serum mindfulness day
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1) 
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)    
  /FIXED=Is_female age STAI_trait pain_cat cortisol_serum mindfulness day | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT day | SUBJECT(ID) COVTYPE(UN).

VARSTOCASES
  /MAKE pain FROM pain pred_slope pred_intercept
  /INDEX=obs_or_pred(pain) 
  /KEEP=ID sex age STAI_trait pain_cat cortisol_serum cortisol_saliva mindfulness weight IQ 
    household_income day Is_female Is_male
  /NULL=KEEP.

SORT CASES  BY ID.
SPLIT FILE SEPARATE BY ID.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=day pain obs_or_pred MISSING=LISTWISE REPORTMISSING=NO    
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO SUBGROUP=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: day=col(source(s), name("day"), unit.category())
  DATA: pain=col(source(s), name("pain"), unit.category())
  DATA: obs_or_pred=col(source(s), name("obs_or_pred"), unit.category())
  GUIDE: axis(dim(1), label("day"))
  GUIDE: axis(dim(2), label("pain"))
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("obs_or_pred"))
  GUIDE: text.title(label("Grouped Scatter of pain by day by obs_or_pred"))
  ELEMENT: point(position(day*pain), color.interior(obs_or_pred))
  ELEMENT: line(position(day*pain), color.interior(obs_or_pred), missing.wings())
END GPL.

SPLIT FILE OFF.

DATASET ACTIVATE DataSet2.
COMPUTE day_centered=day - 2.50.
EXECUTE.

COMPUTE day_centered_squared=day_centered * day_centered.
EXECUTE.

MIXED pain BY Is_female WITH age STAI_trait pain_cat cortisol_serum mindfulness day_centered 
    day_centered_squared
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1) 
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)    
  /FIXED=Is_female age STAI_trait pain_cat cortisol_serum mindfulness day_centered 
    day_centered_squared | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=INTERCEPT day_centered day_centered_squared | SUBJECT(ID) COVTYPE(UN)
  /SAVE=PRED.

VARSTOCASES
  /MAKE pain FROM pred_slope pred_slope_daysquared pain
  /INDEX=obs_or_pred(pain) 
  /KEEP=ID sex age STAI_trait pain_cat cortisol_serum cortisol_saliva mindfulness weight IQ 
    household_income day Is_female Is_male pred_intercept day_centered day_centered_squared
  /NULL=KEEP.

SORT CASES  BY ID.
SPLIT FILE SEPARATE BY ID.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=day pain obs_or_pred MISSING=LISTWISE REPORTMISSING=NO    
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO SUBGROUP=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: day=col(source(s), name("day"), unit.category())
  DATA: pain=col(source(s), name("pain"))
  DATA: obs_or_pred=col(source(s), name("obs_or_pred"), unit.category())
  GUIDE: axis(dim(1), label("day"))
  GUIDE: axis(dim(2), delta(1), label("Predicted Values"))
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("obs_or_pred"))
  GUIDE: text.title(label("Grouped Scatter of Predicted Values by day by obs_or_pred"))
  SCALE: linear(dim(2), min(0), max(9), origin(0))
  ELEMENT: point(position(day*pain), color.interior(obs_or_pred))
  ELEMENT: line(position(day*pain), color.interior(obs_or_pred), missing.wings())
END GPL.
