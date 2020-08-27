* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.
RECODE Sex ('male'=0) ('female'=1) (MISSING=SYSMIS) (ELSE=SYSMIS) INTO sex_dummy.
EXECUTE.

DATASET ACTIVATE DataSet1.
LOGISTIC REGRESSION VARIABLES Survived
  /METHOD=ENTER Age Pclass sibsp_rec parch_rec INT_sibsp_parch_rec is_female 
  /CONTRAST (Pclass)=Indicator
  /CONTRAST (sibsp_rec)=Indicator
  /CONTRAST (parch_rec)=Indicator
  /CONTRAST (INT_sibsp_parch_rec)=Indicator
  /CONTRAST (is_female)=Indicator
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).
