* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.
* Delete existing single-variable validation rules.
DATAFILE ATTRIBUTE DELETE=$VD.SRule.
* Delete existing links between variables and rules.
VARIABLE ATTRIBUTE VARIABLES=ALL DELETE=$VD.SRuleRef.
* (Re)define single-variable validation rules.
DATAFILE ATTRIBUTE ATTRIBUTE=
   $VD.SRule[1]("Label='20-80', Type='Numeric', Domain='Range', Minimum='20', Maximum='80', "+
    "FlagUserMissing='Yes', FlagSystemMissing='Yes', FlagBlank='No', FlagNoninteger='Yes', "+
    "FlagUnlabeled='No' ")
   $VD.SRule[2]("Label='1-10', Type='Numeric', Domain='Range', Minimum='1', Maximum='10', "+
    "FlagUserMissing='Yes', FlagSystemMissing='Yes', FlagBlank='No', FlagNoninteger='Yes', "+
    "FlagUnlabeled='No' ")
   $VD.SRule[3]("Label='0-52', Type='Numeric', Domain='Range', Minimum='0', Maximum='52', "+
    "FlagUserMissing='Yes', FlagSystemMissing='Yes', FlagBlank='No', FlagNoninteger='Yes', "+
    "FlagUnlabeled='No' ")
   $VD.SRule[4]("Label='1-6', Type='Numeric', Domain='Range', Minimum='1', Maximum='6', "+
    "FlagUserMissing='Yes', FlagSystemMissing='Yes', FlagBlank='No', FlagNoninteger='No', "+
    "FlagUnlabeled='No' ")
   $VD.SRule[5]("Label='sexrule', Type='String', Domain='List', FlagUserMissing='Yes', "+
    "FlagSystemMissing='No', FlagBlank='Yes', CaseSensitive='No',List='male' 'female'  ")
   $VD.SRule[6]("Label='18-100', Type='Numeric', Domain='Range', Minimum='18', Maximum='100', "+
    "FlagUserMissing='Yes', FlagSystemMissing='Yes', FlagBlank='No', FlagNoninteger='Yes', "+
    "FlagUnlabeled='No' ")
   $VD.SRule[7]("Label='cortisol', Type='Numeric', Domain='Range', Minimum='1', Maximum='10', "+
    "FlagUserMissing='Yes', FlagSystemMissing='Yes', FlagBlank='No', FlagNoninteger='No', "+
    "FlagUnlabeled='No' ").
* (Re)define links between variables and rules.
VARIABLE ATTRIBUTE
    VARIABLES=pain ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[2]',OutcomeVar='@110_pain'")
    /VARIABLES=STAI_trait ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@2080_STAI_trait'")
    /VARIABLES=pain_cat ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[3]',OutcomeVar='@052_pain_cat'")
    /VARIABLES=mindfulness ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[4]',OutcomeVar='@16_mindfulness'")
    /VARIABLES=sex ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[5]',OutcomeVar='sexrule_sex'")
    /VARIABLES=age ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[6]',OutcomeVar='@18100_age'")
    /VARIABLES=cortisol_saliva ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[7]',OutcomeVar='cortisol_cortisol_saliva'")
    /VARIABLES=cortisol_serum ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[7]',OutcomeVar='cortisol_cortisol_serum'").
TEMPORARY.
* 0-52.
COMPUTE @052_pain_cat=NOT(VALUE(pain_cat)>=0 AND VALUE(pain_cat)<=52 AND 
    VALUE(pain_cat)=TRUNC(VALUE(pain_cat)) AND NOT(MISSING(pain_cat))).
* 1-10.
COMPUTE @110_pain=NOT(VALUE(pain)>=1 AND VALUE(pain)<=10 AND VALUE(pain)=TRUNC(VALUE(pain)) AND 
    NOT(MISSING(pain))).
* 1-6.
COMPUTE @16_mindfulness=NOT(VALUE(mindfulness)>=1 AND VALUE(mindfulness)<=6 AND 
    NOT(MISSING(mindfulness))).
* 18-100.
COMPUTE @18100_age=NOT(VALUE(age)>=18 AND VALUE(age)<=100 AND VALUE(age)=TRUNC(VALUE(age)) AND 
    NOT(MISSING(age))).
* 20-80.
COMPUTE @2080_STAI_trait=NOT(VALUE(STAI_trait)>=20 AND VALUE(STAI_trait)<=80 AND 
    VALUE(STAI_trait)=TRUNC(VALUE(STAI_trait)) AND NOT(MISSING(STAI_trait))).
* cortisol.
COMPUTE cortisol_cortisol_saliva=NOT(VALUE(cortisol_saliva)>=1 AND VALUE(cortisol_saliva)<=10 AND 
    NOT(MISSING(cortisol_saliva))).
COMPUTE cortisol_cortisol_serum=NOT(VALUE(cortisol_serum)>=1 AND VALUE(cortisol_serum)<=10 AND 
    NOT(MISSING(cortisol_serum))).
* sexrule.
COMPUTE sexrule_sex=NOT(ANY(LOWER(sex),'male','female')) OR MISSING(sex) OR sex=''.
* Mark rule outcome variables as such in SPSS Statistics data dictionary.
VARIABLE ATTRIBUTE VARIABLES=@052_pain_cat TO sexrule_sex ATTRIBUTE=$VD.RuleOutcomeVar("Yes").
* Validate Data.
VALIDATEDATA VARIABLES=pain STAI_trait pain_cat mindfulness sex age cortisol_saliva cortisol_serum
  /VARCHECKS STATUS=ON PCTMISSING=70 PCTEQUAL=95 PCTUNEQUAL=90 CV=0.001 STDDEV=0
  /CASECHECKS REPORTEMPTY=YES SCOPE=ALLVARS
  /CASEREPORT DISPLAY=YES MINVIOLATIONS=10 CASELIMIT=FIRSTN(300)
  /RULESUMMARIES BYVARIABLE.

DATASET ACTIVATE DataSet2.
FREQUENCIES VARIABLES=pain age STAI_trait pain_cat cortisol_serum cortisol_saliva mindfulness
  /NTILES=4
  /NTILES=4
  /STATISTICS=STDDEV VARIANCE RANGE MINIMUM MAXIMUM SEMEAN MEAN MEDIAN MODE SKEWNESS SESKEW 
    KURTOSIS SEKURT
  /HISTOGRAM NORMAL
  /ORDER=ANALYSIS.

DESCRIPTIVES VARIABLES=pain age STAI_trait pain_cat cortisol_serum cortisol_saliva mindfulness
  /STATISTICS=MEAN STDDEV VARIANCE RANGE MIN MAX SEMEAN KURTOSIS SKEWNESS.

EXAMINE VARIABLES=pain age STAI_trait pain_cat cortisol_serum cortisol_saliva mindfulness
  /PLOT BOXPLOT HISTOGRAM NPPLOT
  /COMPARE VARIABLES
  /STATISTICS DESCRIPTIVES EXTREME
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

RECODE sex ('female'=1) ('male'=0) INTO is_female.
EXECUTE.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) BCOV R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT pain
  /METHOD=ENTER age is_female
  /METHOD=ENTER age is_female STAI_trait pain_cat cortisol_serum cortisol_saliva mindfulness
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS NORMPROB(ZRESID)
  /SAVE PRED COOK RESID.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=ID COO_1 MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: ID=col(source(s), name("ID"), unit.category())
  DATA: COO_1=col(source(s), name("COO_1"))
  GUIDE: axis(dim(1), label("ID"))
  GUIDE: axis(dim(2), label("Cook's Distance"))
  GUIDE: text.title(label("Simple Scatter of Cook's Distance by ID"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: point(position(ID*COO_1))
END GPL.

EXAMINE VARIABLES=RES_1
  /PLOT HISTOGRAM NPPLOT
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

COMPUTE sq_unstand_resid=RES_1 * RES_1.
EXECUTE.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT sq_unstand_resid
  /METHOD=ENTER age is_female
  /METHOD=ENTER age STAI_trait pain_cat cortisol_serum cortisol_saliva mindfulness is_female
  /SCATTERPLOT=(*ZRESID ,*ZPRED).

DATASET ACTIVATE DataSet1.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) BCOV R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT pain
  /METHOD=ENTER age is_female
  /METHOD=ENTER STAI_trait pain_cat cortisol_serum mindfulness
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS NORMPROB(ZRESID)
  /SAVE PRED COOK RESID.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=ID COO_2 MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: ID=col(source(s), name("ID"), unit.category())
  DATA: COO_2=col(source(s), name("COO_2"))
  GUIDE: axis(dim(1), label("ID"))
  GUIDE: axis(dim(2), label("Cook's Distance"))
  GUIDE: text.title(label("Simple Scatter of Cook's Distance by ID"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: point(position(ID*COO_2))
END GPL.

EXAMINE VARIABLES=RES_2
  /PLOT BOXPLOT STEMLEAF HISTOGRAM NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

COMPUTE sq_unstand_res_2=RES_2 * RES_2.
EXECUTE.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) BCOV R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT sq_unstand_res_2
  /METHOD=ENTER age is_female
  /METHOD=ENTER STAI_trait pain_cat cortisol_serum mindfulness
  /SCATTERPLOT=(*ZRESID ,*ZPRED).

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) BCOV R ANOVA CHANGE SELECTION
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT pain
  /METHOD=ENTER age is_female
  /METHOD=ENTER STAI_trait pain_cat cortisol_serum mindfulness
  /SCATTERPLOT=(*ZRESID ,*ZPRED).
