* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.
* Delete existing single-variable validation rules.
DATAFILE ATTRIBUTE DELETE=$VD.SRule.
* Delete existing links between variables and rules.
VARIABLE ATTRIBUTE VARIABLES=ALL DELETE=$VD.SRuleRef.
* (Re)define single-variable validation rules.
DATAFILE ATTRIBUTE ATTRIBUTE=
   $VD.SRule[1]("Label='0-10', Type='Numeric', Domain='Range', Minimum='0', Maximum='10', "+
    "FlagUserMissing='Yes', FlagSystemMissing='Yes', FlagBlank='No', FlagNoninteger='Yes', "+
    "FlagUnlabeled='No' ")
   $VD.SRule[2]("Label='sex', Type='String', Domain='List', FlagUserMissing='Yes', "+
    "FlagSystemMissing='No', FlagBlank='Yes', CaseSensitive='Yes',List='male' 'female'  ")
   $VD.SRule[3]("Label='age', Type='Numeric', Domain='Range', Minimum='0', Maximum='100', "+
    "FlagUserMissing='Yes', FlagSystemMissing='Yes', FlagBlank='No', FlagNoninteger='Yes', "+
    "FlagUnlabeled='No' ")
   $VD.SRule[4]("Label='0-52', Type='Numeric', Domain='Range', Minimum='0', Maximum='52', "+
    "FlagUserMissing='Yes', FlagSystemMissing='Yes', FlagBlank='No', FlagNoninteger='Yes', "+
    "FlagUnlabeled='No' ")
   $VD.SRule[5]("Label='20-80', Type='Numeric', Domain='Range', Minimum='20', Maximum='80', "+
    "FlagUserMissing='Yes', FlagSystemMissing='Yes', FlagBlank='No', FlagNoninteger='Yes', "+
    "FlagUnlabeled='No' ")
   $VD.SRule[6]("Label='1-6', Type='Numeric', Domain='Range', Minimum='1', Maximum='6', "+
    "FlagUserMissing='Yes', FlagSystemMissing='Yes', FlagBlank='No', FlagNoninteger='No', "+
    "FlagUnlabeled='No' ")
   $VD.SRule[7]("Label='1-8', Type='Numeric', Domain='Range', Minimum='1', Maximum='8', "+
    "FlagUserMissing='Yes', FlagSystemMissing='Yes', FlagBlank='No', FlagNoninteger='No', "+
    "FlagUnlabeled='No' ")
   $VD.SRule[8]("Label='hospital', Type='String', Domain='List', FlagUserMissing='Yes', "+
    "FlagSystemMissing='No', FlagBlank='Yes', CaseSensitive='Yes',List='hospital_1' 'hospital_2' "+
    "'hospital_3' 'hospital_4' 'hospital_5' 'hospital_6' 'hospital_7' 'hospital_8' 'hospital_9' "+
    "'hospital_10'  ").
* (Re)define links between variables and rules.
VARIABLE ATTRIBUTE
    VARIABLES=pain ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@010_pain'")
    /VARIABLES=sex ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[2]',OutcomeVar='sex_sex'")
    /VARIABLES=age ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[3]',OutcomeVar='age_age'")
    /VARIABLES=STAI_trait ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[5]',OutcomeVar='@2080_STAI_trait'")
    /VARIABLES=pain_cat ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[4]',OutcomeVar='@052_pain_cat'")
    /VARIABLES=cortisol_serum ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[7]',OutcomeVar='@18_cortisol_serum'")
    /VARIABLES=mindfulness ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[6]',OutcomeVar='@16_mindfulness'")
    /VARIABLES=hospital ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[8]',OutcomeVar='hospital_hospital'").
TEMPORARY.
* 0-10.
COMPUTE @010_pain=NOT(VALUE(pain)>=0 AND VALUE(pain)<=10 AND VALUE(pain)=TRUNC(VALUE(pain)) AND 
    NOT(MISSING(pain))).
* 0-52.
COMPUTE @052_pain_cat=NOT(VALUE(pain_cat)>=0 AND VALUE(pain_cat)<=52 AND 
    VALUE(pain_cat)=TRUNC(VALUE(pain_cat)) AND NOT(MISSING(pain_cat))).
* 1-6.
COMPUTE @16_mindfulness=NOT(VALUE(mindfulness)>=1 AND VALUE(mindfulness)<=6 AND 
    NOT(MISSING(mindfulness))).
* 1-8.
COMPUTE @18_cortisol_serum=NOT(VALUE(cortisol_serum)>=1 AND VALUE(cortisol_serum)<=8 AND 
    NOT(MISSING(cortisol_serum))).
* 20-80.
COMPUTE @2080_STAI_trait=NOT(VALUE(STAI_trait)>=20 AND VALUE(STAI_trait)<=80 AND 
    VALUE(STAI_trait)=TRUNC(VALUE(STAI_trait)) AND NOT(MISSING(STAI_trait))).
* age.
COMPUTE age_age=NOT(VALUE(age)>=0 AND VALUE(age)<=100 AND VALUE(age)=TRUNC(VALUE(age)) AND 
    NOT(MISSING(age))).
* hospital.
COMPUTE hospital_hospital=NOT(ANY(hospital,'hospital_1','hospital_2','hospital_3','hospital_4',
    'hospital_5','hospital_6','hospital_7','hospital_8','hospital_9','hospital_10')) OR 
    MISSING(hospital) OR hospital=''.
* sex.
COMPUTE sex_sex=NOT(ANY(sex,'male','female')) OR MISSING(sex) OR sex=''.
* Mark rule outcome variables as such in SPSS Statistics data dictionary.
VARIABLE ATTRIBUTE VARIABLES=@010_pain TO sex_sex ATTRIBUTE=$VD.RuleOutcomeVar("Yes").
* Validate Data.
VALIDATEDATA VARIABLES=pain sex age STAI_trait pain_cat cortisol_serum mindfulness hospital
  /VARCHECKS STATUS=ON PCTMISSING=70 PCTEQUAL=95 PCTUNEQUAL=90 CV=0.001 STDDEV=0
  /CASECHECKS REPORTEMPTY=YES SCOPE=ALLVARS
  /CASEREPORT DISPLAY=YES MINVIOLATIONS=1 CASELIMIT=FIRSTN(100)
  /RULESUMMARIES BYVARIABLE.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=cortisol_serum pain hospital MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO SUBGROUP=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: cortisol_serum=col(source(s), name("cortisol_serum"))
  DATA: pain=col(source(s), name("pain"), unit.category())
  DATA: hospital=col(source(s), name("hospital"), unit.category())
  GUIDE: axis(dim(1), label("cortisol_serum"))
  GUIDE: axis(dim(2), label("pain"))
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("hospital"))
  GUIDE: text.title(label("Grouped Scatter of pain by cortisol_serum by hospital"))
  ELEMENT: point(position(cortisol_serum*pain), color.interior(hospital))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=mindfulness pain hospital MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO SUBGROUP=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: mindfulness=col(source(s), name("mindfulness"))
  DATA: pain=col(source(s), name("pain"), unit.category())
  DATA: hospital=col(source(s), name("hospital"), unit.category())
  GUIDE: axis(dim(1), label("mindfulness"))
  GUIDE: axis(dim(2), label("pain"))
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("hospital"))
  GUIDE: text.title(label("Grouped Scatter of pain by mindfulness by hospital"))
  ELEMENT: point(position(mindfulness*pain), color.interior(hospital))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=STAI_trait pain hospital MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO SUBGROUP=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: STAI_trait=col(source(s), name("STAI_trait"))
  DATA: pain=col(source(s), name("pain"), unit.category())
  DATA: hospital=col(source(s), name("hospital"), unit.category())
  GUIDE: axis(dim(1), label("STAI_trait"))
  GUIDE: axis(dim(2), label("pain"))
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("hospital"))
  GUIDE: text.title(label("Grouped Scatter of pain by STAI_trait by hospital"))
  ELEMENT: point(position(STAI_trait*pain), color.interior(hospital))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=pain_cat pain hospital MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO SUBGROUP=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: pain_cat=col(source(s), name("pain_cat"))
  DATA: pain=col(source(s), name("pain"), unit.category())
  DATA: hospital=col(source(s), name("hospital"), unit.category())
  GUIDE: axis(dim(1), label("pain_cat"))
  GUIDE: axis(dim(2), label("pain"))
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("hospital"))
  GUIDE: text.title(label("Grouped Scatter of pain by pain_cat by hospital"))
  ELEMENT: point(position(pain_cat*pain), color.interior(hospital))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=age pain hospital MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO SUBGROUP=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: age=col(source(s), name("age"))
  DATA: pain=col(source(s), name("pain"), unit.category())
  DATA: hospital=col(source(s), name("hospital"), unit.category())
  GUIDE: axis(dim(1), label("age"))
  GUIDE: axis(dim(2), label("pain"))
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("hospital"))
  GUIDE: text.title(label("Grouped Scatter of pain by age by hospital"))
  ELEMENT: point(position(age*pain), color.interior(hospital))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=sex pain hospital MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: sex=col(source(s), name("sex"), unit.category())
  DATA: pain=col(source(s), name("pain"), unit.category())
  DATA: hospital=col(source(s), name("hospital"), unit.category())
  COORD: rect(dim(1,2), cluster(3,0))
  GUIDE: axis(dim(3), label("sex"))
  GUIDE: axis(dim(2), label("pain"))
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("hospital"))
  GUIDE: text.title(label("Clustered Bar of pain by sex by hospital"))
  ELEMENT: interval(position(hospital*pain*sex), color.interior(hospital), 
    shape.interior(shape.square))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=cortisol_serum pain hospital MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO SUBGROUP=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: cortisol_serum=col(source(s), name("cortisol_serum"))
  DATA: pain=col(source(s), name("pain"), unit.category())
  DATA: hospital=col(source(s), name("hospital"), unit.category())
  GUIDE: axis(dim(1), label("cortisol_serum"))
  GUIDE: axis(dim(2), label("pain"))
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("hospital"))
  GUIDE: text.title(label("Grouped Scatter of pain by cortisol_serum by hospital"))
  ELEMENT: point(position(cortisol_serum*pain), color.interior(hospital))
  ELEMENT: line(position(smooth.linear(cortisol_serum*pain)), split(hospital)))
END GPL.

SPSSINC CREATE DUMMIES VARIABLE=sex 
ROOTNAME1=sex 
/OPTIONS ORDER=A USEVALUELABELS=YES USEML=YES OMITFIRST=NO.

  MIXED pain BY is_female WITH age STAI_trait pain_cat cortisol_serum mindfulness
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1) 
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)    
  /FIXED=is_female age STAI_trait pain_cat cortisol_serum mindfulness | SSTYPE(3)
  /METHOD=REML
  /PRINT=CORB  SOLUTION
  /RANDOM=INTERCEPT | SUBJECT(hospital) COVTYPE(VC)
  /SAVE=FIXPRED.

  MIXED pain BY is_female WITH age STAI_trait pain_cat cortisol_serum mindfulness
  /CRITERIA=DFMETHOD(SATTERTHWAITE) CIN(95) MXITER(100) MXSTEP(10) SCORING(1) 
    SINGULAR(0.000000000001) HCONVERGE(0, ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)    
  /FIXED=| SSTYPE(3)
  /METHOD=REML
  /PRINT=CORB  SOLUTION
  /RANDOM=INTERCEPT | SUBJECT(hospital) COVTYPE(VC).

DESCRIPTIVES VARIABLES=FXPRED_2
  /STATISTICS=MEAN STDDEV VARIANCE MIN MAX.


