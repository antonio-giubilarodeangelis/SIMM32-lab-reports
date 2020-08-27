* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.
* (Re)define single-variable validation rules.
DATAFILE ATTRIBUTE ATTRIBUTE=
   $VD.SRule[1]("Label='1-5', Type='Numeric', Domain='Range', Minimum='1', Maximum='5', "+
    "FlagUserMissing='Yes', FlagSystemMissing='Yes', FlagBlank='No', FlagNoninteger='Yes', "+
    "FlagUnlabeled='No' ")
   $VD.SRule[2]("Label='1-4', Type='Numeric', Domain='Range', Minimum='1', Maximum='4', "+
    "FlagUserMissing='Yes', FlagSystemMissing='Yes', FlagBlank='No', FlagNoninteger='Yes', "+
    "FlagUnlabeled='No' ")
   $VD.SRule[3]("Label='1-2', Type='Numeric', Domain='List', FlagUserMissing='Yes', "+
    "FlagSystemMissing='Yes', FlagBlank='No', CaseSensitive='No',List='1' '2'  ").
* (Re)define links between variables and rules.
VARIABLE ATTRIBUTE
    VARIABLES=ar1 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar1'")
    /VARIABLES=ar2 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar2'")
    /VARIABLES=ar3 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar3'")
    /VARIABLES=ar4 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar4'")
    /VARIABLES=ar5 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar5'")
    /VARIABLES=ar6 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar6'")
    /VARIABLES=ar7 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar7'")
    /VARIABLES=ar8 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar8'")
    /VARIABLES=ar9 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar9'")
    /VARIABLES=ar10 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar10'")
    /VARIABLES=ar11 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar11'")
    /VARIABLES=ar12 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar12'")
    /VARIABLES=ar13 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar13'")
    /VARIABLES=ar14 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar14'")
    /VARIABLES=ar15 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar15'")
    /VARIABLES=ar16 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar16'")
    /VARIABLES=ar17 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar17'")
    /VARIABLES=ar18 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar18'")
    /VARIABLES=ar19 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar19'")
    /VARIABLES=ar20 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar20'")
    /VARIABLES=ar21 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar21'")
    /VARIABLES=ar22 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar22'")
    /VARIABLES=ar23 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar23'")
    /VARIABLES=ar24 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar24'")
    /VARIABLES=ar25 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar25'")
    /VARIABLES=ar26 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar26'")
    /VARIABLES=ar27 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar27'")
    /VARIABLES=ar28 ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_ar28'")
    /VARIABLES=sex ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[3]',OutcomeVar='@12_sex'")
    /VARIABLES=party ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[2]',OutcomeVar='@14_party'")
    /VARIABLES=liberal ATTRIBUTE=
      $VD.SRuleRef[1]("Rule='$VD.SRule[1]',OutcomeVar='@15_liberal'").
TEMPORARY.
* 1-2.
COMPUTE @12_sex=NOT(ANY(VALUE(sex),1,2) AND NOT(MISSING(sex))).
* 1-4.
COMPUTE @14_party=NOT(VALUE(party)>=1 AND VALUE(party)<=4 AND VALUE(party)=TRUNC(VALUE(party)) AND 
    NOT(MISSING(party))).
* 1-5.
DO REPEAT #OV= @15_ar3 @15_ar27 @15_ar2 @15_ar26 @15_ar1 @15_ar28 @15_ar7 @15_ar6 @15_ar5 @15_ar4 
    @15_ar9 @15_ar8 @15_ar23 @15_ar22 @15_ar25 @15_liberal @15_ar24 @15_ar21 @15_ar20 @15_ar19 @15_ar16 
    @15_ar15 @15_ar18 @15_ar17 @15_ar12 @15_ar11 @15_ar14 @15_ar13 @15_ar10
  /#IV= ar3 ar27 ar2 ar26 ar1 ar28 ar7 ar6 ar5 ar4 ar9 ar8 ar23 ar22 ar25 liberal ar24 ar21 ar20 
    ar19 ar16 ar15 ar18 ar17 ar12 ar11 ar14 ar13 ar10.
COMPUTE #OV=NOT(VALUE(#IV)>=1 AND VALUE(#IV)<=5 AND VALUE(#IV)=TRUNC(VALUE(#IV)) AND 
    NOT(MISSING(#IV))).
END REPEAT.
* Mark rule outcome variables as such in SPSS Statistics data dictionary.
VARIABLE ATTRIBUTE VARIABLES=@12_sex TO @15_ar10 ATTRIBUTE=$VD.RuleOutcomeVar("Yes").
* Validate Data.
VALIDATEDATA VARIABLES=ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 
    ar18 ar19 ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28 sex party liberal
  /VARCHECKS STATUS=ON PCTMISSING=70 PCTEQUAL=95 PCTUNEQUAL=90
  /CASECHECKS REPORTEMPTY=YES SCOPE=ALLVARS
  /CASEREPORT DISPLAY=YES MINVIOLATIONS=1 CASELIMIT=FIRSTN(100)
  /RULESUMMARIES BYRULE.
* Summaries of categorical analysis variables.
FREQUENCIES VARIABLES=ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 
    ar18 ar19 ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28 sex party liberal.

DATASET ACTIVATE DataSet1.
DESCRIPTIVES VARIABLES=ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 
    ar18 ar19 ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28 sex party liberal
  /STATISTICS=MEAN STDDEV VARIANCE MIN MAX.

CORRELATIONS
  /VARIABLES=ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28 sex party liberal
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

FACTOR
  /VARIABLES ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /PRINT INITIAL KMO EXTRACTION
  /PLOT EIGEN
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION ML
  /ROTATION NOROTATE.

* Parallel Analysis Program For Raw Data and Data Permutations.

* To run this program you need to first specify the data
  for analysis and then RUN, all at once, the commands
  from the MATRIX statement to the END MATRIX statement.

* This program conducts parallel analyses on data files in which
  the rows of the data matrix are cases/individuals and the
  columns are variables;  Data are read/entered into the program
  using the GET command (see the GET command below);  The GET 
  command reads an SPSS data file, which can be either the 
  current, active SPSS data file or a previously saved data file;
  A valid filename/location must be specified on the GET command;
  A subset of variables for the analyses can be specified by using
  the "/ VAR =" subcommand with the GET statement;  There can be
  no missing values.

* You must also specify:
  -- the # of parallel data sets for the analyses;
  -- the desired percentile of the distribution and random
     data eigenvalues;
  -- whether principal components analyses or principal axis/common
     factor analysis are to be conducted, and
  -- whether normally distributed random data generation or 
     permutations of the raw data set are to be used in the
     parallel analyses.

* Permutations of the raw data set can be time consuming;
  Each parallel data set is based on column-wise random shufflings
  of the values in the raw data matrix using Castellan's (1992, 
  BRMIC, 24, 72-77) algorithm; The distributions of the original 
  raw variables are exactly preserved in the shuffled versions used
  in the parallel analyses; Permutations of the raw data set are
  thus highly accurate and most relevant, especially in cases where
  the raw data are not normally distributed or when they do not meet
  the assumption of multivariate normality (see Longman & Holden,
  1992, BRMIC, 24, 493, for a Fortran version); If you would
  like to go this route, it is perhaps best to (1) first run a 
  normally distributed random data generation parallel analysis to
  familiarize yourself with the program and to get a ballpark
  reference point for the number of factors/components;
  (2) then run a permutations of the raw data parallel analysis
  using a small number of datasets (e.g., 100), just to see how long
  the program takes to run; then (3) run a permutations of the raw
  data parallel analysis using the number of parallel data sets that
  you would like use for your final analyses; 1000 datasets are 
  usually sufficient, although more datasets should be used if
  there are close calls.


* These next commands generate artificial raw data 
  (500 cases) that can be used for a trial-run of
  the program, instead of using your own raw data; 
  Just select and run this whole file; However, make sure to
  delete the artificial data commands before attempting to
  run your own data.

set mxloops=9000 printback=off width=80  seed = 1953125.
matrix.

* Enter the name/location of the data file for analyses after "FILE =";
  If you specify "FILE = *", then the program will read the current,
  active SPSS data file; Alternatively, enter the name/location
  of a previously saved SPSS data file instead of "*";
  you can use the "/ VAR =" subcommand after "/ missing=omit"
  subcommand to select variables for the analyses.
GET raw / FILE = * / missing=omit / VAR = ar1 to ar28.

* Enter the desired number of parallel data sets here.
compute ndatsets = 1000.

* Enter the desired percentile here.
compute percent  = 95.

* Enter either
  1 for principal components analysis, or
  2 for principal axis/common factor analysis.
compute kind = 1 .

* Enter either
  1 for normally distributed random data generation parallel analysis, or
  2 for permutations of the raw data set.
compute randtype = 1.


****************** End of user specifications. ******************

compute ncases   = nrow(raw). 
compute nvars    = ncol(raw).

* principal components analysis & random normal data generation.
do if (kind = 1 and randtype = 1).
compute nm1 = 1 / (ncases-1).
compute vcv = nm1 * (sscp(raw) - ((t(csum(raw))*csum(raw))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute realeval = eval(d * vcv * d).
compute evals = make(nvars,ndatsets,-9999).
loop #nds = 1 to ndatsets.
compute x = sqrt(2 * (ln(uniform(ncases,nvars)) * -1) ) &*
            cos(6.283185 * uniform(ncases,nvars) ).
compute vcv = nm1 * (sscp(x) - ((t(csum(x))*csum(x))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute evals(:,#nds) = eval(d * vcv * d).
end loop.
end if.

* principal components analysis & raw data permutation.
do if (kind = 1 and randtype = 2).
compute nm1 = 1 / (ncases-1).
compute vcv = nm1 * (sscp(raw) - ((t(csum(raw))*csum(raw))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute realeval = eval(d * vcv * d).
compute evals = make(nvars,ndatsets,-9999).
loop #nds = 1 to ndatsets.
compute x = raw.
loop #c = 1 to nvars.
loop #r = 1 to (ncases -1).
compute k = trunc( (ncases - #r + 1) * uniform(1,1) + 1 )  + #r - 1.
compute d = x(#r,#c).
compute x(#r,#c) = x(k,#c).
compute x(k,#c) = d.
end loop.
end loop.
compute vcv = nm1 * (sscp(x) - ((t(csum(x))*csum(x))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute evals(:,#nds) = eval(d * vcv * d).
end loop.
end if.

* PAF/common factor analysis & random normal data generation.
do if (kind = 2 and randtype = 1).
compute nm1 = 1 / (ncases-1).
compute vcv = nm1 * (sscp(raw) - ((t(csum(raw))*csum(raw))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute cr = (d * vcv * d).
compute smc = 1 - (1 &/ diag(inv(cr)) ).
call setdiag(cr,smc).
compute realeval = eval(cr).
compute evals = make(nvars,ndatsets,-9999).
compute nm1 = 1 / (ncases-1).
loop #nds = 1 to ndatsets.
compute x = sqrt(2 * (ln(uniform(ncases,nvars)) * -1) ) &*
            cos(6.283185 * uniform(ncases,nvars) ).
compute vcv = nm1 * (sscp(x) - ((t(csum(x))*csum(x))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute r = d * vcv * d.
compute smc = 1 - (1 &/ diag(inv(r)) ).
call setdiag(r,smc).
compute evals(:,#nds) = eval(r).
end loop.
end if.

* PAF/common factor analysis & raw data permutation.
do if (kind = 2 and randtype = 2).
compute nm1 = 1 / (ncases-1).
compute vcv = nm1 * (sscp(raw) - ((t(csum(raw))*csum(raw))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute cr = (d * vcv * d).
compute smc = 1 - (1 &/ diag(inv(cr)) ).
call setdiag(cr,smc).
compute realeval = eval(cr).
compute evals = make(nvars,ndatsets,-9999).
compute nm1 = 1 / (ncases-1).
loop #nds = 1 to ndatsets.
compute x = raw.
loop #c = 1 to nvars.
loop #r = 1 to (ncases -1).
compute k = trunc( (ncases - #r + 1) * uniform(1,1) + 1 )  + #r - 1.
compute d = x(#r,#c).
compute x(#r,#c) = x(k,#c).
compute x(k,#c) = d.
end loop.
end loop.
compute vcv = nm1 * (sscp(x) - ((t(csum(x))*csum(x))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute r = d * vcv * d.
compute smc = 1 - (1 &/ diag(inv(r)) ).
call setdiag(r,smc).
compute evals(:,#nds) = eval(r).
end loop.
end if.

* identifying the eigenvalues corresponding to the desired percentile.
compute num = rnd((percent*ndatsets)/100).
compute results = { t(1:nvars), realeval, t(1:nvars), t(1:nvars) }.
loop #root = 1 to nvars.
compute ranks = rnkorder(evals(#root,:)).
loop #col = 1 to ndatsets.
do if (ranks(1,#col) = num).
compute results(#root,4) = evals(#root,#col).
break.
end if.
end loop.
end loop.
compute results(:,3) = rsum(evals) / ndatsets.

print /title="PARALLEL ANALYSIS:".
do if (kind = 1 and randtype = 1).
print /title="Principal Components & Random Normal Data Generation".
else if (kind = 1 and randtype = 2).
print /title="Principal Components & Raw Data Permutation".
else if (kind = 2 and randtype = 1).
print /title="PAF/Common Factor Analysis & Random Normal Data Generation".
else if (kind = 2 and randtype = 2).
print /title="PAF/Common Factor Analysis & Raw Data Permutation".
end if.
compute specifs = {ncases; nvars; ndatsets; percent}.
print specifs /title="Specifications for this Run:"
 /rlabels="Ncases" "Nvars" "Ndatsets" "Percent".
print results 
 /title="Raw Data Eigenvalues, & Mean & Percentile Random Data Eigenvalues"
 /clabels="Root" "Raw Data" "Means" "Prcntyle"  /format "f12.6".

do if   (kind = 2).
print / space = 1.
print /title="Warning: Parallel analyses of adjusted correlation matrices".
print /title="eg, with SMCs on the diagonal, tend to indicate more factors".
print /title="than warranted (Buja, A., & Eyuboglu, N., 1992, Remarks on parallel".
print /title="analysis. Multivariate Behavioral Research, 27, 509-540.).".
print /title="The eigenvalues for trivial, negligible factors in the real".
print /title="data commonly surpass corresponding random data eigenvalues".
print /title="for the same roots. The eigenvalues from parallel analyses".
print /title="can be used to determine the real data eigenvalues that are".
print /title="beyond chance, but additional procedures should then be used".
print /title="to trim trivial factors.".
print / space = 2.
print /title="Principal components eigenvalues are often used to determine".
print /title="the number of common factors. This is the default in most".
print /title="statistical software packages, and it is the primary practice".
print /title="in the literature. It is also the method used by many factor".
print /title="analysis experts, including Cattell, who often examined".
print /title="principal components eigenvalues in his scree plots to determine".
print /title="the number of common factors. But others believe this common".
print /title="practice is wrong. Principal components eigenvalues are based".
print /title="on all of the variance in correlation matrices, including both".
print /title="the variance that is shared among variables and the variances".
print /title="that are unique to the variables. In contrast, principal".
print /title="axis eigenvalues are based solely on the shared variance".
print /title="among the variables. The two procedures are qualitatively".
print /title="different. Some therefore claim that the eigenvalues from one".
print /title="extraction method should not be used to determine".
print /title="the number of factors for the other extraction method.".
print /title="The issue remains neglected and unsettled.".
end if.

compute root      = results(:,1).
compute rawdata = results(:,2).
compute percntyl = results(:,4).

save results /outfile= 'C:\Users\dea_anto\Documents\Multivariate analysis\Lab 6\screedata.sav' / var=root rawdata means percntyl .

end matrix.

* plots the eigenvalues, by root, for the real/raw data and for the random data.
GET file= 'C:\Users\dea_anto\Documents\Multivariate analysis\Lab 6\screedata.sav'.
TSPLOT VARIABLES= rawdata means percntyl /ID= root /NOLOG.

* Parallel Analysis Program For Raw Data and Data Permutations.

* To run this program you need to first specify the data
  for analysis and then RUN, all at once, the commands
  from the MATRIX statement to the END MATRIX statement.

* This program conducts parallel analyses on data files in which
  the rows of the data matrix are cases/individuals and the
  columns are variables;  Data are read/entered into the program
  using the GET command (see the GET command below);  The GET 
  command reads an SPSS data file, which can be either the 
  current, active SPSS data file or a previously saved data file;
  A valid filename/location must be specified on the GET command;
  A subset of variables for the analyses can be specified by using
  the "/ VAR =" subcommand with the GET statement;  There can be
  no missing values.

* You must also specify:
  -- the # of parallel data sets for the analyses;
  -- the desired percentile of the distribution and random
     data eigenvalues;
  -- whether principal components analyses or principal axis/common
     factor analysis are to be conducted, and
  -- whether normally distributed random data generation or 
     permutations of the raw data set are to be used in the
     parallel analyses.

* Permutations of the raw data set can be time consuming;
  Each parallel data set is based on column-wise random shufflings
  of the values in the raw data matrix using Castellan's (1992, 
  BRMIC, 24, 72-77) algorithm; The distributions of the original 
  raw variables are exactly preserved in the shuffled versions used
  in the parallel analyses; Permutations of the raw data set are
  thus highly accurate and most relevant, especially in cases where
  the raw data are not normally distributed or when they do not meet
  the assumption of multivariate normality (see Longman & Holden,
  1992, BRMIC, 24, 493, for a Fortran version); If you would
  like to go this route, it is perhaps best to (1) first run a 
  normally distributed random data generation parallel analysis to
  familiarize yourself with the program and to get a ballpark
  reference point for the number of factors/components;
  (2) then run a permutations of the raw data parallel analysis
  using a small number of datasets (e.g., 100), just to see how long
  the program takes to run; then (3) run a permutations of the raw
  data parallel analysis using the number of parallel data sets that
  you would like use for your final analyses; 1000 datasets are 
  usually sufficient, although more datasets should be used if
  there are close calls.


* These next commands generate artificial raw data 
  (500 cases) that can be used for a trial-run of
  the program, instead of using your own raw data; 
  Just select and run this whole file; However, make sure to
  delete the artificial data commands before attempting to
  run your own data.

set mxloops=9000 printback=off width=80  seed = 1953125.
matrix.

* Enter the name/location of the data file for analyses after "FILE =";
  If you specify "FILE = *", then the program will read the current,
  active SPSS data file; Alternatively, enter the name/location
  of a previously saved SPSS data file instead of "*";
  you can use the "/ VAR =" subcommand after "/ missing=omit"
  subcommand to select variables for the analyses.
GET raw / FILE = * / missing=omit / VAR = ar1 to ar28.

* Enter the desired number of parallel data sets here.
compute ndatsets = 1000.

* Enter the desired percentile here.
compute percent  = 95.

* Enter either
  1 for principal components analysis, or
  2 for principal axis/common factor analysis.
compute kind = 2 .

* Enter either
  1 for normally distributed random data generation parallel analysis, or
  2 for permutations of the raw data set.
compute randtype = 1.


****************** End of user specifications. ******************

compute ncases   = nrow(raw). 
compute nvars    = ncol(raw).

* principal components analysis & random normal data generation.
do if (kind = 1 and randtype = 1).
compute nm1 = 1 / (ncases-1).
compute vcv = nm1 * (sscp(raw) - ((t(csum(raw))*csum(raw))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute realeval = eval(d * vcv * d).
compute evals = make(nvars,ndatsets,-9999).
loop #nds = 1 to ndatsets.
compute x = sqrt(2 * (ln(uniform(ncases,nvars)) * -1) ) &*
            cos(6.283185 * uniform(ncases,nvars) ).
compute vcv = nm1 * (sscp(x) - ((t(csum(x))*csum(x))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute evals(:,#nds) = eval(d * vcv * d).
end loop.
end if.

* principal components analysis & raw data permutation.
do if (kind = 1 and randtype = 2).
compute nm1 = 1 / (ncases-1).
compute vcv = nm1 * (sscp(raw) - ((t(csum(raw))*csum(raw))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute realeval = eval(d * vcv * d).
compute evals = make(nvars,ndatsets,-9999).
loop #nds = 1 to ndatsets.
compute x = raw.
loop #c = 1 to nvars.
loop #r = 1 to (ncases -1).
compute k = trunc( (ncases - #r + 1) * uniform(1,1) + 1 )  + #r - 1.
compute d = x(#r,#c).
compute x(#r,#c) = x(k,#c).
compute x(k,#c) = d.
end loop.
end loop.
compute vcv = nm1 * (sscp(x) - ((t(csum(x))*csum(x))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute evals(:,#nds) = eval(d * vcv * d).
end loop.
end if.

* PAF/common factor analysis & random normal data generation.
do if (kind = 2 and randtype = 1).
compute nm1 = 1 / (ncases-1).
compute vcv = nm1 * (sscp(raw) - ((t(csum(raw))*csum(raw))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute cr = (d * vcv * d).
compute smc = 1 - (1 &/ diag(inv(cr)) ).
call setdiag(cr,smc).
compute realeval = eval(cr).
compute evals = make(nvars,ndatsets,-9999).
compute nm1 = 1 / (ncases-1).
loop #nds = 1 to ndatsets.
compute x = sqrt(2 * (ln(uniform(ncases,nvars)) * -1) ) &*
            cos(6.283185 * uniform(ncases,nvars) ).
compute vcv = nm1 * (sscp(x) - ((t(csum(x))*csum(x))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute r = d * vcv * d.
compute smc = 1 - (1 &/ diag(inv(r)) ).
call setdiag(r,smc).
compute evals(:,#nds) = eval(r).
end loop.
end if.

* PAF/common factor analysis & raw data permutation.
do if (kind = 2 and randtype = 2).
compute nm1 = 1 / (ncases-1).
compute vcv = nm1 * (sscp(raw) - ((t(csum(raw))*csum(raw))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute cr = (d * vcv * d).
compute smc = 1 - (1 &/ diag(inv(cr)) ).
call setdiag(cr,smc).
compute realeval = eval(cr).
compute evals = make(nvars,ndatsets,-9999).
compute nm1 = 1 / (ncases-1).
loop #nds = 1 to ndatsets.
compute x = raw.
loop #c = 1 to nvars.
loop #r = 1 to (ncases -1).
compute k = trunc( (ncases - #r + 1) * uniform(1,1) + 1 )  + #r - 1.
compute d = x(#r,#c).
compute x(#r,#c) = x(k,#c).
compute x(k,#c) = d.
end loop.
end loop.
compute vcv = nm1 * (sscp(x) - ((t(csum(x))*csum(x))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute r = d * vcv * d.
compute smc = 1 - (1 &/ diag(inv(r)) ).
call setdiag(r,smc).
compute evals(:,#nds) = eval(r).
end loop.
end if.

* identifying the eigenvalues corresponding to the desired percentile.
compute num = rnd((percent*ndatsets)/100).
compute results = { t(1:nvars), realeval, t(1:nvars), t(1:nvars) }.
loop #root = 1 to nvars.
compute ranks = rnkorder(evals(#root,:)).
loop #col = 1 to ndatsets.
do if (ranks(1,#col) = num).
compute results(#root,4) = evals(#root,#col).
break.
end if.
end loop.
end loop.
compute results(:,3) = rsum(evals) / ndatsets.

print /title="PARALLEL ANALYSIS:".
do if (kind = 1 and randtype = 1).
print /title="Principal Components & Random Normal Data Generation".
else if (kind = 1 and randtype = 2).
print /title="Principal Components & Raw Data Permutation".
else if (kind = 2 and randtype = 1).
print /title="PAF/Common Factor Analysis & Random Normal Data Generation".
else if (kind = 2 and randtype = 2).
print /title="PAF/Common Factor Analysis & Raw Data Permutation".
end if.
compute specifs = {ncases; nvars; ndatsets; percent}.
print specifs /title="Specifications for this Run:"
 /rlabels="Ncases" "Nvars" "Ndatsets" "Percent".
print results 
 /title="Raw Data Eigenvalues, & Mean & Percentile Random Data Eigenvalues"
 /clabels="Root" "Raw Data" "Means" "Prcntyle"  /format "f12.6".

do if   (kind = 2).
print / space = 1.
print /title="Warning: Parallel analyses of adjusted correlation matrices".
print /title="eg, with SMCs on the diagonal, tend to indicate more factors".
print /title="than warranted (Buja, A., & Eyuboglu, N., 1992, Remarks on parallel".
print /title="analysis. Multivariate Behavioral Research, 27, 509-540.).".
print /title="The eigenvalues for trivial, negligible factors in the real".
print /title="data commonly surpass corresponding random data eigenvalues".
print /title="for the same roots. The eigenvalues from parallel analyses".
print /title="can be used to determine the real data eigenvalues that are".
print /title="beyond chance, but additional procedures should then be used".
print /title="to trim trivial factors.".
print / space = 2.
print /title="Principal components eigenvalues are often used to determine".
print /title="the number of common factors. This is the default in most".
print /title="statistical software packages, and it is the primary practice".
print /title="in the literature. It is also the method used by many factor".
print /title="analysis experts, including Cattell, who often examined".
print /title="principal components eigenvalues in his scree plots to determine".
print /title="the number of common factors. But others believe this common".
print /title="practice is wrong. Principal components eigenvalues are based".
print /title="on all of the variance in correlation matrices, including both".
print /title="the variance that is shared among variables and the variances".
print /title="that are unique to the variables. In contrast, principal".
print /title="axis eigenvalues are based solely on the shared variance".
print /title="among the variables. The two procedures are qualitatively".
print /title="different. Some therefore claim that the eigenvalues from one".
print /title="extraction method should not be used to determine".
print /title="the number of factors for the other extraction method.".
print /title="The issue remains neglected and unsettled.".
end if.

compute root      = results(:,1).
compute rawdata = results(:,2).
compute percntyl = results(:,4).

save results /outfile= 'C:\Users\dea_anto\Documents\Multivariate analysis\Lab 6\screedata.sav' / var=root rawdata means percntyl .

end matrix.

* plots the eigenvalues, by root, for the real/raw data and for the random data.
GET file= 'C:\Users\dea_anto\Documents\Multivariate analysis\Lab 6\screedata.sav'.
TSPLOT VARIABLES= rawdata means percntyl /ID= root /NOLOG.

FACTOR
  /VARIABLES ar2 ar4 ar5 ar6 ar7 ar8 ar10 ar11 ar12 ar13 ar15 ar17 ar18 ar19 ar20 ar21 ar23 ar24 
    ar26 ar27 ar28 ar1 ar3 ar9 ar14 ar16 ar22 ar25
  /MISSING LISTWISE 
  /ANALYSIS ar2 ar4 ar5 ar6 ar7 ar8 ar10 ar11 ar12 ar13 ar15 ar17 ar18 ar19 ar20 ar21 ar23 ar24 
    ar26 ar27 ar28 ar1 ar3 ar9 ar14 ar16 ar22 ar25
  /PRINT UNIVARIATE INITIAL EXTRACTION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(1) ITERATE(25)
  /EXTRACTION ML
  /ROTATION NOROTATE.

FACTOR
  /VARIABLES ar2 ar4 ar5 ar6 ar7 ar8 ar10 ar11 ar12 ar13 ar15 ar17 ar18 ar19 ar20 ar21 ar23 ar24 
    ar26 ar27 ar28 ar1 ar3 ar9 ar14 ar16 ar22 ar25
  /MISSING LISTWISE 
  /ANALYSIS ar2 ar4 ar5 ar6 ar7 ar8 ar10 ar11 ar12 ar13 ar15 ar17 ar18 ar19 ar20 ar21 ar23 ar24 
    ar26 ar27 ar28 ar1 ar3 ar9 ar14 ar16 ar22 ar25
  /PRINT UNIVARIATE INITIAL EXTRACTION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(2) ITERATE(25)
  /EXTRACTION ML
  /ROTATION NOROTATE.

FACTOR
  /VARIABLES ar2 ar4 ar5 ar6 ar7 ar8 ar10 ar11 ar12 ar13 ar15 ar17 ar18 ar19 ar20 ar21 ar23 ar24 
    ar26 ar27 ar28 ar1 ar3 ar9 ar14 ar16 ar22 ar25
  /MISSING LISTWISE 
  /ANALYSIS ar2 ar4 ar5 ar6 ar7 ar8 ar10 ar11 ar12 ar13 ar15 ar17 ar18 ar19 ar20 ar21 ar23 ar24 
    ar26 ar27 ar28 ar1 ar3 ar9 ar14 ar16 ar22 ar25
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(2) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION VARIMAX.

FACTOR
  /VARIABLES ar2 ar4 ar5 ar6 ar7 ar8 ar10 ar11 ar12 ar13 ar15 ar17 ar18 ar19 ar20 ar21 ar23 ar24 
    ar26 ar27 ar28 ar1 ar3 ar9 ar14 ar16 ar22 ar25
  /MISSING LISTWISE 
  /ANALYSIS ar2 ar4 ar5 ar6 ar7 ar8 ar10 ar11 ar12 ar13 ar15 ar17 ar18 ar19 ar20 ar21 ar23 ar24 
    ar26 ar27 ar28 ar1 ar3 ar9 ar14 ar16 ar22 ar25
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(2) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION PROMAX(4).

FACTOR
  /VARIABLES ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(3) ITERATE(25)
  /EXTRACTION ML
  /ROTATION NOROTATE.

FACTOR
  /VARIABLES ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(3) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION VARIMAX.

FACTOR
  /VARIABLES ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(3) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION PROMAX(4).

FACTOR
  /VARIABLES ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(4) ITERATE(25)
  /EXTRACTION ML
  /ROTATION NOROTATE.

FACTOR
  /VARIABLES ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(4) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION VARIMAX.

FACTOR
  /VARIABLES ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(4) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION PROMAX(4).

FACTOR
  /VARIABLES ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(5) ITERATE(25)
  /EXTRACTION ML
  /ROTATION NOROTATE.

FACTOR
  /VARIABLES ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(5) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION VARIMAX.

FACTOR
  /VARIABLES ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar19 
    ar20 ar21 ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(5) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION PROMAX(4).

*Ar19 and ar21 excluded

FACTOR
  /VARIABLES ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar20 
    ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar1 ar2 ar3 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar20 
    ar22 ar23 ar24 ar25 ar26 ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(4) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION VARIMAX.

*Ar3 excluded

FACTOR
  /VARIABLES ar1 ar2 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar20 ar22 
    ar23 ar24 ar25 ar26 ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar1 ar2 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar16 ar17 ar18 ar20 ar22 
    ar23 ar24 ar25 ar26 ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(4) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION VARIMAX.

*Ar16 excluded

FACTOR
  /VARIABLES ar1 ar2 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar17 ar18 ar20 ar22 ar23 
    ar24 ar25 ar26 ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar1 ar2 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar17 ar18 ar20 ar22 ar23 
    ar24 ar25 ar26 ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(4) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION VARIMAX.

*Ar22 excluded

FACTOR
  /VARIABLES ar1 ar2 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar17 ar18 ar20 ar23 ar24 
    ar25 ar26 ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar1 ar2 ar4 ar5 ar6 ar7 ar8 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar17 ar18 ar20 ar23 ar24 
    ar25 ar26 ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(4) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION VARIMAX.

*Ar8 excluded

FACTOR
  /VARIABLES ar1 ar2 ar4 ar5 ar6 ar7 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar17 ar18 ar20 ar23 ar24 
    ar25 ar26 ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar1 ar2 ar4 ar5 ar6 ar7 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar17 ar18 ar20 ar23 ar24 ar25 
    ar26 ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(4) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION VARIMAX.

*Ar1 excluded

FACTOR
  /VARIABLES ar2 ar4 ar5 ar6 ar7 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar17 ar18 ar20 ar23 ar24 ar25 
    ar26 ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar2 ar4 ar5 ar6 ar7 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar17 ar18 ar20 ar23 ar24 ar25 
    ar26 ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(4) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION VARIMAX.

*Ar25 excluded

FACTOR
  /VARIABLES ar2 ar4 ar5 ar6 ar7 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar17 ar18 ar20 ar23 ar24 ar26 
    ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar2 ar4 ar5 ar6 ar7 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar17 ar18 ar20 ar23 ar24 ar26 
    ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(4) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION VARIMAX.

FACTOR
  /VARIABLES ar2 ar4 ar5 ar6 ar7 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar17 ar18 ar20 ar23 ar24 ar26 
    ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar2 ar4 ar5 ar6 ar7 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar17 ar18 ar20 ar23 ar24 ar26 
    ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(4) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION VARIMAX
  /SAVE REG(ALL).

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT liberal
  /METHOD=ENTER FAC1_1 FAC2_1 FAC3_1 FAC4_1.

FACTOR
  /VARIABLES ar2 ar4 ar5 ar6 ar7 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar17 ar18 ar20 ar23 ar24 ar26 
    ar27 ar28
  /MISSING LISTWISE 
  /ANALYSIS ar2 ar4 ar5 ar6 ar7 ar9 ar10 ar11 ar12 ar13 ar14 ar15 ar17 ar18 ar20 ar23 ar24 ar26 
    ar27 ar28
  /PRINT UNIVARIATE INITIAL EXTRACTION ROTATION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(4) ITERATE(25)
  /EXTRACTION ML
  /CRITERIA ITERATE(25)
  /ROTATION PROMAX(4)
  /SAVE REG(ALL).

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT liberal
  /METHOD=ENTER FAC1_2 FAC2_2 FAC3_2 FAC4_2.
