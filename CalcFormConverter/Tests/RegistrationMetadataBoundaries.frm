* Test-only late registration and exact leaf/fallback parity driver.Skip;
Local cfcLateTest=0;
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcPGIndex?)=cfcShare(cfcPGIndex);
.sort
Hide cfcLateTest;
#$cfcPGOK=7;
#$cfcPGCost=11;
#$cfcPGDependent=13;
#$cfcPGFractional=17;
#call cfcPGRegisterResult(cfcLateTest)
#do cfcLateKey=cfcLateTest
#$cfcLateIndex=`cfcLateKey';
#inside $cfcLateIndex
id cfcShare(cfcPGIndex?)=cfcPGIndex;
#endinside
#$cfcLateValue=cfcMRMeta(`$cfcLateIndex');
#write "LATE Result-0 cfcMRMeta %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGTerms(`$cfcLateIndex');
#write "LATE Result-0 cfcPGTerms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGAtoms(`$cfcLateIndex');
#write "LATE Result-0 cfcPGAtoms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGReach(`$cfcLateIndex');
#write "LATE Result-0 cfcPGReach %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGSize(`$cfcLateIndex');
#write "LATE Result-0 cfcPGSize %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGDenOK(`$cfcLateIndex');
#write "LATE Result-0 cfcPGDenOK %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGParts(`$cfcLateIndex');
#write "LATE Result-0 cfcPGParts %$ %$",$cfcLateIndex,$cfcLateValue
#enddo
* A repeat must use complete registration, without rebuilding metadata.
#$cfcLateBefore=$cfcLateFlushes;
#call cfcPGRegisterResult(cfcLateTest)
#if `$cfcLateBefore' != `$cfcLateFlushes'
#message FAIL repeated registration rebuilt metadata
#terminate 1
#endif
#do cfcLeakSlot=1,32
#ifdef `cfcMBId`cfcLeakSlot''
#message FAIL leaked slot
#terminate 1
#endif
#enddo
#ifdef `cfcMBCount'
#message FAIL leaked counter
#terminate 1
#endif
Unhide cfcLateTest;
Drop cfcLateTest;
.sort
#write "LATECASE Result-0 PASS"
Skip;
Local cfcLateTest=0;
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcPGIndex?)=cfcShare(cfcPGIndex);
.sort
Hide cfcLateTest;
#$cfcPGOK=7;
#$cfcPGCost=11;
#$cfcPGDependent=13;
#$cfcPGFractional=17;
#call cfcPGRegisterScalars(cfcLateTest)
#if (`$cfcPGOK' != 7) || (`$cfcPGCost' != 11) || (`$cfcPGDependent' != 13) || (`$cfcPGFractional' != 17)
#message FAIL caller state
#terminate 1
#endif
#do cfcLateKey=cfcLateTest
#$cfcLateIndex=`cfcLateKey';
#inside $cfcLateIndex
id cfcShare(cfcPGIndex?)=cfcPGIndex;
#endinside
#$cfcLateValue=cfcMRMeta(`$cfcLateIndex');
#write "LATE Scalars-0 cfcMRMeta %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGTerms(`$cfcLateIndex');
#write "LATE Scalars-0 cfcPGTerms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGAtoms(`$cfcLateIndex');
#write "LATE Scalars-0 cfcPGAtoms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGReach(`$cfcLateIndex');
#write "LATE Scalars-0 cfcPGReach %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGSize(`$cfcLateIndex');
#write "LATE Scalars-0 cfcPGSize %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGDenOK(`$cfcLateIndex');
#write "LATE Scalars-0 cfcPGDenOK %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGParts(`$cfcLateIndex');
#write "LATE Scalars-0 cfcPGParts %$ %$",$cfcLateIndex,$cfcLateValue
#enddo
* A repeat must use complete registration, without rebuilding metadata.
#$cfcLateBefore=$cfcLateFlushes;
#call cfcPGRegisterScalars(cfcLateTest)
#if `$cfcLateBefore' != `$cfcLateFlushes'
#message FAIL repeated registration rebuilt metadata
#terminate 1
#endif
#do cfcLeakSlot=1,32
#ifdef `cfcMBId`cfcLeakSlot''
#message FAIL leaked slot
#terminate 1
#endif
#enddo
#ifdef `cfcMBCount'
#message FAIL leaked counter
#terminate 1
#endif
Unhide cfcLateTest;
Drop cfcLateTest;
.sort
#write "LATECASE Scalars-0 PASS"
Skip;
Local cfcLateTest=cfcPGCollect(cfs2+2000*cfs3+cfs4^2);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcPGIndex?)=cfcShare(cfcPGIndex);
.sort
Hide cfcLateTest;
#$cfcPGOK=7;
#$cfcPGCost=11;
#$cfcPGDependent=13;
#$cfcPGFractional=17;
#call cfcPGRegisterResult(cfcLateTest)
#do cfcLateKey=cfcLateTest
#$cfcLateIndex=`cfcLateKey';
#inside $cfcLateIndex
id cfcShare(cfcPGIndex?)=cfcPGIndex;
#endinside
#$cfcLateValue=cfcMRMeta(`$cfcLateIndex');
#write "LATE Result-1 cfcMRMeta %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGTerms(`$cfcLateIndex');
#write "LATE Result-1 cfcPGTerms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGAtoms(`$cfcLateIndex');
#write "LATE Result-1 cfcPGAtoms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGReach(`$cfcLateIndex');
#write "LATE Result-1 cfcPGReach %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGSize(`$cfcLateIndex');
#write "LATE Result-1 cfcPGSize %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGDenOK(`$cfcLateIndex');
#write "LATE Result-1 cfcPGDenOK %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGParts(`$cfcLateIndex');
#write "LATE Result-1 cfcPGParts %$ %$",$cfcLateIndex,$cfcLateValue
#enddo
* A repeat must use complete registration, without rebuilding metadata.
#$cfcLateBefore=$cfcLateFlushes;
#call cfcPGRegisterResult(cfcLateTest)
#if `$cfcLateBefore' != `$cfcLateFlushes'
#message FAIL repeated registration rebuilt metadata
#terminate 1
#endif
#do cfcLeakSlot=1,32
#ifdef `cfcMBId`cfcLeakSlot''
#message FAIL leaked slot
#terminate 1
#endif
#enddo
#ifdef `cfcMBCount'
#message FAIL leaked counter
#terminate 1
#endif
Unhide cfcLateTest;
Drop cfcLateTest;
.sort
#write "LATECASE Result-1 PASS"
Skip;
Local cfcLateTest=cfcPGCollect(cfs2+2001*cfs3+cfs4^2);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcPGIndex?)=cfcShare(cfcPGIndex);
.sort
Hide cfcLateTest;
#$cfcPGOK=7;
#$cfcPGCost=11;
#$cfcPGDependent=13;
#$cfcPGFractional=17;
#call cfcPGRegisterScalars(cfcLateTest)
#if (`$cfcPGOK' != 7) || (`$cfcPGCost' != 11) || (`$cfcPGDependent' != 13) || (`$cfcPGFractional' != 17)
#message FAIL caller state
#terminate 1
#endif
#do cfcLateKey=cfcLateTest
#$cfcLateIndex=`cfcLateKey';
#inside $cfcLateIndex
id cfcShare(cfcPGIndex?)=cfcPGIndex;
#endinside
#$cfcLateValue=cfcMRMeta(`$cfcLateIndex');
#write "LATE Scalars-1 cfcMRMeta %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGTerms(`$cfcLateIndex');
#write "LATE Scalars-1 cfcPGTerms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGAtoms(`$cfcLateIndex');
#write "LATE Scalars-1 cfcPGAtoms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGReach(`$cfcLateIndex');
#write "LATE Scalars-1 cfcPGReach %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGSize(`$cfcLateIndex');
#write "LATE Scalars-1 cfcPGSize %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGDenOK(`$cfcLateIndex');
#write "LATE Scalars-1 cfcPGDenOK %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGParts(`$cfcLateIndex');
#write "LATE Scalars-1 cfcPGParts %$ %$",$cfcLateIndex,$cfcLateValue
#enddo
* A repeat must use complete registration, without rebuilding metadata.
#$cfcLateBefore=$cfcLateFlushes;
#call cfcPGRegisterScalars(cfcLateTest)
#if `$cfcLateBefore' != `$cfcLateFlushes'
#message FAIL repeated registration rebuilt metadata
#terminate 1
#endif
#do cfcLeakSlot=1,32
#ifdef `cfcMBId`cfcLeakSlot''
#message FAIL leaked slot
#terminate 1
#endif
#enddo
#ifdef `cfcMBCount'
#message FAIL leaked counter
#terminate 1
#endif
Unhide cfcLateTest;
Drop cfcLateTest;
.sort
#write "LATECASE Scalars-1 PASS"
Skip;
Local cfcLateTest=cfcPGCollect(cfs2+3000*cfs3+cfs4^2)+cfcPGCollect(cfs2+3010*cfs3+cfs4^2)+cfcPGCollect(cfs2+3020*cfs3+cfs4^2)+cfcPGCollect(cfs2+3030*cfs3+cfs4^2)+cfcPGCollect(cfs2+3040*cfs3+cfs4^2)+cfcPGCollect(cfs2+3050*cfs3+cfs4^2)+cfcPGCollect(cfs2+3060*cfs3+cfs4^2)+cfcPGCollect(cfs2+3070*cfs3+cfs4^2)+cfcPGCollect(cfs2+3080*cfs3+cfs4^2)+cfcPGCollect(cfs2+3090*cfs3+cfs4^2)+cfcPGCollect(cfs2+3100*cfs3+cfs4^2)+cfcPGCollect(cfs2+3110*cfs3+cfs4^2)+cfcPGCollect(cfs2+3120*cfs3+cfs4^2)+cfcPGCollect(cfs2+3130*cfs3+cfs4^2)+cfcPGCollect(cfs2+3140*cfs3+cfs4^2)+cfcPGCollect(cfs2+3150*cfs3+cfs4^2)+cfcPGCollect(cfs2+3160*cfs3+cfs4^2)+cfcPGCollect(cfs2+3170*cfs3+cfs4^2)+cfcPGCollect(cfs2+3180*cfs3+cfs4^2)+cfcPGCollect(cfs2+3190*cfs3+cfs4^2)+cfcPGCollect(cfs2+3200*cfs3+cfs4^2)+cfcPGCollect(cfs2+3210*cfs3+cfs4^2)+cfcPGCollect(cfs2+3220*cfs3+cfs4^2)+cfcPGCollect(cfs2+3230*cfs3+cfs4^2)+cfcPGCollect(cfs2+3240*cfs3+cfs4^2)+cfcPGCollect(cfs2+3250*cfs3+cfs4^2)+cfcPGCollect(cfs2+3260*cfs3+cfs4^2)+cfcPGCollect(cfs2+3270*cfs3+cfs4^2)+cfcPGCollect(cfs2+3280*cfs3+cfs4^2)+cfcPGCollect(cfs2+3290*cfs3+cfs4^2)+cfcPGCollect(cfs2+3300*cfs3+cfs4^2);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcPGIndex?)=cfcShare(cfcPGIndex);
.sort
Hide cfcLateTest;
#$cfcPGOK=7;
#$cfcPGCost=11;
#$cfcPGDependent=13;
#$cfcPGFractional=17;
#call cfcPGRegisterResult(cfcLateTest)
#do cfcLateKey=cfcLateTest
#$cfcLateIndex=`cfcLateKey';
#inside $cfcLateIndex
id cfcShare(cfcPGIndex?)=cfcPGIndex;
#endinside
#$cfcLateValue=cfcMRMeta(`$cfcLateIndex');
#write "LATE Result-31 cfcMRMeta %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGTerms(`$cfcLateIndex');
#write "LATE Result-31 cfcPGTerms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGAtoms(`$cfcLateIndex');
#write "LATE Result-31 cfcPGAtoms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGReach(`$cfcLateIndex');
#write "LATE Result-31 cfcPGReach %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGSize(`$cfcLateIndex');
#write "LATE Result-31 cfcPGSize %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGDenOK(`$cfcLateIndex');
#write "LATE Result-31 cfcPGDenOK %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGParts(`$cfcLateIndex');
#write "LATE Result-31 cfcPGParts %$ %$",$cfcLateIndex,$cfcLateValue
#enddo
* A repeat must use complete registration, without rebuilding metadata.
#$cfcLateBefore=$cfcLateFlushes;
#call cfcPGRegisterResult(cfcLateTest)
#if `$cfcLateBefore' != `$cfcLateFlushes'
#message FAIL repeated registration rebuilt metadata
#terminate 1
#endif
#do cfcLeakSlot=1,32
#ifdef `cfcMBId`cfcLeakSlot''
#message FAIL leaked slot
#terminate 1
#endif
#enddo
#ifdef `cfcMBCount'
#message FAIL leaked counter
#terminate 1
#endif
Unhide cfcLateTest;
Drop cfcLateTest;
.sort
#write "LATECASE Result-31 PASS"
Skip;
Local cfcLateTest=cfcPGCollect(cfs2+3001*cfs3+cfs4^2)+cfcPGCollect(cfs2+3011*cfs3+cfs4^2)+cfcPGCollect(cfs2+3021*cfs3+cfs4^2)+cfcPGCollect(cfs2+3031*cfs3+cfs4^2)+cfcPGCollect(cfs2+3041*cfs3+cfs4^2)+cfcPGCollect(cfs2+3051*cfs3+cfs4^2)+cfcPGCollect(cfs2+3061*cfs3+cfs4^2)+cfcPGCollect(cfs2+3071*cfs3+cfs4^2)+cfcPGCollect(cfs2+3081*cfs3+cfs4^2)+cfcPGCollect(cfs2+3091*cfs3+cfs4^2)+cfcPGCollect(cfs2+3101*cfs3+cfs4^2)+cfcPGCollect(cfs2+3111*cfs3+cfs4^2)+cfcPGCollect(cfs2+3121*cfs3+cfs4^2)+cfcPGCollect(cfs2+3131*cfs3+cfs4^2)+cfcPGCollect(cfs2+3141*cfs3+cfs4^2)+cfcPGCollect(cfs2+3151*cfs3+cfs4^2)+cfcPGCollect(cfs2+3161*cfs3+cfs4^2)+cfcPGCollect(cfs2+3171*cfs3+cfs4^2)+cfcPGCollect(cfs2+3181*cfs3+cfs4^2)+cfcPGCollect(cfs2+3191*cfs3+cfs4^2)+cfcPGCollect(cfs2+3201*cfs3+cfs4^2)+cfcPGCollect(cfs2+3211*cfs3+cfs4^2)+cfcPGCollect(cfs2+3221*cfs3+cfs4^2)+cfcPGCollect(cfs2+3231*cfs3+cfs4^2)+cfcPGCollect(cfs2+3241*cfs3+cfs4^2)+cfcPGCollect(cfs2+3251*cfs3+cfs4^2)+cfcPGCollect(cfs2+3261*cfs3+cfs4^2)+cfcPGCollect(cfs2+3271*cfs3+cfs4^2)+cfcPGCollect(cfs2+3281*cfs3+cfs4^2)+cfcPGCollect(cfs2+3291*cfs3+cfs4^2)+cfcPGCollect(cfs2+3301*cfs3+cfs4^2);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcPGIndex?)=cfcShare(cfcPGIndex);
.sort
Hide cfcLateTest;
#$cfcPGOK=7;
#$cfcPGCost=11;
#$cfcPGDependent=13;
#$cfcPGFractional=17;
#call cfcPGRegisterScalars(cfcLateTest)
#if (`$cfcPGOK' != 7) || (`$cfcPGCost' != 11) || (`$cfcPGDependent' != 13) || (`$cfcPGFractional' != 17)
#message FAIL caller state
#terminate 1
#endif
#do cfcLateKey=cfcLateTest
#$cfcLateIndex=`cfcLateKey';
#inside $cfcLateIndex
id cfcShare(cfcPGIndex?)=cfcPGIndex;
#endinside
#$cfcLateValue=cfcMRMeta(`$cfcLateIndex');
#write "LATE Scalars-31 cfcMRMeta %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGTerms(`$cfcLateIndex');
#write "LATE Scalars-31 cfcPGTerms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGAtoms(`$cfcLateIndex');
#write "LATE Scalars-31 cfcPGAtoms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGReach(`$cfcLateIndex');
#write "LATE Scalars-31 cfcPGReach %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGSize(`$cfcLateIndex');
#write "LATE Scalars-31 cfcPGSize %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGDenOK(`$cfcLateIndex');
#write "LATE Scalars-31 cfcPGDenOK %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGParts(`$cfcLateIndex');
#write "LATE Scalars-31 cfcPGParts %$ %$",$cfcLateIndex,$cfcLateValue
#enddo
* A repeat must use complete registration, without rebuilding metadata.
#$cfcLateBefore=$cfcLateFlushes;
#call cfcPGRegisterScalars(cfcLateTest)
#if `$cfcLateBefore' != `$cfcLateFlushes'
#message FAIL repeated registration rebuilt metadata
#terminate 1
#endif
#do cfcLeakSlot=1,32
#ifdef `cfcMBId`cfcLeakSlot''
#message FAIL leaked slot
#terminate 1
#endif
#enddo
#ifdef `cfcMBCount'
#message FAIL leaked counter
#terminate 1
#endif
Unhide cfcLateTest;
Drop cfcLateTest;
.sort
#write "LATECASE Scalars-31 PASS"
Skip;
Local cfcLateTest=cfcPGCollect(cfs2+4000*cfs3+cfs4^2)+cfcPGCollect(cfs2+4010*cfs3+cfs4^2)+cfcPGCollect(cfs2+4020*cfs3+cfs4^2)+cfcPGCollect(cfs2+4030*cfs3+cfs4^2)+cfcPGCollect(cfs2+4040*cfs3+cfs4^2)+cfcPGCollect(cfs2+4050*cfs3+cfs4^2)+cfcPGCollect(cfs2+4060*cfs3+cfs4^2)+cfcPGCollect(cfs2+4070*cfs3+cfs4^2)+cfcPGCollect(cfs2+4080*cfs3+cfs4^2)+cfcPGCollect(cfs2+4090*cfs3+cfs4^2)+cfcPGCollect(cfs2+4100*cfs3+cfs4^2)+cfcPGCollect(cfs2+4110*cfs3+cfs4^2)+cfcPGCollect(cfs2+4120*cfs3+cfs4^2)+cfcPGCollect(cfs2+4130*cfs3+cfs4^2)+cfcPGCollect(cfs2+4140*cfs3+cfs4^2)+cfcPGCollect(cfs2+4150*cfs3+cfs4^2)+cfcPGCollect(cfs2+4160*cfs3+cfs4^2)+cfcPGCollect(cfs2+4170*cfs3+cfs4^2)+cfcPGCollect(cfs2+4180*cfs3+cfs4^2)+cfcPGCollect(cfs2+4190*cfs3+cfs4^2)+cfcPGCollect(cfs2+4200*cfs3+cfs4^2)+cfcPGCollect(cfs2+4210*cfs3+cfs4^2)+cfcPGCollect(cfs2+4220*cfs3+cfs4^2)+cfcPGCollect(cfs2+4230*cfs3+cfs4^2)+cfcPGCollect(cfs2+4240*cfs3+cfs4^2)+cfcPGCollect(cfs2+4250*cfs3+cfs4^2)+cfcPGCollect(cfs2+4260*cfs3+cfs4^2)+cfcPGCollect(cfs2+4270*cfs3+cfs4^2)+cfcPGCollect(cfs2+4280*cfs3+cfs4^2)+cfcPGCollect(cfs2+4290*cfs3+cfs4^2)+cfcPGCollect(cfs2+4300*cfs3+cfs4^2)+cfcPGCollect(cfs2+4310*cfs3+cfs4^2);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcPGIndex?)=cfcShare(cfcPGIndex);
.sort
Hide cfcLateTest;
#$cfcPGOK=7;
#$cfcPGCost=11;
#$cfcPGDependent=13;
#$cfcPGFractional=17;
#call cfcPGRegisterResult(cfcLateTest)
#do cfcLateKey=cfcLateTest
#$cfcLateIndex=`cfcLateKey';
#inside $cfcLateIndex
id cfcShare(cfcPGIndex?)=cfcPGIndex;
#endinside
#$cfcLateValue=cfcMRMeta(`$cfcLateIndex');
#write "LATE Result-32 cfcMRMeta %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGTerms(`$cfcLateIndex');
#write "LATE Result-32 cfcPGTerms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGAtoms(`$cfcLateIndex');
#write "LATE Result-32 cfcPGAtoms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGReach(`$cfcLateIndex');
#write "LATE Result-32 cfcPGReach %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGSize(`$cfcLateIndex');
#write "LATE Result-32 cfcPGSize %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGDenOK(`$cfcLateIndex');
#write "LATE Result-32 cfcPGDenOK %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGParts(`$cfcLateIndex');
#write "LATE Result-32 cfcPGParts %$ %$",$cfcLateIndex,$cfcLateValue
#enddo
* A repeat must use complete registration, without rebuilding metadata.
#$cfcLateBefore=$cfcLateFlushes;
#call cfcPGRegisterResult(cfcLateTest)
#if `$cfcLateBefore' != `$cfcLateFlushes'
#message FAIL repeated registration rebuilt metadata
#terminate 1
#endif
#do cfcLeakSlot=1,32
#ifdef `cfcMBId`cfcLeakSlot''
#message FAIL leaked slot
#terminate 1
#endif
#enddo
#ifdef `cfcMBCount'
#message FAIL leaked counter
#terminate 1
#endif
Unhide cfcLateTest;
Drop cfcLateTest;
.sort
#write "LATECASE Result-32 PASS"
Skip;
Local cfcLateTest=cfcPGCollect(cfs2+4001*cfs3+cfs4^2)+cfcPGCollect(cfs2+4011*cfs3+cfs4^2)+cfcPGCollect(cfs2+4021*cfs3+cfs4^2)+cfcPGCollect(cfs2+4031*cfs3+cfs4^2)+cfcPGCollect(cfs2+4041*cfs3+cfs4^2)+cfcPGCollect(cfs2+4051*cfs3+cfs4^2)+cfcPGCollect(cfs2+4061*cfs3+cfs4^2)+cfcPGCollect(cfs2+4071*cfs3+cfs4^2)+cfcPGCollect(cfs2+4081*cfs3+cfs4^2)+cfcPGCollect(cfs2+4091*cfs3+cfs4^2)+cfcPGCollect(cfs2+4101*cfs3+cfs4^2)+cfcPGCollect(cfs2+4111*cfs3+cfs4^2)+cfcPGCollect(cfs2+4121*cfs3+cfs4^2)+cfcPGCollect(cfs2+4131*cfs3+cfs4^2)+cfcPGCollect(cfs2+4141*cfs3+cfs4^2)+cfcPGCollect(cfs2+4151*cfs3+cfs4^2)+cfcPGCollect(cfs2+4161*cfs3+cfs4^2)+cfcPGCollect(cfs2+4171*cfs3+cfs4^2)+cfcPGCollect(cfs2+4181*cfs3+cfs4^2)+cfcPGCollect(cfs2+4191*cfs3+cfs4^2)+cfcPGCollect(cfs2+4201*cfs3+cfs4^2)+cfcPGCollect(cfs2+4211*cfs3+cfs4^2)+cfcPGCollect(cfs2+4221*cfs3+cfs4^2)+cfcPGCollect(cfs2+4231*cfs3+cfs4^2)+cfcPGCollect(cfs2+4241*cfs3+cfs4^2)+cfcPGCollect(cfs2+4251*cfs3+cfs4^2)+cfcPGCollect(cfs2+4261*cfs3+cfs4^2)+cfcPGCollect(cfs2+4271*cfs3+cfs4^2)+cfcPGCollect(cfs2+4281*cfs3+cfs4^2)+cfcPGCollect(cfs2+4291*cfs3+cfs4^2)+cfcPGCollect(cfs2+4301*cfs3+cfs4^2)+cfcPGCollect(cfs2+4311*cfs3+cfs4^2);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcPGIndex?)=cfcShare(cfcPGIndex);
.sort
Hide cfcLateTest;
#$cfcPGOK=7;
#$cfcPGCost=11;
#$cfcPGDependent=13;
#$cfcPGFractional=17;
#call cfcPGRegisterScalars(cfcLateTest)
#if (`$cfcPGOK' != 7) || (`$cfcPGCost' != 11) || (`$cfcPGDependent' != 13) || (`$cfcPGFractional' != 17)
#message FAIL caller state
#terminate 1
#endif
#do cfcLateKey=cfcLateTest
#$cfcLateIndex=`cfcLateKey';
#inside $cfcLateIndex
id cfcShare(cfcPGIndex?)=cfcPGIndex;
#endinside
#$cfcLateValue=cfcMRMeta(`$cfcLateIndex');
#write "LATE Scalars-32 cfcMRMeta %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGTerms(`$cfcLateIndex');
#write "LATE Scalars-32 cfcPGTerms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGAtoms(`$cfcLateIndex');
#write "LATE Scalars-32 cfcPGAtoms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGReach(`$cfcLateIndex');
#write "LATE Scalars-32 cfcPGReach %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGSize(`$cfcLateIndex');
#write "LATE Scalars-32 cfcPGSize %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGDenOK(`$cfcLateIndex');
#write "LATE Scalars-32 cfcPGDenOK %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGParts(`$cfcLateIndex');
#write "LATE Scalars-32 cfcPGParts %$ %$",$cfcLateIndex,$cfcLateValue
#enddo
* A repeat must use complete registration, without rebuilding metadata.
#$cfcLateBefore=$cfcLateFlushes;
#call cfcPGRegisterScalars(cfcLateTest)
#if `$cfcLateBefore' != `$cfcLateFlushes'
#message FAIL repeated registration rebuilt metadata
#terminate 1
#endif
#do cfcLeakSlot=1,32
#ifdef `cfcMBId`cfcLeakSlot''
#message FAIL leaked slot
#terminate 1
#endif
#enddo
#ifdef `cfcMBCount'
#message FAIL leaked counter
#terminate 1
#endif
Unhide cfcLateTest;
Drop cfcLateTest;
.sort
#write "LATECASE Scalars-32 PASS"
Skip;
Local cfcLateTest=cfcPGCollect(cfs2+5000*cfs3+cfs4^2)+cfcPGCollect(cfs2+5010*cfs3+cfs4^2)+cfcPGCollect(cfs2+5020*cfs3+cfs4^2)+cfcPGCollect(cfs2+5030*cfs3+cfs4^2)+cfcPGCollect(cfs2+5040*cfs3+cfs4^2)+cfcPGCollect(cfs2+5050*cfs3+cfs4^2)+cfcPGCollect(cfs2+5060*cfs3+cfs4^2)+cfcPGCollect(cfs2+5070*cfs3+cfs4^2)+cfcPGCollect(cfs2+5080*cfs3+cfs4^2)+cfcPGCollect(cfs2+5090*cfs3+cfs4^2)+cfcPGCollect(cfs2+5100*cfs3+cfs4^2)+cfcPGCollect(cfs2+5110*cfs3+cfs4^2)+cfcPGCollect(cfs2+5120*cfs3+cfs4^2)+cfcPGCollect(cfs2+5130*cfs3+cfs4^2)+cfcPGCollect(cfs2+5140*cfs3+cfs4^2)+cfcPGCollect(cfs2+5150*cfs3+cfs4^2)+cfcPGCollect(cfs2+5160*cfs3+cfs4^2)+cfcPGCollect(cfs2+5170*cfs3+cfs4^2)+cfcPGCollect(cfs2+5180*cfs3+cfs4^2)+cfcPGCollect(cfs2+5190*cfs3+cfs4^2)+cfcPGCollect(cfs2+5200*cfs3+cfs4^2)+cfcPGCollect(cfs2+5210*cfs3+cfs4^2)+cfcPGCollect(cfs2+5220*cfs3+cfs4^2)+cfcPGCollect(cfs2+5230*cfs3+cfs4^2)+cfcPGCollect(cfs2+5240*cfs3+cfs4^2)+cfcPGCollect(cfs2+5250*cfs3+cfs4^2)+cfcPGCollect(cfs2+5260*cfs3+cfs4^2)+cfcPGCollect(cfs2+5270*cfs3+cfs4^2)+cfcPGCollect(cfs2+5280*cfs3+cfs4^2)+cfcPGCollect(cfs2+5290*cfs3+cfs4^2)+cfcPGCollect(cfs2+5300*cfs3+cfs4^2)+cfcPGCollect(cfs2+5310*cfs3+cfs4^2)+cfcPGCollect(cfs2+5320*cfs3+cfs4^2);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcPGIndex?)=cfcShare(cfcPGIndex);
.sort
Hide cfcLateTest;
#$cfcPGOK=7;
#$cfcPGCost=11;
#$cfcPGDependent=13;
#$cfcPGFractional=17;
#call cfcPGRegisterResult(cfcLateTest)
#do cfcLateKey=cfcLateTest
#$cfcLateIndex=`cfcLateKey';
#inside $cfcLateIndex
id cfcShare(cfcPGIndex?)=cfcPGIndex;
#endinside
#$cfcLateValue=cfcMRMeta(`$cfcLateIndex');
#write "LATE Result-33 cfcMRMeta %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGTerms(`$cfcLateIndex');
#write "LATE Result-33 cfcPGTerms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGAtoms(`$cfcLateIndex');
#write "LATE Result-33 cfcPGAtoms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGReach(`$cfcLateIndex');
#write "LATE Result-33 cfcPGReach %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGSize(`$cfcLateIndex');
#write "LATE Result-33 cfcPGSize %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGDenOK(`$cfcLateIndex');
#write "LATE Result-33 cfcPGDenOK %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGParts(`$cfcLateIndex');
#write "LATE Result-33 cfcPGParts %$ %$",$cfcLateIndex,$cfcLateValue
#enddo
* A repeat must use complete registration, without rebuilding metadata.
#$cfcLateBefore=$cfcLateFlushes;
#call cfcPGRegisterResult(cfcLateTest)
#if `$cfcLateBefore' != `$cfcLateFlushes'
#message FAIL repeated registration rebuilt metadata
#terminate 1
#endif
#do cfcLeakSlot=1,32
#ifdef `cfcMBId`cfcLeakSlot''
#message FAIL leaked slot
#terminate 1
#endif
#enddo
#ifdef `cfcMBCount'
#message FAIL leaked counter
#terminate 1
#endif
Unhide cfcLateTest;
Drop cfcLateTest;
.sort
#write "LATECASE Result-33 PASS"
Skip;
Local cfcLateTest=cfcPGCollect(cfs2+5001*cfs3+cfs4^2)+cfcPGCollect(cfs2+5011*cfs3+cfs4^2)+cfcPGCollect(cfs2+5021*cfs3+cfs4^2)+cfcPGCollect(cfs2+5031*cfs3+cfs4^2)+cfcPGCollect(cfs2+5041*cfs3+cfs4^2)+cfcPGCollect(cfs2+5051*cfs3+cfs4^2)+cfcPGCollect(cfs2+5061*cfs3+cfs4^2)+cfcPGCollect(cfs2+5071*cfs3+cfs4^2)+cfcPGCollect(cfs2+5081*cfs3+cfs4^2)+cfcPGCollect(cfs2+5091*cfs3+cfs4^2)+cfcPGCollect(cfs2+5101*cfs3+cfs4^2)+cfcPGCollect(cfs2+5111*cfs3+cfs4^2)+cfcPGCollect(cfs2+5121*cfs3+cfs4^2)+cfcPGCollect(cfs2+5131*cfs3+cfs4^2)+cfcPGCollect(cfs2+5141*cfs3+cfs4^2)+cfcPGCollect(cfs2+5151*cfs3+cfs4^2)+cfcPGCollect(cfs2+5161*cfs3+cfs4^2)+cfcPGCollect(cfs2+5171*cfs3+cfs4^2)+cfcPGCollect(cfs2+5181*cfs3+cfs4^2)+cfcPGCollect(cfs2+5191*cfs3+cfs4^2)+cfcPGCollect(cfs2+5201*cfs3+cfs4^2)+cfcPGCollect(cfs2+5211*cfs3+cfs4^2)+cfcPGCollect(cfs2+5221*cfs3+cfs4^2)+cfcPGCollect(cfs2+5231*cfs3+cfs4^2)+cfcPGCollect(cfs2+5241*cfs3+cfs4^2)+cfcPGCollect(cfs2+5251*cfs3+cfs4^2)+cfcPGCollect(cfs2+5261*cfs3+cfs4^2)+cfcPGCollect(cfs2+5271*cfs3+cfs4^2)+cfcPGCollect(cfs2+5281*cfs3+cfs4^2)+cfcPGCollect(cfs2+5291*cfs3+cfs4^2)+cfcPGCollect(cfs2+5301*cfs3+cfs4^2)+cfcPGCollect(cfs2+5311*cfs3+cfs4^2)+cfcPGCollect(cfs2+5321*cfs3+cfs4^2);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcPGIndex?)=cfcShare(cfcPGIndex);
.sort
Hide cfcLateTest;
#$cfcPGOK=7;
#$cfcPGCost=11;
#$cfcPGDependent=13;
#$cfcPGFractional=17;
#call cfcPGRegisterScalars(cfcLateTest)
#if (`$cfcPGOK' != 7) || (`$cfcPGCost' != 11) || (`$cfcPGDependent' != 13) || (`$cfcPGFractional' != 17)
#message FAIL caller state
#terminate 1
#endif
#do cfcLateKey=cfcLateTest
#$cfcLateIndex=`cfcLateKey';
#inside $cfcLateIndex
id cfcShare(cfcPGIndex?)=cfcPGIndex;
#endinside
#$cfcLateValue=cfcMRMeta(`$cfcLateIndex');
#write "LATE Scalars-33 cfcMRMeta %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGTerms(`$cfcLateIndex');
#write "LATE Scalars-33 cfcPGTerms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGAtoms(`$cfcLateIndex');
#write "LATE Scalars-33 cfcPGAtoms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGReach(`$cfcLateIndex');
#write "LATE Scalars-33 cfcPGReach %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGSize(`$cfcLateIndex');
#write "LATE Scalars-33 cfcPGSize %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGDenOK(`$cfcLateIndex');
#write "LATE Scalars-33 cfcPGDenOK %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGParts(`$cfcLateIndex');
#write "LATE Scalars-33 cfcPGParts %$ %$",$cfcLateIndex,$cfcLateValue
#enddo
* A repeat must use complete registration, without rebuilding metadata.
#$cfcLateBefore=$cfcLateFlushes;
#call cfcPGRegisterScalars(cfcLateTest)
#if `$cfcLateBefore' != `$cfcLateFlushes'
#message FAIL repeated registration rebuilt metadata
#terminate 1
#endif
#do cfcLeakSlot=1,32
#ifdef `cfcMBId`cfcLeakSlot''
#message FAIL leaked slot
#terminate 1
#endif
#enddo
#ifdef `cfcMBCount'
#message FAIL leaked counter
#terminate 1
#endif
Unhide cfcLateTest;
Drop cfcLateTest;
.sort
#write "LATECASE Scalars-33 PASS"
Skip;
Local cfcLateTest=cfcPGCollect(cfs2+6000*cfs3+cfs4^2)+cfcPGCollect(cfs2+6010*cfs3+cfs4^2)+cfcPGCollect(cfs2+6020*cfs3+cfs4^2)+cfcPGCollect(cfs2+6030*cfs3+cfs4^2)+cfcPGCollect(cfs2+6040*cfs3+cfs4^2)+cfcPGCollect(cfs2+6050*cfs3+cfs4^2)+cfcPGCollect(cfs2+6060*cfs3+cfs4^2)+cfcPGCollect(cfs2+6070*cfs3+cfs4^2)+cfcPGCollect(cfs2+6080*cfs3+cfs4^2)+cfcPGCollect(cfs2+6090*cfs3+cfs4^2)+cfcPGCollect(cfs2+6100*cfs3+cfs4^2)+cfcPGCollect(cfs2+6110*cfs3+cfs4^2)+cfcPGCollect(cfs2+6120*cfs3+cfs4^2)+cfcPGCollect(cfs2+6130*cfs3+cfs4^2)+cfcPGCollect(cfs2+6140*cfs3+cfs4^2)+cfcPGCollect(cfs2+6150*cfs3+cfs4^2)+cfcPGCollect(cfs2+6160*cfs3+cfs4^2)+cfcPGCollect(cfs2+6170*cfs3+cfs4^2)+cfcPGCollect(cfs2+6180*cfs3+cfs4^2)+cfcPGCollect(cfs2+6190*cfs3+cfs4^2)+cfcPGCollect(cfs2+6200*cfs3+cfs4^2)+cfcPGCollect(cfs2+6210*cfs3+cfs4^2)+cfcPGCollect(cfs2+6220*cfs3+cfs4^2)+cfcPGCollect(cfs2+6230*cfs3+cfs4^2)+cfcPGCollect(cfs2+6240*cfs3+cfs4^2)+cfcPGCollect(cfs2+6250*cfs3+cfs4^2)+cfcPGCollect(cfs2+6260*cfs3+cfs4^2)+cfcPGCollect(cfs2+6270*cfs3+cfs4^2)+cfcPGCollect(cfs2+6280*cfs3+cfs4^2)+cfcPGCollect(cfs2+6290*cfs3+cfs4^2)+cfcPGCollect(cfs2+6300*cfs3+cfs4^2)+cfcPGCollect(cfs2+6310*cfs3+cfs4^2)+cfcPGCollect(cfs2+6320*cfs3+cfs4^2)+cfcPGCollect(cfs2+6330*cfs3+cfs4^2)+cfcPGCollect(cfs2+6340*cfs3+cfs4^2)+cfcPGCollect(cfs2+6350*cfs3+cfs4^2)+cfcPGCollect(cfs2+6360*cfs3+cfs4^2)+cfcPGCollect(cfs2+6370*cfs3+cfs4^2)+cfcPGCollect(cfs2+6380*cfs3+cfs4^2)+cfcPGCollect(cfs2+6390*cfs3+cfs4^2)+cfcPGCollect(cfs2+6400*cfs3+cfs4^2)+cfcPGCollect(cfs2+6410*cfs3+cfs4^2)+cfcPGCollect(cfs2+6420*cfs3+cfs4^2)+cfcPGCollect(cfs2+6430*cfs3+cfs4^2)+cfcPGCollect(cfs2+6440*cfs3+cfs4^2)+cfcPGCollect(cfs2+6450*cfs3+cfs4^2)+cfcPGCollect(cfs2+6460*cfs3+cfs4^2)+cfcPGCollect(cfs2+6470*cfs3+cfs4^2)+cfcPGCollect(cfs2+6480*cfs3+cfs4^2)+cfcPGCollect(cfs2+6490*cfs3+cfs4^2)+cfcPGCollect(cfs2+6500*cfs3+cfs4^2)+cfcPGCollect(cfs2+6510*cfs3+cfs4^2)+cfcPGCollect(cfs2+6520*cfs3+cfs4^2)+cfcPGCollect(cfs2+6530*cfs3+cfs4^2)+cfcPGCollect(cfs2+6540*cfs3+cfs4^2)+cfcPGCollect(cfs2+6550*cfs3+cfs4^2)+cfcPGCollect(cfs2+6560*cfs3+cfs4^2)+cfcPGCollect(cfs2+6570*cfs3+cfs4^2)+cfcPGCollect(cfs2+6580*cfs3+cfs4^2)+cfcPGCollect(cfs2+6590*cfs3+cfs4^2)+cfcPGCollect(cfs2+6600*cfs3+cfs4^2)+cfcPGCollect(cfs2+6610*cfs3+cfs4^2)+cfcPGCollect(cfs2+6620*cfs3+cfs4^2)+cfcPGCollect(cfs2+6630*cfs3+cfs4^2)+cfcPGCollect(cfs2+6640*cfs3+cfs4^2);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcPGIndex?)=cfcShare(cfcPGIndex);
.sort
Hide cfcLateTest;
#$cfcPGOK=7;
#$cfcPGCost=11;
#$cfcPGDependent=13;
#$cfcPGFractional=17;
#call cfcPGRegisterResult(cfcLateTest)
#do cfcLateKey=cfcLateTest
#$cfcLateIndex=`cfcLateKey';
#inside $cfcLateIndex
id cfcShare(cfcPGIndex?)=cfcPGIndex;
#endinside
#$cfcLateValue=cfcMRMeta(`$cfcLateIndex');
#write "LATE Result-65 cfcMRMeta %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGTerms(`$cfcLateIndex');
#write "LATE Result-65 cfcPGTerms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGAtoms(`$cfcLateIndex');
#write "LATE Result-65 cfcPGAtoms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGReach(`$cfcLateIndex');
#write "LATE Result-65 cfcPGReach %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGSize(`$cfcLateIndex');
#write "LATE Result-65 cfcPGSize %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGDenOK(`$cfcLateIndex');
#write "LATE Result-65 cfcPGDenOK %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGParts(`$cfcLateIndex');
#write "LATE Result-65 cfcPGParts %$ %$",$cfcLateIndex,$cfcLateValue
#enddo
* A repeat must use complete registration, without rebuilding metadata.
#$cfcLateBefore=$cfcLateFlushes;
#call cfcPGRegisterResult(cfcLateTest)
#if `$cfcLateBefore' != `$cfcLateFlushes'
#message FAIL repeated registration rebuilt metadata
#terminate 1
#endif
#do cfcLeakSlot=1,32
#ifdef `cfcMBId`cfcLeakSlot''
#message FAIL leaked slot
#terminate 1
#endif
#enddo
#ifdef `cfcMBCount'
#message FAIL leaked counter
#terminate 1
#endif
Unhide cfcLateTest;
Drop cfcLateTest;
.sort
#write "LATECASE Result-65 PASS"
Skip;
Local cfcLateTest=cfcPGCollect(cfs2+6001*cfs3+cfs4^2)+cfcPGCollect(cfs2+6011*cfs3+cfs4^2)+cfcPGCollect(cfs2+6021*cfs3+cfs4^2)+cfcPGCollect(cfs2+6031*cfs3+cfs4^2)+cfcPGCollect(cfs2+6041*cfs3+cfs4^2)+cfcPGCollect(cfs2+6051*cfs3+cfs4^2)+cfcPGCollect(cfs2+6061*cfs3+cfs4^2)+cfcPGCollect(cfs2+6071*cfs3+cfs4^2)+cfcPGCollect(cfs2+6081*cfs3+cfs4^2)+cfcPGCollect(cfs2+6091*cfs3+cfs4^2)+cfcPGCollect(cfs2+6101*cfs3+cfs4^2)+cfcPGCollect(cfs2+6111*cfs3+cfs4^2)+cfcPGCollect(cfs2+6121*cfs3+cfs4^2)+cfcPGCollect(cfs2+6131*cfs3+cfs4^2)+cfcPGCollect(cfs2+6141*cfs3+cfs4^2)+cfcPGCollect(cfs2+6151*cfs3+cfs4^2)+cfcPGCollect(cfs2+6161*cfs3+cfs4^2)+cfcPGCollect(cfs2+6171*cfs3+cfs4^2)+cfcPGCollect(cfs2+6181*cfs3+cfs4^2)+cfcPGCollect(cfs2+6191*cfs3+cfs4^2)+cfcPGCollect(cfs2+6201*cfs3+cfs4^2)+cfcPGCollect(cfs2+6211*cfs3+cfs4^2)+cfcPGCollect(cfs2+6221*cfs3+cfs4^2)+cfcPGCollect(cfs2+6231*cfs3+cfs4^2)+cfcPGCollect(cfs2+6241*cfs3+cfs4^2)+cfcPGCollect(cfs2+6251*cfs3+cfs4^2)+cfcPGCollect(cfs2+6261*cfs3+cfs4^2)+cfcPGCollect(cfs2+6271*cfs3+cfs4^2)+cfcPGCollect(cfs2+6281*cfs3+cfs4^2)+cfcPGCollect(cfs2+6291*cfs3+cfs4^2)+cfcPGCollect(cfs2+6301*cfs3+cfs4^2)+cfcPGCollect(cfs2+6311*cfs3+cfs4^2)+cfcPGCollect(cfs2+6321*cfs3+cfs4^2)+cfcPGCollect(cfs2+6331*cfs3+cfs4^2)+cfcPGCollect(cfs2+6341*cfs3+cfs4^2)+cfcPGCollect(cfs2+6351*cfs3+cfs4^2)+cfcPGCollect(cfs2+6361*cfs3+cfs4^2)+cfcPGCollect(cfs2+6371*cfs3+cfs4^2)+cfcPGCollect(cfs2+6381*cfs3+cfs4^2)+cfcPGCollect(cfs2+6391*cfs3+cfs4^2)+cfcPGCollect(cfs2+6401*cfs3+cfs4^2)+cfcPGCollect(cfs2+6411*cfs3+cfs4^2)+cfcPGCollect(cfs2+6421*cfs3+cfs4^2)+cfcPGCollect(cfs2+6431*cfs3+cfs4^2)+cfcPGCollect(cfs2+6441*cfs3+cfs4^2)+cfcPGCollect(cfs2+6451*cfs3+cfs4^2)+cfcPGCollect(cfs2+6461*cfs3+cfs4^2)+cfcPGCollect(cfs2+6471*cfs3+cfs4^2)+cfcPGCollect(cfs2+6481*cfs3+cfs4^2)+cfcPGCollect(cfs2+6491*cfs3+cfs4^2)+cfcPGCollect(cfs2+6501*cfs3+cfs4^2)+cfcPGCollect(cfs2+6511*cfs3+cfs4^2)+cfcPGCollect(cfs2+6521*cfs3+cfs4^2)+cfcPGCollect(cfs2+6531*cfs3+cfs4^2)+cfcPGCollect(cfs2+6541*cfs3+cfs4^2)+cfcPGCollect(cfs2+6551*cfs3+cfs4^2)+cfcPGCollect(cfs2+6561*cfs3+cfs4^2)+cfcPGCollect(cfs2+6571*cfs3+cfs4^2)+cfcPGCollect(cfs2+6581*cfs3+cfs4^2)+cfcPGCollect(cfs2+6591*cfs3+cfs4^2)+cfcPGCollect(cfs2+6601*cfs3+cfs4^2)+cfcPGCollect(cfs2+6611*cfs3+cfs4^2)+cfcPGCollect(cfs2+6621*cfs3+cfs4^2)+cfcPGCollect(cfs2+6631*cfs3+cfs4^2)+cfcPGCollect(cfs2+6641*cfs3+cfs4^2);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcPGIndex?)=cfcShare(cfcPGIndex);
.sort
Hide cfcLateTest;
#$cfcPGOK=7;
#$cfcPGCost=11;
#$cfcPGDependent=13;
#$cfcPGFractional=17;
#call cfcPGRegisterScalars(cfcLateTest)
#if (`$cfcPGOK' != 7) || (`$cfcPGCost' != 11) || (`$cfcPGDependent' != 13) || (`$cfcPGFractional' != 17)
#message FAIL caller state
#terminate 1
#endif
#do cfcLateKey=cfcLateTest
#$cfcLateIndex=`cfcLateKey';
#inside $cfcLateIndex
id cfcShare(cfcPGIndex?)=cfcPGIndex;
#endinside
#$cfcLateValue=cfcMRMeta(`$cfcLateIndex');
#write "LATE Scalars-65 cfcMRMeta %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGTerms(`$cfcLateIndex');
#write "LATE Scalars-65 cfcPGTerms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGAtoms(`$cfcLateIndex');
#write "LATE Scalars-65 cfcPGAtoms %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGReach(`$cfcLateIndex');
#write "LATE Scalars-65 cfcPGReach %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGSize(`$cfcLateIndex');
#write "LATE Scalars-65 cfcPGSize %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGDenOK(`$cfcLateIndex');
#write "LATE Scalars-65 cfcPGDenOK %$ %$",$cfcLateIndex,$cfcLateValue
#$cfcLateValue=cfcPGParts(`$cfcLateIndex');
#write "LATE Scalars-65 cfcPGParts %$ %$",$cfcLateIndex,$cfcLateValue
#enddo
* A repeat must use complete registration, without rebuilding metadata.
#$cfcLateBefore=$cfcLateFlushes;
#call cfcPGRegisterScalars(cfcLateTest)
#if `$cfcLateBefore' != `$cfcLateFlushes'
#message FAIL repeated registration rebuilt metadata
#terminate 1
#endif
#do cfcLeakSlot=1,32
#ifdef `cfcMBId`cfcLeakSlot''
#message FAIL leaked slot
#terminate 1
#endif
#enddo
#ifdef `cfcMBCount'
#message FAIL leaked counter
#terminate 1
#endif
Unhide cfcLateTest;
Drop cfcLateTest;
.sort
#write "LATECASE Scalars-65 PASS"
Skip;
Local cfcLeafTest=0;
.sort
* Fill only the known direct-source facts. Unknown dependencies remain symbolic.
#$cfcLeafTerms=termsin_(cfcLeafTest);
Fill cfcMBTerms(900001)=`$cfcLeafTerms';
Fill cfcMBAtoms(900001)=1;
Fill cfcMBLinks(900001)=0;
#call cfcPGRegister(cfcLeafTest,900001)
#$cfcLateValue=cfcPGTerms(900001);
#write "LEAF 1 cfcPGTerms %$",$cfcLateValue
#$cfcLateValue=cfcPGAtoms(900001);
#write "LEAF 1 cfcPGAtoms %$",$cfcLateValue
#$cfcLateValue=cfcPGReach(900001);
#write "LEAF 1 cfcPGReach %$",$cfcLateValue
#$cfcLateValue=cfcPGSize(900001);
#write "LEAF 1 cfcPGSize %$",$cfcLateValue
#$cfcLateValue=cfcPGDenOK(900001);
#write "LEAF 1 cfcPGDenOK %$",$cfcLateValue
#$cfcLateValue=cfcPGParts(900001);
#write "LEAF 1 cfcPGParts %$",$cfcLateValue
Drop cfcLeafTest;
.sort
#write "LEAFCASE 1 PASS"
Skip;
Local cfcLeafTest=1;
.sort
* Fill only the known direct-source facts. Unknown dependencies remain symbolic.
#$cfcLeafTerms=termsin_(cfcLeafTest);
Fill cfcMBTerms(900002)=`$cfcLeafTerms';
Fill cfcMBAtoms(900002)=1;
Fill cfcMBLinks(900002)=0;
#call cfcPGRegister(cfcLeafTest,900002)
#$cfcLateValue=cfcPGTerms(900002);
#write "LEAF 2 cfcPGTerms %$",$cfcLateValue
#$cfcLateValue=cfcPGAtoms(900002);
#write "LEAF 2 cfcPGAtoms %$",$cfcLateValue
#$cfcLateValue=cfcPGReach(900002);
#write "LEAF 2 cfcPGReach %$",$cfcLateValue
#$cfcLateValue=cfcPGSize(900002);
#write "LEAF 2 cfcPGSize %$",$cfcLateValue
#$cfcLateValue=cfcPGDenOK(900002);
#write "LEAF 2 cfcPGDenOK %$",$cfcLateValue
#$cfcLateValue=cfcPGParts(900002);
#write "LEAF 2 cfcPGParts %$",$cfcLateValue
Drop cfcLeafTest;
.sort
#write "LEAFCASE 2 PASS"
Skip;
Local cfcLeafTest=i_*cfs2;
.sort
* Fill only the known direct-source facts. Unknown dependencies remain symbolic.
#$cfcLeafTerms=termsin_(cfcLeafTest);
Fill cfcMBTerms(900003)=`$cfcLeafTerms';
Fill cfcMBAtoms(900003)=1;
Fill cfcMBLinks(900003)=0;
#call cfcPGRegister(cfcLeafTest,900003)
#$cfcLateValue=cfcPGTerms(900003);
#write "LEAF 3 cfcPGTerms %$",$cfcLateValue
#$cfcLateValue=cfcPGAtoms(900003);
#write "LEAF 3 cfcPGAtoms %$",$cfcLateValue
#$cfcLateValue=cfcPGReach(900003);
#write "LEAF 3 cfcPGReach %$",$cfcLateValue
#$cfcLateValue=cfcPGSize(900003);
#write "LEAF 3 cfcPGSize %$",$cfcLateValue
#$cfcLateValue=cfcPGDenOK(900003);
#write "LEAF 3 cfcPGDenOK %$",$cfcLateValue
#$cfcLateValue=cfcPGParts(900003);
#write "LEAF 3 cfcPGParts %$",$cfcLateValue
Drop cfcLeafTest;
.sort
#write "LEAFCASE 3 PASS"
Skip;
Local cfcLeafTest=cfs2^-1;
.sort
* Fill only the known direct-source facts. Unknown dependencies remain symbolic.
#$cfcLeafTerms=termsin_(cfcLeafTest);
Fill cfcMBTerms(900004)=`$cfcLeafTerms';
Fill cfcMBAtoms(900004)=1;
Fill cfcMBLinks(900004)=0;
#call cfcPGRegister(cfcLeafTest,900004)
#$cfcLateValue=cfcPGTerms(900004);
#write "LEAF 4 cfcPGTerms %$",$cfcLateValue
#$cfcLateValue=cfcPGAtoms(900004);
#write "LEAF 4 cfcPGAtoms %$",$cfcLateValue
#$cfcLateValue=cfcPGReach(900004);
#write "LEAF 4 cfcPGReach %$",$cfcLateValue
#$cfcLateValue=cfcPGSize(900004);
#write "LEAF 4 cfcPGSize %$",$cfcLateValue
#$cfcLateValue=cfcPGDenOK(900004);
#write "LEAF 4 cfcPGDenOK %$",$cfcLateValue
#$cfcLateValue=cfcPGParts(900004);
#write "LEAF 4 cfcPGParts %$",$cfcLateValue
Drop cfcLeafTest;
.sort
#write "LEAFCASE 4 PASS"
Skip;
Local cfcLeafTest=cfcShare(999999);
.sort
* Fill only the known direct-source facts. Unknown dependencies remain symbolic.
#$cfcLeafTerms=termsin_(cfcLeafTest);
Fill cfcMBTerms(900005)=`$cfcLeafTerms';
Fill cfcMBAtoms(900005)=1;
Fill cfcMBLinks(900005)=cfcPGRef(999999);
#call cfcPGRegister(cfcLeafTest,900005)
#$cfcLateValue=cfcPGTerms(900005);
#write "LEAF 5 cfcPGTerms %$",$cfcLateValue
#$cfcLateValue=cfcPGAtoms(900005);
#write "LEAF 5 cfcPGAtoms %$",$cfcLateValue
#$cfcLateValue=cfcPGReach(900005);
#write "LEAF 5 cfcPGReach %$",$cfcLateValue
#$cfcLateValue=cfcPGSize(900005);
#write "LEAF 5 cfcPGSize %$",$cfcLateValue
#$cfcLateValue=cfcPGDenOK(900005);
#write "LEAF 5 cfcPGDenOK %$",$cfcLateValue
#$cfcLateValue=cfcPGParts(900005);
#write "LEAF 5 cfcPGParts %$",$cfcLateValue
Drop cfcLeafTest;
.sort
#write "LEAFCASE 5 PASS"
Skip;
Local cfcLeafTest=d_(1,2);
.sort
* Fill only the known direct-source facts. Unknown dependencies remain symbolic.
#$cfcLeafTerms=termsin_(cfcLeafTest);
Fill cfcMBTerms(900006)=`$cfcLeafTerms';
Fill cfcMBAtoms(900006)=1;
Fill cfcMBLinks(900006)=0;
#call cfcPGRegister(cfcLeafTest,900006)
#$cfcLateValue=cfcPGTerms(900006);
#write "LEAF 6 cfcPGTerms %$",$cfcLateValue
#$cfcLateValue=cfcPGAtoms(900006);
#write "LEAF 6 cfcPGAtoms %$",$cfcLateValue
#$cfcLateValue=cfcPGReach(900006);
#write "LEAF 6 cfcPGReach %$",$cfcLateValue
#$cfcLateValue=cfcPGSize(900006);
#write "LEAF 6 cfcPGSize %$",$cfcLateValue
#$cfcLateValue=cfcPGDenOK(900006);
#write "LEAF 6 cfcPGDenOK %$",$cfcLateValue
#$cfcLateValue=cfcPGParts(900006);
#write "LEAF 6 cfcPGParts %$",$cfcLateValue
Drop cfcLeafTest;
.sort
#write "LEAFCASE 6 PASS"
Skip;
Local cfcLeafTest=cfcPolyD(999998);
.sort
* Fill only the known direct-source facts. Unknown dependencies remain symbolic.
#$cfcLeafTerms=termsin_(cfcLeafTest);
Fill cfcMBTerms(900007)=`$cfcLeafTerms';
Fill cfcMBAtoms(900007)=1;
Fill cfcMBLinks(900007)=cfcPGRef(999998);
#call cfcPGRegister(cfcLeafTest,900007)
#$cfcLateValue=cfcPGTerms(900007);
#write "LEAF 7 cfcPGTerms %$",$cfcLateValue
#$cfcLateValue=cfcPGAtoms(900007);
#write "LEAF 7 cfcPGAtoms %$",$cfcLateValue
#$cfcLateValue=cfcPGReach(900007);
#write "LEAF 7 cfcPGReach %$",$cfcLateValue
#$cfcLateValue=cfcPGSize(900007);
#write "LEAF 7 cfcPGSize %$",$cfcLateValue
#$cfcLateValue=cfcPGDenOK(900007);
#write "LEAF 7 cfcPGDenOK %$",$cfcLateValue
#$cfcLateValue=cfcPGParts(900007);
#write "LEAF 7 cfcPGParts %$",$cfcLateValue
Drop cfcLeafTest;
.sort
#write "LEAFCASE 7 PASS"
#write "LATEFLUSHES %$",$cfcLateFlushes
.end
