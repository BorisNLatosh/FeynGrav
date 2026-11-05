* Test-only driver; procedures above are rendered from the actual templates.
Fill cfcMRMeta(999999)=2*cfcMRDegree*cfs2;
Fill cfcPGTerms(999999)=2;
.sort
Skip;
Local cfcRBTest=cfs2+cfs3+cfs4;
Local cfcRBIntern=cfcPGCollect(cfs2+cfs3+cfs4);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcMRIndex?$cfcRBTestIndex)=1;
ModuleOption noparallel;
.sort
#define cfcRBId1 "`$cfcRBTestIndex'"
#$cfcRBTestTerms=termsin_(cfcRBTest);
Fill cfcMBTerms(`cfcRBId1')=`$cfcRBTestTerms';
Fill cfcMBLinks(`cfcRBId1')=0;
#call cfcMRInspect(cfcRBTest)
#$cfcRBExpectedLimit=$cfcMRLimit;
#$cfcRBExpectedDegreeLimit=$cfcMRDegreeLimit;
#$cfcRBExpectedOK=$cfcMROK;
#$cfcRBExpectedCost=$cfcMRCost;
#$cfcRBExpectedMax=$cfcMRMax;
#$cfcRBExpectedCount=$cfcMRCount;
#$cfcRBExpectedVars=$cfcMRVars;
#$cfcRBExpectedMeta=$cfcMRMeta;
#call cfcMRInspectBatch(1)
#if `$cfcRBReady1' != 1
#message FAIL readiness terms-below
#terminate 1
#endif
#call cfcMRUseBatch(cfcRBTest,1)
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedLimit-$cfcMRLimit;
#if `$cfcRBDifference' != 0
#message FAIL field terms-below Limit
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedDegreeLimit-$cfcMRDegreeLimit;
#if `$cfcRBDifference' != 0
#message FAIL field terms-below DegreeLimit
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedOK-$cfcRBOK1;
#else
#$cfcRBDifference=$cfcRBExpectedOK-$cfcMROK;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-below OK
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCost-$cfcRBCost1;
#else
#$cfcRBDifference=$cfcRBExpectedCost-$cfcMRCost;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-below Cost
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMax-$cfcRBMax1;
#else
#$cfcRBDifference=$cfcRBExpectedMax-$cfcMRMax;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-below Max
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCount-$cfcRBCount1;
#else
#$cfcRBDifference=$cfcRBExpectedCount-$cfcMRCount;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-below Count
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedVars-$cfcRBVars1;
#else
#$cfcRBDifference=$cfcRBExpectedVars-$cfcMRVars;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-below Vars
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcRBMeta1;
#else
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcMRMeta;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-below Meta
#terminate 1
#endif
#if `$cfcMROK' != 1
#message FAIL eligibility terms-below
#terminate 1
#endif
#write "RBBOUND terms-below PASS"
Drop cfcRBTest,cfcRBIntern;
.sort
Skip;
Local cfcRBTest=1+cfs2+cfs3+cfs4;
Local cfcRBIntern=cfcPGCollect(1+cfs2+cfs3+cfs4);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcMRIndex?$cfcRBTestIndex)=1;
ModuleOption noparallel;
.sort
#define cfcRBId1 "`$cfcRBTestIndex'"
#$cfcRBTestTerms=termsin_(cfcRBTest);
Fill cfcMBTerms(`cfcRBId1')=`$cfcRBTestTerms';
Fill cfcMBLinks(`cfcRBId1')=0;
#call cfcMRInspect(cfcRBTest)
#$cfcRBExpectedLimit=$cfcMRLimit;
#$cfcRBExpectedDegreeLimit=$cfcMRDegreeLimit;
#$cfcRBExpectedOK=$cfcMROK;
#$cfcRBExpectedCost=$cfcMRCost;
#$cfcRBExpectedMax=$cfcMRMax;
#$cfcRBExpectedCount=$cfcMRCount;
#$cfcRBExpectedVars=$cfcMRVars;
#$cfcRBExpectedMeta=$cfcMRMeta;
#call cfcMRInspectBatch(1)
#if `$cfcRBReady1' != 1
#message FAIL readiness terms-at
#terminate 1
#endif
#call cfcMRUseBatch(cfcRBTest,1)
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedLimit-$cfcMRLimit;
#if `$cfcRBDifference' != 0
#message FAIL field terms-at Limit
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedDegreeLimit-$cfcMRDegreeLimit;
#if `$cfcRBDifference' != 0
#message FAIL field terms-at DegreeLimit
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedOK-$cfcRBOK1;
#else
#$cfcRBDifference=$cfcRBExpectedOK-$cfcMROK;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-at OK
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCost-$cfcRBCost1;
#else
#$cfcRBDifference=$cfcRBExpectedCost-$cfcMRCost;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-at Cost
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMax-$cfcRBMax1;
#else
#$cfcRBDifference=$cfcRBExpectedMax-$cfcMRMax;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-at Max
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCount-$cfcRBCount1;
#else
#$cfcRBDifference=$cfcRBExpectedCount-$cfcMRCount;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-at Count
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedVars-$cfcRBVars1;
#else
#$cfcRBDifference=$cfcRBExpectedVars-$cfcMRVars;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-at Vars
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcRBMeta1;
#else
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcMRMeta;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-at Meta
#terminate 1
#endif
#if `$cfcMROK' != 1
#message FAIL eligibility terms-at
#terminate 1
#endif
#write "RBBOUND terms-at PASS"
Drop cfcRBTest,cfcRBIntern;
.sort
Skip;
Local cfcRBTest=1+cfs2+cfs3+cfs4+cfs5;
Local cfcRBIntern=cfcPGCollect(1+cfs2+cfs3+cfs4+cfs5);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcMRIndex?$cfcRBTestIndex)=1;
ModuleOption noparallel;
.sort
#define cfcRBId1 "`$cfcRBTestIndex'"
#$cfcRBTestTerms=termsin_(cfcRBTest);
Fill cfcMBTerms(`cfcRBId1')=`$cfcRBTestTerms';
Fill cfcMBLinks(`cfcRBId1')=0;
#call cfcMRInspect(cfcRBTest)
#$cfcRBExpectedLimit=$cfcMRLimit;
#$cfcRBExpectedDegreeLimit=$cfcMRDegreeLimit;
#$cfcRBExpectedOK=$cfcMROK;
#$cfcRBExpectedCost=$cfcMRCost;
#$cfcRBExpectedMax=$cfcMRMax;
#$cfcRBExpectedCount=$cfcMRCount;
#$cfcRBExpectedVars=$cfcMRVars;
#$cfcRBExpectedMeta=$cfcMRMeta;
#call cfcMRInspectBatch(1)
#if `$cfcRBReady1' != 0
#message FAIL readiness terms-above
#terminate 1
#endif
#call cfcMRUseBatch(cfcRBTest,1)
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedLimit-$cfcMRLimit;
#if `$cfcRBDifference' != 0
#message FAIL field terms-above Limit
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedDegreeLimit-$cfcMRDegreeLimit;
#if `$cfcRBDifference' != 0
#message FAIL field terms-above DegreeLimit
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedOK-$cfcRBOK1;
#else
#$cfcRBDifference=$cfcRBExpectedOK-$cfcMROK;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-above OK
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCost-$cfcRBCost1;
#else
#$cfcRBDifference=$cfcRBExpectedCost-$cfcMRCost;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-above Cost
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMax-$cfcRBMax1;
#else
#$cfcRBDifference=$cfcRBExpectedMax-$cfcMRMax;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-above Max
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCount-$cfcRBCount1;
#else
#$cfcRBDifference=$cfcRBExpectedCount-$cfcMRCount;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-above Count
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedVars-$cfcRBVars1;
#else
#$cfcRBDifference=$cfcRBExpectedVars-$cfcMRVars;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-above Vars
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcRBMeta1;
#else
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcMRMeta;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field terms-above Meta
#terminate 1
#endif
#if `$cfcMROK' != 0
#message FAIL eligibility terms-above
#terminate 1
#endif
#write "RBBOUND terms-above PASS"
Drop cfcRBTest,cfcRBIntern;
.sort
Skip;
Local cfcRBTest=1+cfs2^3;
Local cfcRBIntern=cfcPGCollect(1+cfs2^3);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcMRIndex?$cfcRBTestIndex)=1;
ModuleOption noparallel;
.sort
#define cfcRBId1 "`$cfcRBTestIndex'"
#$cfcRBTestTerms=termsin_(cfcRBTest);
Fill cfcMBTerms(`cfcRBId1')=`$cfcRBTestTerms';
Fill cfcMBLinks(`cfcRBId1')=0;
#call cfcMRInspect(cfcRBTest)
#$cfcRBExpectedLimit=$cfcMRLimit;
#$cfcRBExpectedDegreeLimit=$cfcMRDegreeLimit;
#$cfcRBExpectedOK=$cfcMROK;
#$cfcRBExpectedCost=$cfcMRCost;
#$cfcRBExpectedMax=$cfcMRMax;
#$cfcRBExpectedCount=$cfcMRCount;
#$cfcRBExpectedVars=$cfcMRVars;
#$cfcRBExpectedMeta=$cfcMRMeta;
#call cfcMRInspectBatch(1)
#if `$cfcRBReady1' != 1
#message FAIL readiness degree-below
#terminate 1
#endif
#call cfcMRUseBatch(cfcRBTest,1)
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedLimit-$cfcMRLimit;
#if `$cfcRBDifference' != 0
#message FAIL field degree-below Limit
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedDegreeLimit-$cfcMRDegreeLimit;
#if `$cfcRBDifference' != 0
#message FAIL field degree-below DegreeLimit
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedOK-$cfcRBOK1;
#else
#$cfcRBDifference=$cfcRBExpectedOK-$cfcMROK;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-below OK
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCost-$cfcRBCost1;
#else
#$cfcRBDifference=$cfcRBExpectedCost-$cfcMRCost;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-below Cost
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMax-$cfcRBMax1;
#else
#$cfcRBDifference=$cfcRBExpectedMax-$cfcMRMax;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-below Max
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCount-$cfcRBCount1;
#else
#$cfcRBDifference=$cfcRBExpectedCount-$cfcMRCount;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-below Count
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedVars-$cfcRBVars1;
#else
#$cfcRBDifference=$cfcRBExpectedVars-$cfcMRVars;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-below Vars
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcRBMeta1;
#else
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcMRMeta;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-below Meta
#terminate 1
#endif
#if `$cfcMROK' != 1
#message FAIL eligibility degree-below
#terminate 1
#endif
#write "RBBOUND degree-below PASS"
Drop cfcRBTest,cfcRBIntern;
.sort
Skip;
Local cfcRBTest=1+cfs2^4;
Local cfcRBIntern=cfcPGCollect(1+cfs2^4);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcMRIndex?$cfcRBTestIndex)=1;
ModuleOption noparallel;
.sort
#define cfcRBId1 "`$cfcRBTestIndex'"
#$cfcRBTestTerms=termsin_(cfcRBTest);
Fill cfcMBTerms(`cfcRBId1')=`$cfcRBTestTerms';
Fill cfcMBLinks(`cfcRBId1')=0;
#call cfcMRInspect(cfcRBTest)
#$cfcRBExpectedLimit=$cfcMRLimit;
#$cfcRBExpectedDegreeLimit=$cfcMRDegreeLimit;
#$cfcRBExpectedOK=$cfcMROK;
#$cfcRBExpectedCost=$cfcMRCost;
#$cfcRBExpectedMax=$cfcMRMax;
#$cfcRBExpectedCount=$cfcMRCount;
#$cfcRBExpectedVars=$cfcMRVars;
#$cfcRBExpectedMeta=$cfcMRMeta;
#call cfcMRInspectBatch(1)
#if `$cfcRBReady1' != 1
#message FAIL readiness degree-at
#terminate 1
#endif
#call cfcMRUseBatch(cfcRBTest,1)
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedLimit-$cfcMRLimit;
#if `$cfcRBDifference' != 0
#message FAIL field degree-at Limit
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedDegreeLimit-$cfcMRDegreeLimit;
#if `$cfcRBDifference' != 0
#message FAIL field degree-at DegreeLimit
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedOK-$cfcRBOK1;
#else
#$cfcRBDifference=$cfcRBExpectedOK-$cfcMROK;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-at OK
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCost-$cfcRBCost1;
#else
#$cfcRBDifference=$cfcRBExpectedCost-$cfcMRCost;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-at Cost
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMax-$cfcRBMax1;
#else
#$cfcRBDifference=$cfcRBExpectedMax-$cfcMRMax;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-at Max
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCount-$cfcRBCount1;
#else
#$cfcRBDifference=$cfcRBExpectedCount-$cfcMRCount;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-at Count
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedVars-$cfcRBVars1;
#else
#$cfcRBDifference=$cfcRBExpectedVars-$cfcMRVars;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-at Vars
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcRBMeta1;
#else
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcMRMeta;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-at Meta
#terminate 1
#endif
#if `$cfcMROK' != 1
#message FAIL eligibility degree-at
#terminate 1
#endif
#write "RBBOUND degree-at PASS"
Drop cfcRBTest,cfcRBIntern;
.sort
Skip;
Local cfcRBTest=1+cfs2^5;
Local cfcRBIntern=cfcPGCollect(1+cfs2^5);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcMRIndex?$cfcRBTestIndex)=1;
ModuleOption noparallel;
.sort
#define cfcRBId1 "`$cfcRBTestIndex'"
#$cfcRBTestTerms=termsin_(cfcRBTest);
Fill cfcMBTerms(`cfcRBId1')=`$cfcRBTestTerms';
Fill cfcMBLinks(`cfcRBId1')=0;
#call cfcMRInspect(cfcRBTest)
#$cfcRBExpectedLimit=$cfcMRLimit;
#$cfcRBExpectedDegreeLimit=$cfcMRDegreeLimit;
#$cfcRBExpectedOK=$cfcMROK;
#$cfcRBExpectedCost=$cfcMRCost;
#$cfcRBExpectedMax=$cfcMRMax;
#$cfcRBExpectedCount=$cfcMRCount;
#$cfcRBExpectedVars=$cfcMRVars;
#$cfcRBExpectedMeta=$cfcMRMeta;
#call cfcMRInspectBatch(1)
#if `$cfcRBReady1' != 1
#message FAIL readiness degree-above
#terminate 1
#endif
#call cfcMRUseBatch(cfcRBTest,1)
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedLimit-$cfcMRLimit;
#if `$cfcRBDifference' != 0
#message FAIL field degree-above Limit
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedDegreeLimit-$cfcMRDegreeLimit;
#if `$cfcRBDifference' != 0
#message FAIL field degree-above DegreeLimit
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedOK-$cfcRBOK1;
#else
#$cfcRBDifference=$cfcRBExpectedOK-$cfcMROK;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-above OK
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCost-$cfcRBCost1;
#else
#$cfcRBDifference=$cfcRBExpectedCost-$cfcMRCost;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-above Cost
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMax-$cfcRBMax1;
#else
#$cfcRBDifference=$cfcRBExpectedMax-$cfcMRMax;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-above Max
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCount-$cfcRBCount1;
#else
#$cfcRBDifference=$cfcRBExpectedCount-$cfcMRCount;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-above Count
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedVars-$cfcRBVars1;
#else
#$cfcRBDifference=$cfcRBExpectedVars-$cfcMRVars;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-above Vars
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcRBMeta1;
#else
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcMRMeta;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field degree-above Meta
#terminate 1
#endif
#if `$cfcMROK' != 0
#message FAIL eligibility degree-above
#terminate 1
#endif
#write "RBBOUND degree-above PASS"
Drop cfcRBTest,cfcRBIntern;
.sort
Skip;
Local cfcRBTest=1+cfs2*cfs3;
Local cfcRBIntern=cfcPGCollect(1+cfs2*cfs3);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcMRIndex?$cfcRBTestIndex)=1;
ModuleOption noparallel;
.sort
#define cfcRBId1 "`$cfcRBTestIndex'"
#$cfcRBTestTerms=termsin_(cfcRBTest);
Fill cfcMBTerms(`cfcRBId1')=`$cfcRBTestTerms';
Fill cfcMBLinks(`cfcRBId1')=0;
#call cfcMRInspect(cfcRBTest)
#$cfcRBExpectedLimit=$cfcMRLimit;
#$cfcRBExpectedDegreeLimit=$cfcMRDegreeLimit;
#$cfcRBExpectedOK=$cfcMROK;
#$cfcRBExpectedCost=$cfcMRCost;
#$cfcRBExpectedMax=$cfcMRMax;
#$cfcRBExpectedCount=$cfcMRCount;
#$cfcRBExpectedVars=$cfcMRVars;
#$cfcRBExpectedMeta=$cfcMRMeta;
#call cfcMRInspectBatch(1)
#if `$cfcRBReady1' != 1
#message FAIL readiness variables-below
#terminate 1
#endif
#call cfcMRUseBatch(cfcRBTest,1)
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedLimit-$cfcMRLimit;
#if `$cfcRBDifference' != 0
#message FAIL field variables-below Limit
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedDegreeLimit-$cfcMRDegreeLimit;
#if `$cfcRBDifference' != 0
#message FAIL field variables-below DegreeLimit
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedOK-$cfcRBOK1;
#else
#$cfcRBDifference=$cfcRBExpectedOK-$cfcMROK;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-below OK
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCost-$cfcRBCost1;
#else
#$cfcRBDifference=$cfcRBExpectedCost-$cfcMRCost;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-below Cost
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMax-$cfcRBMax1;
#else
#$cfcRBDifference=$cfcRBExpectedMax-$cfcMRMax;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-below Max
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCount-$cfcRBCount1;
#else
#$cfcRBDifference=$cfcRBExpectedCount-$cfcMRCount;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-below Count
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedVars-$cfcRBVars1;
#else
#$cfcRBDifference=$cfcRBExpectedVars-$cfcMRVars;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-below Vars
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcRBMeta1;
#else
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcMRMeta;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-below Meta
#terminate 1
#endif
#if `$cfcMROK' != 1
#message FAIL eligibility variables-below
#terminate 1
#endif
#write "RBBOUND variables-below PASS"
Drop cfcRBTest,cfcRBIntern;
.sort
Skip;
Local cfcRBTest=1+cfs2*cfs3*cfs4;
Local cfcRBIntern=cfcPGCollect(1+cfs2*cfs3*cfs4);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcMRIndex?$cfcRBTestIndex)=1;
ModuleOption noparallel;
.sort
#define cfcRBId1 "`$cfcRBTestIndex'"
#$cfcRBTestTerms=termsin_(cfcRBTest);
Fill cfcMBTerms(`cfcRBId1')=`$cfcRBTestTerms';
Fill cfcMBLinks(`cfcRBId1')=0;
#call cfcMRInspect(cfcRBTest)
#$cfcRBExpectedLimit=$cfcMRLimit;
#$cfcRBExpectedDegreeLimit=$cfcMRDegreeLimit;
#$cfcRBExpectedOK=$cfcMROK;
#$cfcRBExpectedCost=$cfcMRCost;
#$cfcRBExpectedMax=$cfcMRMax;
#$cfcRBExpectedCount=$cfcMRCount;
#$cfcRBExpectedVars=$cfcMRVars;
#$cfcRBExpectedMeta=$cfcMRMeta;
#call cfcMRInspectBatch(1)
#if `$cfcRBReady1' != 1
#message FAIL readiness variables-at
#terminate 1
#endif
#call cfcMRUseBatch(cfcRBTest,1)
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedLimit-$cfcMRLimit;
#if `$cfcRBDifference' != 0
#message FAIL field variables-at Limit
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedDegreeLimit-$cfcMRDegreeLimit;
#if `$cfcRBDifference' != 0
#message FAIL field variables-at DegreeLimit
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedOK-$cfcRBOK1;
#else
#$cfcRBDifference=$cfcRBExpectedOK-$cfcMROK;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-at OK
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCost-$cfcRBCost1;
#else
#$cfcRBDifference=$cfcRBExpectedCost-$cfcMRCost;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-at Cost
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMax-$cfcRBMax1;
#else
#$cfcRBDifference=$cfcRBExpectedMax-$cfcMRMax;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-at Max
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCount-$cfcRBCount1;
#else
#$cfcRBDifference=$cfcRBExpectedCount-$cfcMRCount;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-at Count
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedVars-$cfcRBVars1;
#else
#$cfcRBDifference=$cfcRBExpectedVars-$cfcMRVars;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-at Vars
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcRBMeta1;
#else
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcMRMeta;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-at Meta
#terminate 1
#endif
#if `$cfcMROK' != 1
#message FAIL eligibility variables-at
#terminate 1
#endif
#write "RBBOUND variables-at PASS"
Drop cfcRBTest,cfcRBIntern;
.sort
Skip;
Local cfcRBTest=1+cfs2*cfs3*cfs4*cfs5;
Local cfcRBIntern=cfcPGCollect(1+cfs2*cfs3*cfs4*cfs5);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcMRIndex?$cfcRBTestIndex)=1;
ModuleOption noparallel;
.sort
#define cfcRBId1 "`$cfcRBTestIndex'"
#$cfcRBTestTerms=termsin_(cfcRBTest);
Fill cfcMBTerms(`cfcRBId1')=`$cfcRBTestTerms';
Fill cfcMBLinks(`cfcRBId1')=0;
#call cfcMRInspect(cfcRBTest)
#$cfcRBExpectedLimit=$cfcMRLimit;
#$cfcRBExpectedDegreeLimit=$cfcMRDegreeLimit;
#$cfcRBExpectedOK=$cfcMROK;
#$cfcRBExpectedCost=$cfcMRCost;
#$cfcRBExpectedMax=$cfcMRMax;
#$cfcRBExpectedCount=$cfcMRCount;
#$cfcRBExpectedVars=$cfcMRVars;
#$cfcRBExpectedMeta=$cfcMRMeta;
#call cfcMRInspectBatch(1)
#if `$cfcRBReady1' != 1
#message FAIL readiness variables-above
#terminate 1
#endif
#call cfcMRUseBatch(cfcRBTest,1)
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedLimit-$cfcMRLimit;
#if `$cfcRBDifference' != 0
#message FAIL field variables-above Limit
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedDegreeLimit-$cfcMRDegreeLimit;
#if `$cfcRBDifference' != 0
#message FAIL field variables-above DegreeLimit
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedOK-$cfcRBOK1;
#else
#$cfcRBDifference=$cfcRBExpectedOK-$cfcMROK;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-above OK
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCost-$cfcRBCost1;
#else
#$cfcRBDifference=$cfcRBExpectedCost-$cfcMRCost;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-above Cost
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMax-$cfcRBMax1;
#else
#$cfcRBDifference=$cfcRBExpectedMax-$cfcMRMax;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-above Max
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCount-$cfcRBCount1;
#else
#$cfcRBDifference=$cfcRBExpectedCount-$cfcMRCount;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-above Count
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedVars-$cfcRBVars1;
#else
#$cfcRBDifference=$cfcRBExpectedVars-$cfcMRVars;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-above Vars
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcRBMeta1;
#else
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcMRMeta;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field variables-above Meta
#terminate 1
#endif
#if `$cfcMROK' != 0
#message FAIL eligibility variables-above
#terminate 1
#endif
#write "RBBOUND variables-above PASS"
Drop cfcRBTest,cfcRBIntern;
.sort
Skip;
Local cfcRBTest=i_*cfs1^2+cfs2;
Local cfcRBIntern=cfcPGCollect(i_*cfs1^2+cfs2);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcMRIndex?$cfcRBTestIndex)=1;
ModuleOption noparallel;
.sort
#define cfcRBId1 "`$cfcRBTestIndex'"
#$cfcRBTestTerms=termsin_(cfcRBTest);
Fill cfcMBTerms(`cfcRBId1')=`$cfcRBTestTerms';
Fill cfcMBLinks(`cfcRBId1')=0;
#call cfcMRInspect(cfcRBTest)
#$cfcRBExpectedLimit=$cfcMRLimit;
#$cfcRBExpectedDegreeLimit=$cfcMRDegreeLimit;
#$cfcRBExpectedOK=$cfcMROK;
#$cfcRBExpectedCost=$cfcMRCost;
#$cfcRBExpectedMax=$cfcMRMax;
#$cfcRBExpectedCount=$cfcMRCount;
#$cfcRBExpectedVars=$cfcMRVars;
#$cfcRBExpectedMeta=$cfcMRMeta;
#call cfcMRInspectBatch(1)
#if `$cfcRBReady1' != 1
#message FAIL readiness complex-D
#terminate 1
#endif
#call cfcMRUseBatch(cfcRBTest,1)
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedLimit-$cfcMRLimit;
#if `$cfcRBDifference' != 0
#message FAIL field complex-D Limit
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedDegreeLimit-$cfcMRDegreeLimit;
#if `$cfcRBDifference' != 0
#message FAIL field complex-D DegreeLimit
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedOK-$cfcRBOK1;
#else
#$cfcRBDifference=$cfcRBExpectedOK-$cfcMROK;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field complex-D OK
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCost-$cfcRBCost1;
#else
#$cfcRBDifference=$cfcRBExpectedCost-$cfcMRCost;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field complex-D Cost
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMax-$cfcRBMax1;
#else
#$cfcRBDifference=$cfcRBExpectedMax-$cfcMRMax;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field complex-D Max
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCount-$cfcRBCount1;
#else
#$cfcRBDifference=$cfcRBExpectedCount-$cfcMRCount;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field complex-D Count
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedVars-$cfcRBVars1;
#else
#$cfcRBDifference=$cfcRBExpectedVars-$cfcMRVars;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field complex-D Vars
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcRBMeta1;
#else
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcMRMeta;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field complex-D Meta
#terminate 1
#endif
#if `$cfcMROK' != 1
#message FAIL eligibility complex-D
#terminate 1
#endif
#write "RBBOUND complex-D PASS"
Drop cfcRBTest,cfcRBIntern;
.sort
Skip;
Local cfcRBTest=denom_(cfs2+cfs3)+cfs4;
Local cfcRBIntern=cfcPGCollect(denom_(cfs2+cfs3)+cfs4);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcMRIndex?$cfcRBTestIndex)=1;
ModuleOption noparallel;
.sort
#define cfcRBId1 "`$cfcRBTestIndex'"
#$cfcRBTestTerms=termsin_(cfcRBTest);
Fill cfcMBTerms(`cfcRBId1')=`$cfcRBTestTerms';
Fill cfcMBLinks(`cfcRBId1')=0;
#call cfcMRInspect(cfcRBTest)
#$cfcRBExpectedLimit=$cfcMRLimit;
#$cfcRBExpectedDegreeLimit=$cfcMRDegreeLimit;
#$cfcRBExpectedOK=$cfcMROK;
#$cfcRBExpectedCost=$cfcMRCost;
#$cfcRBExpectedMax=$cfcMRMax;
#$cfcRBExpectedCount=$cfcMRCount;
#$cfcRBExpectedVars=$cfcMRVars;
#$cfcRBExpectedMeta=$cfcMRMeta;
#call cfcMRInspectBatch(1)
#if `$cfcRBReady1' != 1
#message FAIL readiness native-denominator
#terminate 1
#endif
#call cfcMRUseBatch(cfcRBTest,1)
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedLimit-$cfcMRLimit;
#if `$cfcRBDifference' != 0
#message FAIL field native-denominator Limit
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedDegreeLimit-$cfcMRDegreeLimit;
#if `$cfcRBDifference' != 0
#message FAIL field native-denominator DegreeLimit
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedOK-$cfcRBOK1;
#else
#$cfcRBDifference=$cfcRBExpectedOK-$cfcMROK;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field native-denominator OK
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCost-$cfcRBCost1;
#else
#$cfcRBDifference=$cfcRBExpectedCost-$cfcMRCost;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field native-denominator Cost
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMax-$cfcRBMax1;
#else
#$cfcRBDifference=$cfcRBExpectedMax-$cfcMRMax;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field native-denominator Max
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCount-$cfcRBCount1;
#else
#$cfcRBDifference=$cfcRBExpectedCount-$cfcMRCount;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field native-denominator Count
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedVars-$cfcRBVars1;
#else
#$cfcRBDifference=$cfcRBExpectedVars-$cfcMRVars;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field native-denominator Vars
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcRBMeta1;
#else
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcMRMeta;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field native-denominator Meta
#terminate 1
#endif
#if `$cfcMROK' != 1
#message FAIL eligibility native-denominator
#terminate 1
#endif
#write "RBBOUND native-denominator PASS"
Drop cfcRBTest,cfcRBIntern;
.sort
Skip;
Local cfcRBTest=cfcTC(cfs2)+cfs3;
Local cfcRBIntern=cfcPGCollect(cfcTC(cfs2)+cfs3);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcMRIndex?$cfcRBTestIndex)=1;
ModuleOption noparallel;
.sort
#define cfcRBId1 "`$cfcRBTestIndex'"
#$cfcRBTestTerms=termsin_(cfcRBTest);
Fill cfcMBTerms(`cfcRBId1')=`$cfcRBTestTerms';
Fill cfcMBLinks(`cfcRBId1')=0;
#call cfcMRInspect(cfcRBTest)
#$cfcRBExpectedLimit=$cfcMRLimit;
#$cfcRBExpectedDegreeLimit=$cfcMRDegreeLimit;
#$cfcRBExpectedOK=$cfcMROK;
#$cfcRBExpectedCost=$cfcMRCost;
#$cfcRBExpectedMax=$cfcMRMax;
#$cfcRBExpectedCount=$cfcMRCount;
#$cfcRBExpectedVars=$cfcMRVars;
#$cfcRBExpectedMeta=$cfcMRMeta;
#call cfcMRInspectBatch(1)
#if `$cfcRBReady1' != 1
#message FAIL readiness unsupported
#terminate 1
#endif
#call cfcMRUseBatch(cfcRBTest,1)
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedLimit-$cfcMRLimit;
#if `$cfcRBDifference' != 0
#message FAIL field unsupported Limit
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedDegreeLimit-$cfcMRDegreeLimit;
#if `$cfcRBDifference' != 0
#message FAIL field unsupported DegreeLimit
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedOK-$cfcRBOK1;
#else
#$cfcRBDifference=$cfcRBExpectedOK-$cfcMROK;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field unsupported OK
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCost-$cfcRBCost1;
#else
#$cfcRBDifference=$cfcRBExpectedCost-$cfcMRCost;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field unsupported Cost
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMax-$cfcRBMax1;
#else
#$cfcRBDifference=$cfcRBExpectedMax-$cfcMRMax;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field unsupported Max
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCount-$cfcRBCount1;
#else
#$cfcRBDifference=$cfcRBExpectedCount-$cfcMRCount;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field unsupported Count
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedVars-$cfcRBVars1;
#else
#$cfcRBDifference=$cfcRBExpectedVars-$cfcMRVars;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field unsupported Vars
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcRBMeta1;
#else
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcMRMeta;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field unsupported Meta
#terminate 1
#endif
#if `$cfcMROK' != 0
#message FAIL eligibility unsupported
#terminate 1
#endif
#write "RBBOUND unsupported PASS"
Drop cfcRBTest,cfcRBIntern;
.sort
Skip;
Local cfcRBTest=cfcShare(999999)+cfs3;
Local cfcRBIntern=cfcPGCollect(cfcShare(999999)+cfs3);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcMRIndex?$cfcRBTestIndex)=1;
ModuleOption noparallel;
.sort
#define cfcRBId1 "`$cfcRBTestIndex'"
#$cfcRBTestTerms=termsin_(cfcRBTest);
Fill cfcMBTerms(`cfcRBId1')=`$cfcRBTestTerms';
Fill cfcMBLinks(`cfcRBId1')=cfcPGRef(999999);
#call cfcMRInspect(cfcRBTest)
#$cfcRBExpectedLimit=$cfcMRLimit;
#$cfcRBExpectedDegreeLimit=$cfcMRDegreeLimit;
#$cfcRBExpectedOK=$cfcMROK;
#$cfcRBExpectedCost=$cfcMRCost;
#$cfcRBExpectedMax=$cfcMRMax;
#$cfcRBExpectedCount=$cfcMRCount;
#$cfcRBExpectedVars=$cfcMRVars;
#$cfcRBExpectedMeta=$cfcMRMeta;
#call cfcMRInspectBatch(1)
#if `$cfcRBReady1' != 1
#message FAIL readiness ready-child
#terminate 1
#endif
#call cfcMRUseBatch(cfcRBTest,1)
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedLimit-$cfcMRLimit;
#if `$cfcRBDifference' != 0
#message FAIL field ready-child Limit
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedDegreeLimit-$cfcMRDegreeLimit;
#if `$cfcRBDifference' != 0
#message FAIL field ready-child DegreeLimit
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedOK-$cfcRBOK1;
#else
#$cfcRBDifference=$cfcRBExpectedOK-$cfcMROK;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field ready-child OK
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCost-$cfcRBCost1;
#else
#$cfcRBDifference=$cfcRBExpectedCost-$cfcMRCost;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field ready-child Cost
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMax-$cfcRBMax1;
#else
#$cfcRBDifference=$cfcRBExpectedMax-$cfcMRMax;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field ready-child Max
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCount-$cfcRBCount1;
#else
#$cfcRBDifference=$cfcRBExpectedCount-$cfcMRCount;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field ready-child Count
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedVars-$cfcRBVars1;
#else
#$cfcRBDifference=$cfcRBExpectedVars-$cfcMRVars;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field ready-child Vars
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcRBMeta1;
#else
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcMRMeta;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field ready-child Meta
#terminate 1
#endif
#if `$cfcMROK' != 1
#message FAIL eligibility ready-child
#terminate 1
#endif
#write "RBBOUND ready-child PASS"
Drop cfcRBTest,cfcRBIntern;
.sort
Skip;
Local cfcRBTest=cfcShare(999998)+cfs3;
Local cfcRBIntern=cfcPGCollect(cfcShare(999998)+cfs3);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcMRIndex?$cfcRBTestIndex)=1;
ModuleOption noparallel;
.sort
#define cfcRBId1 "`$cfcRBTestIndex'"
#$cfcRBTestTerms=termsin_(cfcRBTest);
Fill cfcMBTerms(`cfcRBId1')=`$cfcRBTestTerms';
Fill cfcMBLinks(`cfcRBId1')=cfcPGRef(999998);
#call cfcMRInspect(cfcRBTest)
#$cfcRBExpectedLimit=$cfcMRLimit;
#$cfcRBExpectedDegreeLimit=$cfcMRDegreeLimit;
#$cfcRBExpectedOK=$cfcMROK;
#$cfcRBExpectedCost=$cfcMRCost;
#$cfcRBExpectedMax=$cfcMRMax;
#$cfcRBExpectedCount=$cfcMRCount;
#$cfcRBExpectedVars=$cfcMRVars;
#$cfcRBExpectedMeta=$cfcMRMeta;
#call cfcMRInspectBatch(1)
#if `$cfcRBReady1' != 0
#message FAIL readiness forward-child
#terminate 1
#endif
#call cfcMRUseBatch(cfcRBTest,1)
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedLimit-$cfcMRLimit;
#if `$cfcRBDifference' != 0
#message FAIL field forward-child Limit
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedDegreeLimit-$cfcMRDegreeLimit;
#if `$cfcRBDifference' != 0
#message FAIL field forward-child DegreeLimit
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedOK-$cfcRBOK1;
#else
#$cfcRBDifference=$cfcRBExpectedOK-$cfcMROK;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field forward-child OK
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCost-$cfcRBCost1;
#else
#$cfcRBDifference=$cfcRBExpectedCost-$cfcMRCost;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field forward-child Cost
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMax-$cfcRBMax1;
#else
#$cfcRBDifference=$cfcRBExpectedMax-$cfcMRMax;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field forward-child Max
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCount-$cfcRBCount1;
#else
#$cfcRBDifference=$cfcRBExpectedCount-$cfcMRCount;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field forward-child Count
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedVars-$cfcRBVars1;
#else
#$cfcRBDifference=$cfcRBExpectedVars-$cfcMRVars;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field forward-child Vars
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcRBMeta1;
#else
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcMRMeta;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field forward-child Meta
#terminate 1
#endif
#if `$cfcMROK' != 0
#message FAIL eligibility forward-child
#terminate 1
#endif
#write "RBBOUND forward-child PASS"
Drop cfcRBTest,cfcRBIntern;
.sort
Skip;
Local cfcRBTest=cfs2^2+cfs3^2;
Local cfcRBIntern=cfcPGCollect(cfs2^2+cfs3^2);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcMRIndex?$cfcRBTestIndex)=1;
ModuleOption noparallel;
.sort
#define cfcRBId1 "`$cfcRBTestIndex'"
#call cfcMRInspect(cfcRBTest)
#$cfcRBExpectedLimit=$cfcMRLimit;
#$cfcRBExpectedDegreeLimit=$cfcMRDegreeLimit;
#$cfcRBExpectedOK=$cfcMROK;
#$cfcRBExpectedCost=$cfcMRCost;
#$cfcRBExpectedMax=$cfcMRMax;
#$cfcRBExpectedCount=$cfcMRCount;
#$cfcRBExpectedVars=$cfcMRVars;
#$cfcRBExpectedMeta=$cfcMRMeta;
#call cfcMRInspectBatch(1)
#if `$cfcRBReady1' != 0
#message FAIL readiness missing-metadata
#terminate 1
#endif
#call cfcMRUseBatch(cfcRBTest,1)
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedLimit-$cfcMRLimit;
#if `$cfcRBDifference' != 0
#message FAIL field missing-metadata Limit
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedDegreeLimit-$cfcMRDegreeLimit;
#if `$cfcRBDifference' != 0
#message FAIL field missing-metadata DegreeLimit
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedOK-$cfcRBOK1;
#else
#$cfcRBDifference=$cfcRBExpectedOK-$cfcMROK;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field missing-metadata OK
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCost-$cfcRBCost1;
#else
#$cfcRBDifference=$cfcRBExpectedCost-$cfcMRCost;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field missing-metadata Cost
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMax-$cfcRBMax1;
#else
#$cfcRBDifference=$cfcRBExpectedMax-$cfcMRMax;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field missing-metadata Max
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCount-$cfcRBCount1;
#else
#$cfcRBDifference=$cfcRBExpectedCount-$cfcMRCount;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field missing-metadata Count
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedVars-$cfcRBVars1;
#else
#$cfcRBDifference=$cfcRBExpectedVars-$cfcMRVars;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field missing-metadata Vars
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcRBMeta1;
#else
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcMRMeta;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field missing-metadata Meta
#terminate 1
#endif
#if `$cfcMROK' != 1
#message FAIL eligibility missing-metadata
#terminate 1
#endif
#write "RBBOUND missing-metadata PASS"
Drop cfcRBTest,cfcRBIntern;
.sort
Skip;
Local cfcRBTest=cfs2^5+cfs3;
Local cfcRBIntern=cfcPGCollect(cfs2^5+cfs3);
ArgToExtraSymbol tonumber,cfcPGCollect;
id cfcPGCollect(cfcMRIndex?$cfcRBTestIndex)=1;
ModuleOption noparallel;
.sort
#define cfcRBId1 "`$cfcRBTestIndex'"
#$cfcRBTestTerms=termsin_(cfcRBTest);
Fill cfcMBTerms(`cfcRBId1')=`$cfcRBTestTerms';
Fill cfcMBLinks(`cfcRBId1')=0;
#define cfcCGComplete "1"
#call cfcMRInspect(cfcRBTest)
#$cfcRBExpectedLimit=$cfcMRLimit;
#$cfcRBExpectedDegreeLimit=$cfcMRDegreeLimit;
#$cfcRBExpectedOK=$cfcMROK;
#$cfcRBExpectedCost=$cfcMRCost;
#$cfcRBExpectedMax=$cfcMRMax;
#$cfcRBExpectedCount=$cfcMRCount;
#$cfcRBExpectedVars=$cfcMRVars;
#$cfcRBExpectedMeta=$cfcMRMeta;
#call cfcMRInspectBatch(1)
#if `$cfcRBReady1' != 0
#message FAIL readiness complete-mode
#terminate 1
#endif
#call cfcMRUseBatch(cfcRBTest,1)
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedLimit-$cfcMRLimit;
#if `$cfcRBDifference' != 0
#message FAIL field complete-mode Limit
#terminate 1
#endif
#$cfcRBDifference=$cfcRBExpectedDegreeLimit-$cfcMRDegreeLimit;
#if `$cfcRBDifference' != 0
#message FAIL field complete-mode DegreeLimit
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedOK-$cfcRBOK1;
#else
#$cfcRBDifference=$cfcRBExpectedOK-$cfcMROK;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field complete-mode OK
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCost-$cfcRBCost1;
#else
#$cfcRBDifference=$cfcRBExpectedCost-$cfcMRCost;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field complete-mode Cost
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMax-$cfcRBMax1;
#else
#$cfcRBDifference=$cfcRBExpectedMax-$cfcMRMax;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field complete-mode Max
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedCount-$cfcRBCount1;
#else
#$cfcRBDifference=$cfcRBExpectedCount-$cfcMRCount;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field complete-mode Count
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedVars-$cfcRBVars1;
#else
#$cfcRBDifference=$cfcRBExpectedVars-$cfcMRVars;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field complete-mode Vars
#terminate 1
#endif
#if `$cfcRBReady1' == 1
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcRBMeta1;
#else
#$cfcRBDifference=$cfcRBExpectedMeta-$cfcMRMeta;
#endif
#if `$cfcRBDifference' != 0
#message FAIL field complete-mode Meta
#terminate 1
#endif
#if `$cfcMROK' != 1
#message FAIL eligibility complete-mode
#terminate 1
#endif
#write "RBBOUND complete-mode PASS"
Drop cfcRBTest,cfcRBIntern;
.sort
#undefine cfcCGComplete
#$cfcRBDifference=$cfcMRMeta-$cfcRBExpectedMeta;
#if `$cfcRBDifference' != 0
#message FAIL metadata application
#terminate 1
#endif
#write "RBBOUND COMPLETE 16"
.end
