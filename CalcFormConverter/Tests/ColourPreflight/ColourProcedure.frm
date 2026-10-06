* Independent algebraic checks of the unmodified upstream SUn procedure.
* Supply -D CFCSUNFILE=/absolute/path/SUn.prc when launching FORM or TFORM.
* The unchanged reference is in ThirdParty/FORMColour/SUn.prc.
#ifndef `CFCSUNFILE'
  #message Supply CFCSUNFILE pointing to the upstream SUn.prc.
  #terminate
#endif
Off Statistics;
Symbols a,nf,NF,NA,cF,cA,[cF-cA/6];
CFunction Tr(cyclic);
Tensor T,Tp,f(antisymmetric);
Indices i1=NF,i2=NF,i3=NF,i4=NF;
Indices j1=NA,j2=NA,j3=NA;
Indices testu=NF,testv=NF,testw=NF,testx=NF;
Indices testa=NA,testb=NA,testc=NA,testd=NA;
Dimension NF;
#include `CFCSUNFILE'
Local fundamentalLoop = d_(testu,testu)-NF;
Local adjointLoop = d_(testa,testa)-(NF^2-1);
Local fundamentalDelta = d_(testu,testv)*d_(testv,testw)-d_(testu,testw);
Local adjointDelta = d_(testa,testb)*d_(testb,testc)-d_(testa,testc);
Local oneTrace = Tr(testa);
Local twoTrace = Tr(testa,testb)-d_(testa,testb)/2;
Local casimir = T(testa,testa,testu,testv)-(NF^2-1)/(2*NF)*d_(testu,testv);
Local sandwich = T(testa,testb,testa,testu,testv)+T(testb,testu,testv)/(2*NF);
Local completeness = T(testa,testu,testv)*T(testa,testw,testx)
 -(d_(testu,testx)*d_(testv,testw)-d_(testu,testv)*d_(testw,testx)/NF)/2;
Local structureConstants = f(testa,testc,testd)*f(testb,testc,testd)-NF*d_(testa,testb);
* d(a,b,c) = 2 [Tr(a,b,c)+Tr(b,a,c)], at Tr(Ta Tb)=delta(a,b)/2.
Local symmetricConstants = 4*(Tr(testa,testc,testd)+Tr(testc,testa,testd))
 *(Tr(testb,testc,testd)+Tr(testc,testb,testd))-(NF^2-4)/NF*d_(testa,testb);
Local mixedConstants = 2*f(testa,testc,testd)*(Tr(testb,testc,testd)+Tr(testc,testb,testd));
Local traceCommutator = Tr(testa,testb,testc)-Tr(testb,testa,testc)-i_/2*f(testa,testb,testc);
Local chainJoin = T(testa,testu,testv)*T(testb,testv,testw)-T(testa,testb,testu,testw);
Local chainClosure = T(testa,testu,testv)*T(testb,testv,testu)-d_(testa,testb)/2;
Local traceProduct = Tr(testa,testb)*Tr(testa,testb)-(NF^2-1)/4;
Local tracePower = Tr(testa,testb)^2-(NF^2-1)/4;
Local cyclicTrace = Tr(testa,testb,testc,testd)-Tr(testc,testd,testa,testb);
* Observations rather than assertions: these require adapter/output-basis handling.
Local emptyTraceObservation = Tr();
Local threeTraceObservation = Tr(testa,testb,testc);
Local fourTraceObservation = Tr(testa,testb,testc,testd);
#call SUn
* FORM substitutions must cover inverse powers as well as positive powers.
id a^-1=2;
id nf^-1=1;
id a=1/2;
id nf=1;
id NA=NF^2-1;
.sort
* Every named residual below must vanish; observation rows are not assertions.
#do test = {fundamentalLoop,adjointLoop,fundamentalDelta,adjointDelta,oneTrace,twoTrace,casimir,sandwich,completeness,structureConstants,symmetricConstants,mixedConstants,traceCommutator,chainJoin,chainClosure,traceProduct,tracePower,cyclicTrace}
 #if `ZERO_`test'' == 0
  #message Nonzero colour identity residual: `test'
  #terminate
 #endif
#enddo
#create <ColourProcedure.out>
#do test = {fundamentalLoop,adjointLoop,fundamentalDelta,adjointDelta,oneTrace,twoTrace,casimir,sandwich,completeness,structureConstants,symmetricConstants,mixedConstants,traceCommutator,chainJoin,chainClosure,traceProduct,tracePower,cyclicTrace,emptyTraceObservation,threeTraceObservation,fourTraceObservation}
 #write <ColourProcedure.out> "`test'=%E",`test'
#enddo
#close <ColourProcedure.out>
.end
