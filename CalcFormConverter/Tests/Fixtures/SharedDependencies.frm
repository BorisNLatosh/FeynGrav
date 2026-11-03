Off Statistics;
Symbols D,cfcDGNum;
CFunctions f,cfcRef,cfcShare,cfcPolyN,cfcPolyD,cfcNum,cfcNumPart;
Local seed=f(D-1);
ArgToExtraSymbol tonumber,f;
.sort
Drop seed;
.sort
Local seed=f(2*cfcShare(1)+3);
ArgToExtraSymbol tonumber,f;
.sort
Drop seed;
.sort
Local seed=f(cfcShare(1)+cfcShare(2));
ArgToExtraSymbol tonumber,f;
.sort
Drop seed;
.sort
Local seed=f(cfcShare(2)+cfcShare(3));
ArgToExtraSymbol tonumber,f;
.sort
Drop seed;
.sort
Local cfcRefs=cfcRef(1)+cfcRef(3)+cfcRef(4);
.sort
@DEPENDENCY_CLOSURE@
NSkip;
Hide cfcRefs;
Local residual=cfcRefs-cfcRef(1)-cfcRef(2)-cfcRef(3)-cfcRef(4);
.sort
#$bad=termsin_(residual);
#write "SET_RESIDUAL %$",$bad
Drop residual;
.sort
#do key=cfcRefs
Local def=`key';
id cfcRef(cfcDGNum?$index)=extrasymbol_(cfcDGNum);
.sort
#write "DEFINITION %$ %E",$index,def
Drop def;
.sort
#enddo
.end
