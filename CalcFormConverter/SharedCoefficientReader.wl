(* ::Package:: *)
(* ::Title:: *)
(* Buffered, validated shared coefficient records *)
(* ::Text:: *)
(* Loaded in the private converter context. The stream and every dictionary
   belong to one import. New definitions may refer only to earlier definitions;
   no result text is executed as Wolfram source. Legacy dictionaries remain
   non-recursive. Buffering is independent of mathematical reconstruction. *)

SetAttributes[srWithReader,HoldAll];
srWithReader[stream_,body_] := Block[{$srBuffer="",$srPosition=1,
    $srPositions=<||>,$srCursors=<||>,$srSeen=<||>,$srFramed=False},body];
srFlush[stream_] := Module[{n=StringLength[$srBuffer]-$srPosition+1},
    If[n>0,SetStreamPosition[stream,StreamPosition[stream]-n]];
    $srBuffer="";$srPosition=1;$srPositions=<||>;$srCursors=<||>];
srReadUntil[stream_,delimiter_String,limit_:Infinity,allowEnd_:False] := Module[
    {parts={},size=0,positions,index,pos,fragment,remaining},
    While[True,
        If[$srPosition>StringLength[$srBuffer],
            $srBuffer=Quiet[Check[FromCharacterCode[BinaryReadList[stream,"UnsignedInteger8",Min[$pgReadChunkSize,16384]]],$Failed]];
            If[!StringQ[$srBuffer]||$srBuffer==="",
                If[TrueQ[allowEnd]&&size>0,$srPosition=1;Return[StringJoin[parts]]];
                throwFailure["InvalidResult","Truncated shared coefficient section."]];
            $srPosition=1;$srPositions=<||>;$srCursors=<||>];
        positions=Lookup[$srPositions,delimiter,None];
        If[positions===None,positions=First/@StringPosition[$srBuffer,delimiter];AssociateTo[$srPositions,delimiter->positions]];
        index=Lookup[$srCursors,delimiter,1];
        While[index<=Length[positions]&&positions[[index]]<$srPosition,index++];
        If[index<=Length[positions],
            pos=positions[[index]];size+=pos-$srPosition;
            If[size>limit,throwFailure["InvalidResult","Invalid shared coefficient header."]];
            fragment=If[pos===$srPosition,"",StringTake[$srBuffer,{$srPosition,pos-1}]];
            $srPosition=pos+1;AssociateTo[$srCursors,delimiter->index+1];
            Return[StringJoin[Append[parts,fragment]]]];
        remaining=StringLength[$srBuffer]-$srPosition+1;size+=remaining;
        If[size>limit,throwFailure["InvalidResult","Invalid shared coefficient header."]];
        If[remaining>0,AppendTo[parts,StringDrop[$srBuffer,$srPosition-1]]];
        $srPosition=StringLength[$srBuffer]+1
    ]
];
srLine[stream_] := Module[{line},
    Do[line=StringTrim[srReadUntil[stream,"\n",128,True]];If[line=!="",Break[]],{4}];
    If[line=!="",line,throwFailure["InvalidResult","Excessive blank padding in shared coefficient records."]]];
srCount[line_,prefix_,stream_] := Module[{count},
    If[!StringMatchQ[line,RegularExpression[prefix<>" (0|[1-9][0-9]*)"]],
        throwFailure["InvalidResult","Invalid shared coefficient count header."]];
    count=FromDigits[StringDrop[line,StringLength[prefix]+1]];
    If[count>FileByteCount[First[stream]],throwFailure["InvalidResult","Invalid shared coefficient count."]];count
];

(* ::Section:: *)
(* Definition validation and bounded batching *)
srDefinition[text_,tokens_,values_,dim_,allow_,predecoded_] := Module[{cut,lhs,index,value,q},
    cut=StringPosition[text,"=",1];
    If[cut==={},throwFailure["InvalidResult","Invalid shared coefficient definition."]];
    lhs=StringTrim[StringTake[text,cut[[1,1]]-1]];
    If[!StringMatchQ[lhs,RegularExpression["cfcShare\\([1-9][0-9]*\\)"]],throwFailure["InvalidResult","Invalid shared coefficient identifier."]];
    index=FromDigits[StringTake[lhs,{10,-2}]];
    If[KeyExistsQ[$srSeen,index],throwFailure["InvalidResult","Duplicate shared coefficient definition."]];
    value=If[predecoded===$Failed,pgParseNestedChecked[normaliseFORMNumbers[StringDrop[text,cut[[1,2]]]],Function[t,parseResult[t,tokens,dim]]],predecoded];
    q=Lookup[$tdImport,"LoopMomentum",None];
    (* Earlier references already denote validated values. Inspect the compact
       expression before replacing them; rescanning all expanded subtrees here
       would discard the benefit of sharing. *)
    If[!FreeQ[value,_tdToken|_lrToken] || (!allow&&!FreeQ[value,_lrSharedToken]) ||
        (!TrueQ[Lookup[$tdImport,"ExistingCoefficients",False]] &&
         (!FreeQ[value,Momentum[v_,___]/;!FreeQ[v,q]] ||
          AnyTrue[Cases[value,pgDen[n_]:>n,{0,Infinity}],MemberQ[$tdImport["LoopDenominators"],#]&])),
        throwFailure["InvalidResult","A shared coefficient contains an integral or undecomposed loop dependence."]];
    If[allow,value=value/.lrSharedToken[k_Integer]:>$lrSharedValues[k]];
    value=value/.pgDen[n_String]:>values[n];
    AssociateTo[$srSeen,index->True];
    If[allow,AssociateTo[$lrSharedValues,index->value]];
    index->value
];
srDefinitions[stream_,count_,tokens_,values_,dim_,allow_] := Module[
    {left=count,raw,bytes,pending,record,rights,decoded,rows,pair},
    rows=Reap[While[left>0,
        raw={};bytes=0;pending=None;
        While[Length[raw]<Min[64,left]&&bytes<262144,
            record=Catch[StringTrim[srReadUntil[stream,";"]],$failureTag];
            If[FailureQ[record],pending=record;Break[]];
            AppendTo[raw,record];bytes+=StringLength[record]];
        rights=Map[Function[row,With[{p=StringPosition[row,"=",1]},If[p==={},"",StringDrop[row,p[[1,2]]]]]],raw];
        decoded=If[allow&&TrueQ[$pgNestedParsing]&&TrueQ[$importParserActive]&&raw=!={},
            Catch[srBatchScalars[rights],$failureTag],$Failed];
        If[!ListQ[decoded],decoded=$Failed];
        Do[pair=srDefinition[raw[[j]],tokens,values,dim,allow,If[decoded===$Failed,$Failed,decoded[[j]]]];
            If[!allow,Sow[pair]],{j,Length[raw]}];
        left-=Length[raw];
        (* Earlier syntax errors take precedence over a later truncated read. *)
        If[FailureQ[pending],Throw[pending,$failureTag]]
    ]][[2]];
    If[allow,Null,If[rows==={},<||>,Association[First[rows]]]]
];

lrReadShared[stream_,mapping_,tokens_,values_,dim_] := Module[{meta,count,line,allow,old},
    meta=Lookup[mapping,"LoopCoefficientReduction",<||>];
    If[!TrueQ[Lookup[meta,"SharedCoefficientBlocks",False]],Return[Null]];
    allow=TrueQ[Lookup[meta,"IncrementalSharedDefinitions",False]];
    count=srCount[srLine[stream],"CFC_SHARED",stream];
    $lrSharedValues=If[allow,<||>,None];
    old=srDefinitions[stream,count,tokens,values,dim,allow];
    If[!allow,$lrSharedValues=old];
    line=srLine[stream];
    If[line==="CFC_GROUP_BODY"&&allow,$srFramed=True,
        If[line=!="CFC_BODY",throwFailure["InvalidResult","Missing shared coefficient body marker."]]]
];
srReadBody[stream_,consume_,tokens_,values_,dim_] := Module[{line,count,text,tail},
    If[!TrueQ[$srFramed],srFlush[stream];Return[pgReadSummands[stream,consume]]];
    While[True,
        line=srLine[stream];
        If[line==="CFC_END",Break[]];
        count=srCount[line,"CFC_ADD_SHARED",stream];
        srDefinitions[stream,count,tokens,values,dim,True];
        If[srLine[stream]=!="CFC_GROUP",throwFailure["InvalidResult","Missing integral group marker."]];
        text=StringTrim[srReadUntil[stream,";"]];
        If[text==="",throwFailure["InvalidResult","Empty integral group."]];
        consume[text];text=""
    ];
    (* The terminator is mandatory; no expression or second stream may follow. *)
    tail=StringDrop[$srBuffer,$srPosition-1];
    If[StringTrim[tail]=!="",throwFailure["InvalidResult","Unexpected data after the shared result terminator."]];
    While[True,tail=FromCharacterCode[BinaryReadList[stream,"UnsignedInteger8",Min[$pgReadChunkSize,16384]]];
        If[tail==="",Break[]];If[StringTrim[tail]=!="",throwFailure["InvalidResult","Unexpected data after the shared result terminator."]]]
];

(* ::Section:: *)
(* Literal fast paths retain the general parser's typed validators *)
srFastFactor[t_String] := Which[
    StringMatchQ[t,RegularExpression["cfcShare\\(\\s*[0-9]+\\s*\\)(?:\\s*\\^\\s*[0-9]+)?"]],
    With[{n=FromDigits/@StringCases[t,DigitCharacter..]},lrSharedCall[{First[n]}]^If[Length[n]===2,Last[n],1]],
    StringMatchQ[t,RegularExpression["cfcLRDen\\(\\s*cfcShare\\(\\s*[0-9]+\\s*\\)\\s*\\)(?:\\s*\\^\\s*[0-9]+)?"]],
    With[{n=FromDigits/@StringCases[t,DigitCharacter..]},ldgScalarCall[{lrSharedCall[{First[n]}]},True]^If[Length[n]===2,Last[n],1]],
    True,$Failed
];

(* Batched trusted slot construction; rejected batches use the ordered parser. *)
srBatchScalars[texts_List] := Module[{text,pattern,tokens,operators,distinct,codes,rows,decoded,generated,held,substitution,operands,value},
 text=StringRiffle[texts,";"];
 pattern="(?:cfcShare\\([0-9\\s]+\\)(?:\\s*\\^\\s*[0-9]+)?|cfcLR(?:Den|Num)\\(\\s*(?:cfcShare\\([0-9\\s]+\\)|[^()]*)\\s*\\)(?:\\s*\\^\\s*[0-9]+)?|cfcDimRat\\([^()]*\\)|"<>$flatFactorPattern<>")";
 tokens=StringCases[text,RegularExpression[pattern<>"|[+*/();\\-]"]];
 If[tokens==={}||StringReplace[StringJoin[tokens],WhitespaceCharacter->""]=!=StringReplace[text,WhitespaceCharacter->""],Return[$Failed]];
 operators=<|"+"->-1,"-"->-2,"*"->-3,"/"->-4,"("->-5,")"->-6,";"->-7|>;
 distinct=DeleteCases[DeleteDuplicates[tokens],"+"|"-"|"*"|"/"|"("|")"|";"];
 codes=Lookup[Join[AssociationThread[distinct,Range[Length[distinct]]],operators],tokens];
 rows=DeleteCases[SplitBy[codes,#===-7&],{-7..}];
 If[Length[rows]=!=Length[texts]||!AllTrue[rows,pgCoefficientSyntax[]],Return[$Failed]];
 decoded=importFactor/@distinct;
 If[!FreeQ[decoded,_vectorToken|_indexToken|_caAToken|_caFToken],Return[$Failed]];
 generated="{"<>StringJoin[Lookup[Join[AssociationThread[Range[Length[distinct]],("#"<>IntegerString[#]&)/@Range[Length[distinct]]],<|-1->" + ",-2->" - ",-3->"*",-4->"/",-5->"(",-6->")",-7->","|>],codes]]<>"}";
 held=ToExpression[generated,InputForm,HoldComplete];
 substitution=Dispatch[Thread[(Slot/@Range[Length[distinct]])->decoded]];
 operands=Cases[held,HoldPattern[Power[x_,-1]]:>HoldComplete[x],Infinity];
 If[AnyTrue[operands,ReleaseHold[#/.substitution]===0&],Return[$Failed]];
 value=ReleaseHold[held/.substitution];
 If[ListQ[value]&&Length[value]===Length[texts],value,$Failed]
];
