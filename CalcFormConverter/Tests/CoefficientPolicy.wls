$FeynCalcStartupMessages=False;
root=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{root,"CalcFormConverter.wl"}]];
checks=<||>;
assert[label_,value_]:=(AssociateTo[checks,label->TrueQ[value]];If[!TrueQ[value],Print["FAIL\t",label]]);
Begin["CalcFormConverter`Private`"];
policyScalars=<|"Global`a"->"cfs1","Global`b"->"cfs2"|>;
policyCompile[x_]:=Module[{tag=Unique[]},Catch[coefficientPolynomial[encode[x],policyScalars,tag],tag]];
Global`assert["polynomial exact syntax",policyCompile[Global`a+Global`b]==="(cfs1+cfs2)"];
Global`assert["rational exact syntax",policyCompile[2/3]==="(2/3)"];
Do[Global`assert["power boundary "<>ToString[n],(policyCompile[Global`a^n]=!=$Failed)===Between[n,{0,32}]],{n,{-1,0,31,32,33}}];
Do[Global`assert["protected polynomial "<>ToString[j],policyCompile[{
 Sqrt[Global`a],I,Global`c,Pair[LorentzIndex[Global`mu,D],LorentzIndex[Global`nu,D]],
 Pair[Momentum[Global`p,D],Momentum[Global`q,D]],Pair[Momentum[Global`p,D],LorentzIndex[Global`mu,D]]}[[j]]]===$Failed],{j,6}];
(* Construct already validated encoded powers without Wolfram evaluating them. *)
Do[rows=coefficientInverseRules[{<|"Name"->"cfa1","Kind"->"Abbreviation",
 "Expression"->{"Power",{"Symbol","Global`a"},{"Integer",ToString[n]}}|>},policyScalars,"rat"];
 Global`assert["inverse boundary "<>ToString[n],FreeQ[rows,$Failed]===Between[n,{-8,-1}]],{n,{-9,-8,-7,-1,0,1}}];
Global`assert["domain dependent eligibility",coefficientInverseRules[{<|"Name"->"cfa1","Kind"->"Abbreviation","Expression"->{"Power",{"Symbol","Global`a"},{"Integer","-1"}}|>},<||>,"rat"]==={$Failed}];
Global`assert["rational allowance",coefficientLimits["Master"]===<|"Terms"->1000,"Variables"->12,"Degree"->24|>];
Global`assert["factor allowance",coefficientLimits["Factor"]===<|"Terms"->250,"Variables"->8,"Degree"->12|>];
Global`assert["structural allowance",coefficientLimits["Polarisation"]===<|"Structures"->1024,"Terms"->10000,"RationalPasses"->1024|>];
Global`assert["exhausted allowance stays spent",Block[{$rcPolarisationMaximumRationalPasses=-1},coefficientLimits["Polarisation"]["RationalPasses"]]===0];
(* Scalar and vector roles sharing a Wolfram symbol must remain distinct. *)
roleEntries={<|"Kind"->"Scalar","Name"->"cfs1","Expression"->{"Symbol","Global`p"}|>,
 <|"Kind"->"Vector","Name"->"cfv1","Expression"->{"Symbol","Global`p"}|>,
 <|"Kind"->"Vector","Name"->"cfv2","Expression"->encode[Polarization[Global`p,I]]|>,
 <|"Kind"->"Vector","Name"->"cfv3","Expression"->encode[Polarization[Global`p,-I]]|>,
 <|"Kind"->"Index","Name"->"cfi1","Expression"->{"Symbol","Global`mu"}|>};
roles=coefficientRoles[roleEntries,"x"];
Global`assert["role separation",roles["Scalars"]===<|"Global`p"->"cfs1"|>&&Length[roles["Pairs"]]===6&&FreeQ[roles,"cfi1"]];
Global`assert["polarisation labels distinct",Length[roles["PolarisationNames"]]===5&&MemberQ[roles["Pairs"],{"cfv2","cfv3"}]];
Global`assert["pair canonical order",roles["Pairs"]==={{"cfv1","cfv1"},{"cfv1","cfv2"},{"cfv1","cfv3"},{"cfv2","cfv2"},{"cfv2","cfv3"},{"cfv3","cfv3"}}];
Global`assert["empty mapping",coefficientRoles[{},"x"]["Variables"]==={}];
Global`assert["scalarisation negative control",!MemberQ[roles["Variables"],"cfi1"]];
End[];
passed=Count[Values[checks],True];failed=Count[Values[checks],False];
Export[FileNameJoin[{Directory[],"baseline-result.json"}],<|"Completed"->True,"Passed"->passed,"Failed"->failed,"Checks"->checks|>,"RawJSON"];
Print["SUMMARY\tpassed=",passed,"\tfailed=",failed];
Export[FileNameJoin[{Directory[],"test-completed"}],"CoefficientPolicy","Text"];
Quit[If[failed===0,0,1]];
