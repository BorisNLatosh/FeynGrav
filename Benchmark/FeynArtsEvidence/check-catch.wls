Print["ONE=",InputForm[Catch[Throw[Failure["Toy",<|"MessageTemplate"->"x"|>]],_,Function[{value,tag},value]]]];
Print["TWO=",InputForm[CheckAbort[Catch[Apply[Function[{expr},Throw[Failure["Toy",<|"MessageTemplate"->"x"|>]]],{aa}],_,Function[{value,tag},value]],Failure["Aborted",<||>]]]];
Quit[0];
