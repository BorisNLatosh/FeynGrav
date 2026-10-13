Print["SIMPLE=",Catch[Throw[2]]];
Print["PATTERN=",Catch[Throw[3],_,Function[{value,tag},value]]];
Quit[0];
