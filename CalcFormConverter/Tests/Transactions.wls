Get[FileNameJoin[{DirectoryName[$InputFileName],"Common.wl"}]];
transactionRoot=CreateDirectory[FileNameJoin[{testDirectory,"transactions"}]];
old=CalcFormExport[x+1,FileNameJoin[{transactionRoot,"job.frm"}]];
original=Import[#,"Text"]& /@ Lookup[old,{"InputFile","MappingFile"}];
pairIntact[] := (Import[#,"Text"]& /@ Lookup[old,{"InputFile","MappingFile"}])===original;
noDebris[] := Sort[FileNames["*",transactionRoot]]===Sort[Lookup[old,{"InputFile","MappingFile"}]];

(* Stop at the first failed operation, before replacing any target. *)
writeCount=0; renameCount=0;
failure=Block[{CalcFormConverter`Private`exportWriteText,CalcFormConverter`Private`exportRenameFile},
 CalcFormConverter`Private`exportWriteText[path_,text_]:=(writeCount++;$Failed);
 CalcFormConverter`Private`exportRenameFile[___]:=(renameCount++;$Failed);
 CalcFormExport[y+2,old["InputFile"],OverwriteTarget->True]];
assertFailure["first staging write fails","WriteFailed",failure];
assert["staging stops immediately",writeCount===1 && renameCount===0];
assert["staging failure preserves original",pairIntact[] && noDebris[]];

renameCount=0;
failure=Block[{CalcFormConverter`Private`exportRenameFile},
 CalcFormConverter`Private`exportRenameFile[source_,target_,overwrite_]:=(renameCount++;
 If[renameCount===2,$Failed,RenameFile[source,target,OverwriteTarget->overwrite]]);
 CalcFormExport[y+2,old["InputFile"],OverwriteTarget->True]];
assertFailure["second replacement fails","WriteFailed",failure];
assert["rollback restores original pair",pairIntact[] && noDebris[]];

renameCount=0;
aborted=CheckAbort[Block[{CalcFormConverter`Private`exportRenameFile},
 CalcFormConverter`Private`exportRenameFile[source_,target_,overwrite_]:=Module[{result},
 renameCount++;result=RenameFile[source,target,OverwriteTarget->overwrite];If[renameCount===1,Abort[]];result];
 CalcFormExport[y+2,old["InputFile"],OverwriteTarget->True]],"aborted"];
assert["replacement abort delivered",aborted==="aborted"];
assert["abort restores original pair",pairIntact[] && noDebris[]];

newRoot=CreateDirectory[FileNameJoin[{testDirectory,"new-transaction"}]];
renameCount=0;
failure=Block[{CalcFormConverter`Private`exportRenameFile},
 CalcFormConverter`Private`exportRenameFile[source_,target_,overwrite_]:=(renameCount++;
 If[renameCount===2,$Failed,RenameFile[source,target,OverwriteTarget->overwrite]]);
 CalcFormExport[y,FileNameJoin[{newRoot,"new.frm"}]]];
assertFailure["new export second replacement failure","WriteFailed",failure];
assert["partial new export removed",FileNames["*",newRoot]==={}];

(* Recovery failure must preserve originals as backup files, not discard them. *)
renameCount=0;
failure=Block[{CalcFormConverter`Private`exportRenameFile,CalcFormConverter`Private`exportCopyFile},
 CalcFormConverter`Private`exportRenameFile[source_,target_,overwrite_]:=(renameCount++;
 If[renameCount===2,$Failed,RenameFile[source,target,OverwriteTarget->overwrite]]);
 CalcFormConverter`Private`exportCopyFile[source_,target_,overwrite_:False]:=
 If[StringEndsQ[source,".bak"],$Failed,CopyFile[source,target,OverwriteTarget->overwrite]];
 CalcFormExport[y+2,old["InputFile"],OverwriteTarget->True]];
assertFailure["rollback failure reported","RollbackFailed",failure];
recovery=failure[[2]]["RecoveryFiles"];
assert["recovery copies retained",Length[recovery]===2 && AllTrue[recovery,FileExistsQ]];
assert["recovery preserves original bytes",Sort[Import[#,"Text"]& /@ recovery]===Sort[original]];
Scan[Function[path,CopyFile[path,First[Select[Lookup[old,{"InputFile","MappingFile"}],StringStartsQ[path,#]&]],OverwriteTarget->True];DeleteFile[path]],recovery];
assert["manual recovery possible",pairIntact[]];
updated=CalcFormExport[y+2,old["InputFile"],OverwriteTarget->True];
assert["successful replacement leaves no backups",AssociationQ[updated] && noDebris[] && !pairIntact[]];
finish["Export transactions"];
