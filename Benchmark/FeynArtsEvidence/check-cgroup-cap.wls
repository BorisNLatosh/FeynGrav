(* Smoke test only: no FeynGrav, model, FORM or physics. *)
$HistoryLength=0;
line=StringTrim[Import["/proc/self/cgroup","Text"]];
path=Last[StringSplit[line,":"]];
If[!StringStartsQ[path,"/"],Print["NOT_CGROUP_V2=",line];Quit[2]];
max=StringTrim[Import["/sys/fs/cgroup"<>path<>"/memory.max","Text"]];
swap=StringTrim[Import["/sys/fs/cgroup"<>path<>"/memory.swap.max","Text"]];
oom=StringTrim[Import["/sys/fs/cgroup"<>path<>"/memory.oom.group","Text"]];
Print["KERNEL_CGROUP=",path," MEMORY_MAX=",max," SWAP_MAX=",swap," OOM_GROUP=",oom];
Quit[If[max==="805306368"&&swap==="0"&&oom==="1",0,3]];
