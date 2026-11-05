"""Serialize independent canonical scalar variables for every stored free slot."""
import sys
from pathlib import Path
root=Path(sys.argv[1]);n=int(sys.argv[2]);out=Path(sys.argv[3])
sys.path.insert(0,str(root/'FeynArtsIntegration/Verification/QuadraticGravity/Interactions'))
from check_vertices import read_rules
sectors,_=read_rules(root/'Libs'/f'QuadraticGravityVertex_{n-2}',n)
with out.open('x') as f:
    f.write('{')
    for i,p in enumerate(sectors):
        if i:f.write(',\n')
        for j,(mon,c) in enumerate(p.items()):
            if j and c>0:f.write('+')
            f.write(str(c)+''.join(f'*qgz{a}x{b}' for a,b in mon)+' ')
    f.write('}\n')
