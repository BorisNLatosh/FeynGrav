"""Build a standalone FeynArts bundle in a fresh directory from verified libraries."""
from collections import defaultdict
from hashlib import sha256
import json
from pathlib import Path
import sys

PRIVATE = 'QuadraticGravityModel`Private`'
PARAM = 'QuadraticGravityModel`'
KAPPA = r'FeynGrav`\[Kappa]'


def factor_terms(rows):
    """Factor a sparse polynomial by ordered prefixes of its commuting atoms."""
    constant = 0
    groups = defaultdict(list)
    for weight, atoms in rows:
        if atoms:
            groups[atoms[0]].append((weight, atoms[1:]))
        else:
            constant += weight
    pieces = [str(constant)] if constant else []
    for atom, tails in sorted(groups.items()):
        tail = factor_terms(tails)
        pieces.append(atom if tail == '1' else f'{atom}*({tail})')
    return '+'.join(pieces).replace('+-', '-') or '0'


def native_atom(pair, n):
    a,b = pair
    idx = lambda i: ('a' if i<n else 'b')+str(i%n+1)
    mom = lambda i: 'p'+str(i% n+1)
    if b<2*n:
        return f'Global`FAMetricTensor[{idx(a)},{idx(b)}]'
    if a<2*n:
        return f'Global`FAFourVector[{mom(b)},{idx(a)}]'
    return f'Global`FAScalarProduct[{mom(a)},{mom(b)}]'


def graviton(n, polys):
    legs = ','.join(f'{{mom{i},li{2*i-1},li{2*i}}}' for i in range(1,n+1))
    field = ','.join(f's{i} T[j{i},mom{i},{{li{2*i-1},li{2*i}}}]' for i in range(1,n+1))
    types = ','.join(f's{i} T[j{i}]' for i in range(1,n+1))
    fields = ','.join(['T[1]']*n)
    patterns = ','.join(f'{{p{i}_,a{i}_,b{i}_}}' for i in range(1,n+1))
    vector,definitions=[],[]
    for sector,poly in enumerate(polys):
        # Bose symmetry is already proven for each complete mass sector. Keep
        # one symmetric component per sector during FeynArts permutation closure.
        vector.append(f'{PRIVATE}tensor[{n},{sector},{PRIVATE}legs[{legs}]]')
        pairs=sorted({p for mon in poly for p in mon})
        names={p:f'z{i}' for i,p in enumerate(pairs)}
        bindings=','.join(names[p]+'='+native_atom(p,n) for p in pairs)
        rows=[(w,tuple(names[p] for p in mon)) for mon,w in poly.items()]
        expr=factor_terms(rows)
        definitions.append(f'{PRIVATE}value[{n},{sector},{{{patterns}}}]:=With[{{{bindings}}},\n{expr}];')
    generic=f'AnalyticalCoupling[{field}]==G[1][{types}].{{'+','.join(vector)+'}'
    common=f'I*{KAPPA}^{n-2}'
    classes=f'C[{fields}]=={{{{{common}}},{{{common}/{PARAM}M0^2}},{{{common}/{PARAM}M2^2}}}}'
    return definitions,generic,classes


def unique_row(text,prefix):
    rows=[r for r in text.splitlines() if r.startswith(prefix)]
    if len(rows)!=1:
        raise ValueError(f'Expected one row starting with {prefix}')
    return rows[0]


def build(root,dest):
    verify=root/'FeynArtsIntegration/Verification/QuadraticGravity'
    sys.path.insert(0,str(verify/'Interactions'))
    from check_vertices import read_rules, relabel
    probe=verify/'Fixtures/QGProbe.gen'
    gr=root/'FeynArts/Models/GR'
    gen0=(gr/'GR.gen').read_text()
    mod0=(gr/'GR.mod').read_text()
    # Reuse the tested conventional GR Grassmann sector and wavefunctions.
    generic=unique_row(gen0,'M$GenericCouplings={').removesuffix('};')
    start=generic.index('AnalyticalCoupling[s1 T[j1,mom1,{li1,li2}],s2 U[')
    ghostgeneric=generic[start:]
    classes=unique_row(mod0,'M$CouplingMatrices={').removesuffix('};')
    ghostclasses=classes[classes.index('C[T[1],-U[1],U[1]]'):]
    if ghostgeneric.count('AnalyticalCoupling[')!=1 or ghostclasses.count('C[')!=1:
        raise ValueError('Expected TUU as the final GR family')
    prop=probe.read_text().split('M$GenericPropagators=',1)[1].split(' AnalyticalPropagator[External][s1 V',1)[0].rstrip()
    prop=prop.replace('Global`qgM2',PARAM+'M2').replace('Global`qgM0',PARAM+'M0')
    prop=prop.replace('Global`qgEpsilon','(2 FAGaugeXi[T[1]])')
    prop=prop.replace('s1 T[j1,mom,{li1,li2}]]==1','s1 T[j1,mom,{li1,li2}]]==Global`FAPolarizationTensor[T[j1],mom,li1,li2]')
    prop+= 'AnalyticalPropagator[External][s1 U[j1,mom,{li1}]]==1,\nAnalyticalPropagator[Internal][s1 U[j1,mom,{li1}->{li2}]]==-I Global`FAMetricTensor[li1,li2] FAPropagatorDenominator[mom,0]}];'
    definitions=[];genericrows=[];classrows=[];inventory={}
    sources=[probe, gr/'GR.gen',gr/'GR.mod',Path(__file__),verify/'Interactions/check_vertices.py']
    for n in (3,4):
        lib=root/'Libs'/f'QuadraticGravityVertex_{n-2}'
        polys,count=read_rules(lib,n);sources.append(lib)
        # A single Orderless component may only represent a Bose-symmetric tensor.
        for poly in polys:
            for j in range(n-1):
                swap=lambda i: i+1 if i%n==j else i-1 if i%n==j+1 else i
                if relabel(poly,swap)!=poly:
                    raise ValueError(f'{lib.name}: unequal adjacent legs {j+1} and {j+2}')
            for j in range(n):
                swap=lambda i: i+n if i==j else i-n if i==j+n else i
                if relabel(poly,swap)!=poly:
                    raise ValueError(f'{lib.name}: asymmetric tensor slot {j+1}')
        defs,gen,mod=graviton(n,polys)
        definitions+=defs;genericrows.append(gen);classrows.append(mod)
        inventory[str(n)]={'SourceTerms':count,'SectorMonomials':list(map(len,polys))}
    gen='''(* Generated from verified quadratic-gravity rules. The three symmetric
   components per vertex correspond to EH, 1/M0^2 and 1/M2^2. Materialise
   their native tensor polynomial only for concrete line momenta. *)
KinematicIndices[T]={Lorentz,Lorentz}; KinematicIndices[U]={Lorentz};
$FermionLines=True; P$NonCommuting=U; P$ChainBuildOrder={U};
Attributes[FAMetricTensor]=Attributes[FAScalarProduct]={Orderless};
'''+f'Attributes[{PRIVATE}legs]={{Orderless}};\n'+ '\n'.join(definitions)+'\n'
    gen+=f'{PRIVATE}tensor[n_Integer,s_Integer,{PRIVATE}legs[ls__List]] /; !FreeQ[{{ls}},_FourMomentum] := {PRIVATE}value[n,s,{{ls}}];\n'
    gen+='M$GenericPropagators='+prop+'\nM$GenericCouplings={'+',\n'.join(genericrows+[ghostgeneric])+'};\n'
    gen+='M$GenericCouplings=CloseKinematicVector/@M$GenericCouplings;\nM$TruncationRules={_Global`FAPolarizationTensor->1}; M$LastGenericRules={};\n'
    mod=unique_row(mod0,'M$ClassesDescription=')+'\nM$CouplingMatrices={'+',\n'.join(classrows+[ghostclasses])+'};\n'
    mod+='M$CouplingMatrices=CloseCouplingVector/@M$CouplingMatrices; M$LastModelRules={};\n'
    dest.mkdir(parents=True,exist_ok=False)
    for name,text in [('QuadraticGravity.gen',gen),('QuadraticGravity.mod',mod)]:
        (dest/name).write_text(text)
    digest=lambda p:sha256(p.read_bytes()).hexdigest()
    manifest={'Schema':1,'ModelName':'QuadraticGravity','Gauge':'linear de Donder; epsilon = 2 FAGaugeXi[T[1]]','FieldClasses':['T[1]','U[1]','-U[1]'],'InteractionFamilies':['TTT','TTTT','TUU'],'GravitonOrders':[1,2],'LorentzDimension':'Symbolic D','Inventory':inventory,'SourceSHA256':{(str(p.relative_to(root)) if p.is_relative_to(root) else 'FeynArtsIntegration/BuildQuadraticGravity.py'):digest(p) for p in sources},'ModelFileSHA256':{p.name:digest(p) for p in sorted(dest.iterdir())}}
    (dest/'manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')


if __name__=='__main__':
    if len(sys.argv)!=3: raise SystemExit('Usage: BuildQuadraticGravity.py PACKAGE_ROOT FRESH_MODEL_DIRECTORY')
    build(Path(sys.argv[1]).resolve(),Path(sys.argv[2]).resolve())
