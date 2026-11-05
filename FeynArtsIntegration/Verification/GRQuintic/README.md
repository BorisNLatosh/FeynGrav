# Native five-graviton GR tree

The opt-in GR model uses one symmetric five-graviton component. The shipped off-shell source contains 76,230 monomials. There are 722 distinct orbits under permutations of the five whole legs. The seed weight is the coefficient of one monomial divided by its stabiliser size. A sum over all 120 permutations then reproduces the original coefficient once for every distinct monomial. The native tensor remains inert during FeynArts permutation closure and expands its seed when a diagram supplies concrete line momenta. The ten tensor slots remain distinct.

From the repository root, regenerate the template in a fresh file with

```sh
python3 FeynArtsIntegration/Verification/GRQuintic/GenerateSeed.py \
    Libs/GravitonVertex_3 /tmp/new-GRQuinticTensor.gen.in
cmp FeynArtsIntegration/GRQuinticTensor.gen.in /tmp/new-GRQuinticTensor.gen.in
```

The generator reads the signed rational coefficients, checks symmetry under interchange of each graviton's index pair, partitions the monomials into whole-leg orbits and reconstructs the full coefficient dictionary. New or modified libraries require their own verification before updating the source and template hashes. Building and using the supplied model requires no Python process.

## Fresh-kernel checks

Use the normal external resource wrapper. These scripts do not enforce memory or wall-time limits. Set `GR5_OUTPUT` to a fresh existing output directory for every run. `GR5_PACKAGE` can specify the checkout. The build scripts use the public FeynGrav interface, including its library import state. Use separate kernels when changing models.

1. Run `CheckBuilder.wls` with `GR5_ORDER=3` and `GR5_MODEL` pointing to a new model directory under an existing parent. This verifies the compact basis, manifest, source-selection guards and no-overwrite behaviour. In another kernel, use order 2 and a different destination to check byte equality with the shipped model.
2. Run `Check.wls` with `GR5_SUITE=Contact` and `GR5_MODEL` pointing to the order-three bundle. It initialises the model with FeynArts and FeynCalc alone, checks 26 insertions for the 26 topologies and constructs only the single contact. It exports the canonical free-slot polynomial as `contact.wxf`. Indices and five incoming momenta are independent, with symbolic D.
3. Set `GR5_NATIVE` to that WXF file and run `GR5_SUITE=Compare` in a fresh kernel. This independently reads the stored Wolfram expression, compares every coefficient with the native result. The sign and altered-coefficient controls must differ.
4. Run `GR5_SUITE=FORM` in another fresh kernel using the same `GR5_NATIVE` and `GR5_MODEL`. It times `FCFAConvert` on the complete contact and `FeynGravSimplify` on its contraction with the five symmetric external probes made from vectors A, B, A, B and A. It compares this polynomial exactly and rejects the opposite sign.

## Complete physical tree

Use the same order-three model and resource wrapper. Each command below is a separate bounded job. Set `GR5_MODEL` to the model directory. For each export set `GR5_OUTPUT` to its own fresh, empty directory.

1. Run `WolframKernel -noinit -script ExportTree.wls` with `GR5_EXPORT=Templates`. It constructs the native quintic and one cubic–quartic and one three-cubic exchange. It saves the contact coefficients, individual exchange factors, and all 26 labelled topology signatures. Momenta are all incoming, the signature is +---, and the truncated tree uses FeynArts prefactor one. No extra symmetry or Wick factors are applied.
2. Run `python3 PhysicalTree.py TEMPLATES amplitude amplitude.json` for the complete amplitude from that export. The five input momenta and symmetric polarisation dyads appear in the output. All reported five-point values have the common factor `i κ³` removed. `TEMPLATES` denotes the directory from step 1. Use a fresh output filename on every call.
3. For the independent checks, run `ExportTree.wls` again with `GR5_EXPORT=Controls` and a different empty output directory. Compare the two `graphs.json` inventories and copy only `graph-3.json` and `graph-15.json` to `TEMPLATES`. Run `PhysicalTree.py TEMPLATES physical physical.json`. It compares the two extra native graphs with simultaneous leg relabellings, checks **every** exchange against separate Cartesian sewing of the stored GR cubic and quartic vertices, and reproduces the earlier two-probe checks.
4. Run `PhysicalTree.py TEMPLATES ward ward.json`. With the other gravitons transverse and traceless, replace either symmetric slot of each tested graviton by its momentum and use each of the four coordinate basis vectors in the other slot. All 20 complete residuals vanish. All 20 become nonzero if the contact is omitted.
5. Run `PhysicalTree.py TEMPLATES soft soft.json`. At zero soft momentum the four-point reference consists of the quartic plus all three exchanges. Along the exact massless, conserving path in `SoftLimit.py`, only graphs with a soft–hard propagator have a pole. For each such line the residue uses `1/(2 p·q)`, the coefficient obtained by multiplying the original propagator by the soft parameter and taking its exact limit. Compare all four emitter residues separately with the leading soft theorem and test three finite values of the parameter.

In the recorded fixture the five all-incoming momenta are (6,0,0,6), (6,0,0,−6), (−3,−3,0,0), (−4,0,−4,0), (−5,3,4,0). For the unnormalised transverse, traceless states returned in the JSON, the full amplitude divided by i κ³ is −5,741,358. The four-point hard amplitude divided by i κ² in the soft fixture is 25,704 and the leading five-point residue divided by i κ³ is 308,448.

The component checks use the native exchange factors with crossed momenta and polarisations together, the explicit signs of internal Lorentz contractions, and exact rational arithmetic. They evaluate a physical projection at a specified four-dimensional point. The full free-index polynomial at general kinematics has not been constructed. The set of graph numbers is tied to the recorded FeynArts topology ordering. A changed ordering or field/line structure must first pass the cut and weight checks.
