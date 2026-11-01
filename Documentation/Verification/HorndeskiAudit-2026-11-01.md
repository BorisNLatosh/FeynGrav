# Horndeski G2/G3/G5 audit and corrections

**Later delivery:** [RULE-05 is now complete](HorndeskiLibraryMigration-2026-11-01.md), including all 14 G4 libraries and ordinary imports. The checkpoint below records the earlier source/audit delivery; its pending-library statements and JSON hashes describe that earlier state.

1 November 2026; FeynArts_Integration, based on `a7c2a9a`, uncommitted.

**RULE-03 and RULE-04 are accepted within the scope below.** The G2/G3/G5
part of RULE-05 is delivered. At the user's explicit instruction, G4 remains
under RULE-01 and its part of RULE-05 stays open. Its rejected candidate and
14 historical libraries are preserved. No model or notebook was changed.

## Mathematical target and corrections

Use the [Horndeski action](../Models/Horndeski.md#action): (+---), positive
Fourier phase, the fixed mixed-index Riemann convention and X without a half.
The relative Hessian terms are -2 G4,X H2 and +G5,X H3/3. The convention map,
including the chain rule in X, is documented on that page. A signature change
does not reverse the connection sign in the covariant Hessian.

G2/G3 agree with the independent action for the shipped generic vertices.
Their direct constructors nevertheless lost scalar differentiation multiplicities
when equal-valued external momenta were supplied: `Permutations[values]` removes
duplicates. G2/G3/G5 now permute labelled positions. The shared index helper
likewise retains all 2^n n! orientations/permutations when graviton blocks
coincide. Generic distinct-block output order is preserved.

External indices could also collide with fixed temporary indices. Public
constructors now share a fresh scalar-index list within a call; the G3/G5
tensor helpers use lexical local indices. Successful-cache behaviour, failure
propagation and cancellation are preserved. Private helpers called outside
the public construction boundary are not a new public interface.

Three further corrections are required for G5:

1. With the existing common I/2 Fourier prefactor, TII's multiplier is **-2b**,
   yielding the required +b/3 cubic-Hessian action coefficient. Multiplying
   the complete G5 vertex by -2 would also corrupt its Einstein contribution.
2. The Hessian contraction must be polarised symmetrically before replacing
   selected Hessians by their connection parts. For metric-self-adjoint A,B,C,

   $$
   K(A,B,C)=[A][B][C]-[A][BC]-[B][AC]-[C][AB]+[ABC]+[ACB].
   $$

   Thus the three negative trace terms remain separate in T5–T8. Replacing
   them by one term with coefficient -3 before choosing a connection slot
   omits required placements. The symmetric kernel justifies the existing
   1:3:3:1 connection-placement multiplicities.
3. T8's third graviton momentum contracts with its third connection index,
   Lambda3. Previously it reused Lambda2, leaving the third index unmatched.

## Independent reference and scope

The test reference expands inverse metric, metric density, connection, Ricci
and scalar Hessians directly in nilpotent labelled graviton variables. It does
not call production C/I/Gamma tensor helpers. Scalar differentiation uses
labelled Hessian selections and unordered kinetic matchings with their proved
combinatorial multiplicities. Primitive inverse/density/connection identities
are checked before full vertices.

Comparisons use generic off-shell momenta and symbolic D. For the stored
selections, unrestricted independent auxiliary vectors contract every free
index. Pair symmetry and multilinearity are tested separately. This determines
the complete tensor; it is not a numerical component sample or a helicity test.
Momentum conservation is imposed only for the explicitly stated total-derivative
identities, never to conceal a generic vertex discrepancy.

| Family | Independently checked scope | Stored result |
| --- | --- | --- |
| G2 | All 8 shipped (a,b) pairs, n = 1,2 | All 16 libraries unchanged |
| G3 | All 10 shipped pairs, n = 1,2 | All 20 libraries unchanged |
| G5 | All 5 shipped pairs, n = 1,2; complete additional a = 0, b = 1, n = 3 source | Four b = 1 libraries corrected; six b = 0 libraries unchanged |
| G4 | Not part of this acceptance | All 14 files retained, not independently certified by this audit |

Two exact reductions avoid large simultaneous tensor allocations:

- For a undifferentiated scalar factors and r differentiated slots,
  $V_a(p_1,\ldots,p_{a+r})=a!\sum_{|S|=r}V_0(p_S)$.
  This follows by partitioning the labelled permutation sum; each permutation
  occurs exactly once. Inspection confirms that the G3 core uses only its r
  differentiated slots. The largest G3 case, a = 1, b = 2, n = 2, uses six
  copies of the verified a = 0 source, compared with the independently derived
  complete a = 1 action and actual public library. This does not claim a
  completed timed call to the unreduced 720-permutation constructor.
- For G5 n = 3, actual T1–T8 source seeds are contracted before all labelled
  scalar/graviton relabellings. Contraction is linear and commutes with
  relabelling. Each graviton pair is symmetric by the C/Gamma definitions, so
  unrestricted rank-one auxiliary vectors determine this full symmetric tensor.
  Einstein and cubic-Hessian residuals separately vanish. The omitted-T8 and
  wrong-relative-factor controls remain nonzero. This is the complete specified
  vertex, not only the isolated T8 check, and not a large-wrapper timing claim.

Static comparison also establishes that the contracted public definitions use
the same validated expression and differ only by their documented exact
`Contract`, `MomentumExpand` and `Expand` operations. Smaller instances of both
variants are exercised directly. These arguments do not certify all accepted
integer arguments at arbitrary order. In particular, C-tensor dependency bounds
remain, and G5 TII beyond three gravitons still returns a failure (RULE-02).

Common controls cover the canonical massive scalar, G3(phi) integration by
parts, H2 versus Ricci–gradient contraction, constant G3/G5 boundary terms,
flat scalar contacts and the independent G5-to-John identity. No G4 production
vertex is validated by the John identity alone.

## Library and consumer delivery

Only these libraries are regenerated through the actual generator, with checked
serialisation/read-back before publication:

- `HorndeskiG5_0_1_1`, `HorndeskiG5_0_1_2`;
- `HorndeskiG5_1_1_1`, `HorndeskiG5_1_1_2`.

Every new tensor agrees with the independent action. Four-argument public
imports agree for a non-unit complex coupling; zero coupling and coincident
momentum tests pass. An empty family, a failed library value and cancellation
preserve prior definitions and provenance; repair followed by retry succeeds.
All other Horndeski library bytes are unchanged. The
[per-file manifest](HorndeskiAudit-2026-11-01.json) records all 60 old/new hashes
and explicitly separates the 14 unverified G4 files.

The consumer search found no shipped native bundle using these four b = 1
vertices. `ScalarModelBuilder.wl` uses only G5(1,0,1/2) for John; its two public
source tensors remain exactly unchanged. The minimal scalar and EsGB bundles
use their separate sources. Therefore no bundle or amplitude was rebuilt, and
no new Ward or loop-amplitude result is claimed. Existing John model files,
notebooks and verification evidence remain unchanged.

The John manifest records **all imported G5 files at its historical build**,
including unused b = 1 entries. Those old hashes are retained as historical
build provenance, not silently replaced as if a new build occurred. Its actual
b = 0 inputs and generated model hashes are unchanged. This audit's new library
manifest supplies the current provenance separately.

## Checks and resource evidence

| Check group | Passing assertions |
| --- | ---: |
| Initial serial diagnosis / labelled-leg candidate | 48 / 55 |
| G2 n = 1 inventory | 116 |
| G2 n = 2 largest / remaining selections | 20 / 125 |
| G3 n = 1 / n = 2 lower selections | 138 / 141 |
| G3 n = 2 b = 2 base / spectator-slot reduction | 19 / 20 |
| G5 n = 1 / n = 2 John / n = 2 b = 1 | 67 / 52 / 36 |
| Common convention and boundary identities | 17 |
| Three-graviton geometry and isolated T8 / complete G5 n = 3 | 9 / 13 |
| Fresh-index regression / validation, failure, cancellation and cache checks | 31 / 95 |
| Shared helper, scalar consumer and one actual parallel worker | 26 |
| Regeneration and independent comparisons, n = 1 / n = 2 | 11 / 11 |
| Candidate public import and recovery | 29 |
| Portable focused regression | 48 |
| Installed source/public import | 11 |

These groups overlap; the table is an execution ledger, not that many independent
physical processes. Every accepted run has zero unexpected diagnostics, zero
resource events and no owned process survivors. Limits were aggregate 1 GiB,
zero swap, 120 seconds plus 10 seconds grace and fresh host headroom of 2 GiB.
Most algebra batches use serial dispatch and serial FORM. The shared-helper
batch additionally checks one explicitly launched/closed parallel worker;
unrestricted automatic worker counts are not certified.

Retained development failures include an initial default-worker memory event,
large FeynCalc construction/contraction timeouts, context/inventory fixture
corrections, and a failure-injection test that hit a previously successful
cache. They are not counted as passing runs. The latter fixture was corrected
with fresh arguments; the implementation was not changed to bypass its cache.
The large cases were subsequently checked by FORM and the exact reductions
above, without increasing limits or erasing the failed records.

The retained evidence directory is
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-11-01-horndeski-audit/`.
It includes raw assertions, diagnostics, script/source hashes, process ownership,
resource/termination logs, old/new tensors, source diffs and installation receipts.
[Portable checks](../../Rules/Verification/README.md) preserve a small independent
regression in the repository. They require an external resource-limited launcher;
the script alone is not a memory/time limit.

## Scientific migration and remaining work

Restart the kernel after upgrading and recompute any result using one of the
four corrected b = 1 G5 libraries. A global rescaling of an old amplitude is not
valid: the Einstein block stays unchanged while Hessian tensor structures change.
The coupling still denotes the coefficient of phi^a X^b; no hidden factorial or
half is introduced and public signatures are unchanged.

Results constructed directly with coincident scalar momenta or colliding dummy
indices should also be recomputed. Previously stored generic G2/G3 libraries
remain valid within the audited scope. No user notebook or saved output has
been rewritten.

G4 repair and migration remain open under RULE-01/RULE-05 at the user's request.
The separate RULE-02 extension, arbitrary higher-order claims and native loop
validation are not delivered here. No commit, push or release is included.
