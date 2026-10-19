# Release preparation for FeynGrav 4.1.0

19 October 2026. **Unreleased preparation**, originally tested as uncommitted changes on top of `e910bbe`.
This record does not announce a release, tag or publication.

## Changes

- Set the main README and startup version label to 4.1.0; retain the unreleased
  status and document the agreed versioning transition.
- Add release notes with incompatible names/options, scientific corrections,
  dependency requirements, supported models and limitations.
- Correct current model-index claims about the subsequent full John derivation
  and selected scalar Ward checks. Preserve earlier reports as historical
  evidence, adding dated resolution pointers rather than rewriting their tests.
- Extend `install.m`'s revision-aware dependency table to cover all currently
  loaded converter companions and literal FORM templates, including incremental
  parsing, propagator grouping and coefficient processing. Missing files return
  `IncompletePackage` before staged package evaluation. Historical sources that
  do not name a new module do not acquire that requirement.

## New checks

**Exact software checks: 24 passed**, in one bounded Wolfram batch:

- Complete layout accepted.
- Each of the 19 dependency-table targets removed in turn: rejected with
  `IncompletePackage`, identifying the missing file.
- A historical-source fixture without a parser reference does not require that
  parser. This checks the compatibility mechanism, not every historical archive.
- A local ZIP snapshot installed into a fresh evidence-only destination using
  the public installer, with dependency installation disabled and no download.
  Its fresh child kernels passed dependency, staged-package loading and
  representative graviton/scalar/fermion/vector vertex checks.
- Installed source carries the 4.1.0 label and excludes local `AGENTS.md`.

The snapshot contained tracked paths with their current working-tree contents;
new release-note/report files were not included in that test archive. This tests
runtime packaging, not the final publication archive. Source hashes are retained.
The existing application installation was not replaced.

The process tree was limited to 1 GiB RAM, zero swap and 120 seconds with
10 seconds termination grace, with at least 2 GiB available memory checked
immediately before launch. Runtime was approximately 9.2 seconds. The retained
cgroup peak was **450,699,264 bytes** (not process RSS); all memory-limit and
OOM event counters were zero. The service exited successfully. Its end-of-service
memory summary differs from the retained live cgroup peak and is not used as the
peak measurement. No Wolfram warning messages appeared in the test log.

Static inspection confirms that every current literal converter companion and
FORM-template dependency is represented in installer validation. Protected
library, model-expression/manifest and notebook hashes remained unchanged.
Relative Markdown file links and source/documentation whitespace were checked.

Evidence directory:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-19-release-4.1.0/`

It contains the test script, bounded launcher, source hashes, candidate ZIP,
24 assertion results, installer result, logs and resource records. Paths are
local evidence locations, not portable runtime dependencies.

## Retained mathematical evidence and boundaries

This preparation did not rerun algebra regressions, model initialisation,
front-end rendering, FORM jobs or Full benchmarks. The existing mathematical
checks are linked from the [release notes](../ReleaseNotes-4.1.0.md); no new
mathematical claim follows from installer success.

The higher-order EsGB source/order-three library and axion action convention
remain explicitly unverified. No new runtime guard or interaction change was
introduced. Loop validation and higher-valence performance work remain deferred.
The installer still defaults to `main`; no unavailable release tag is selected.

## Before actual publication

Review and commit the intended release contents only when authorised. Build the
final archive from that exact revision, verify its contents and repeat a bounded
installation smoke test if runtime contents change. Confirm release notes and
version labels, then create the authorised immutable tag and GitHub Release.
Installation from `main` remains the agreed default. Tagged installation is an
optional documentation detail; the version tag will be created when publishing.
Do not claim that `main` is pinned. These checks did not publish, tag or push
the package; the preparation is being committed separately from publication.
