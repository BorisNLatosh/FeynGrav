# Generation progress verification

Date: 6 October 2026.

## Scope

Unconditional reporting now covers all 24 `Generate*` and `Generate*Specific`
commands. This supersedes the reporting scope in
[ScalarBatchProgress.md](ScalarBatchProgress.md). Standalone converter reporting,
interaction formulas, selection ranges, library filenames and return contracts
are unchanged.

## Checks

53 focused checks passed in fresh Wolfram kernels (Wolfram 15.0.1,
FeynCalc 10.2.1, FORM/TFORM 4.3).

- All 24 public generation commands were exercised with mocked generation,
  checking non-empty scheduling, labels and completion messages despite
  `ShowProgress -> False`. These checks do not calculate every interaction family.
- Horndeski labels included all three filename parameters and the scalar momentum
  count; pure-gravity labels distinguished library order from graviton count.
- An empty Gauss–Bonnet batch reported zero jobs and returned `Null`.
- Real serial FORM generated both order-one scalar libraries through batch and
  specific commands in temporary directories. The resulting expressions agreed
  exactly. A real two-worker TFORM order-one fermion calculation completed all
  stages and published its temporary library.
- A mocked order-five scalar batch checked ten ordered jobs and replacement counts.
- Controlled errors covered invalid arguments, missing FORM, construction,
  serialisation, timeout and publication followed by failed backup cleanup.
  Completed-file accounting, diagnostic paths and abort propagation were checked.
- Mocked converter events verified contextual elapsed-time messages and the
  serial fallback explanation. Reporter definitions and global options were
  restored after calls.

The two temporary test scripts were `/tmp/all-progress-tests.wls` and
`/tmp/all-progress-events.wls`; JSON summaries and logs used the same prefix.
No repository libraries, example notebooks or benchmark files were regenerated.
Their pre-edit checksums were checked after testing. No full algebra regression
or Full benchmark suite was run for this reporting change.

The startup explanation and guide were updated after the executable checks;
that final source change only adds a message string.

## Source snapshot

SHA-256 of `Libs/FeynGravLibrariesGenerator.wl`:

```text
926eafd98d4a4cac1e636357a8342c25ea0c1f86f7dbe928f86cf721cd8b9641
```
