# Three importer candidates — 3 October 2026

## Decision

**Keep the existing implementation.** None of the three authorized candidate families demonstrated a performance gain beyond the observed variation on the 250,114-byte tensor fixture. All were rejected before larger trials. No incremental or cumulative improvement was retained in this round; the previously accepted tensor-factor optimisation remains unchanged.

## Small-fixture results

Each row is one fresh kernel with a separately recorded first import, one warmup and three measured public imports. Conversion and validation work are inside the import timer. Rows are in execution order; the final baseline is a reverse-order drift control, not a separate paired baseline for each candidate.

| Version | Median, s | Measured range, s | Largest sampled measured-import RSS, MiB |
|---|---:|---:|---:|
| Current baseline, before candidates | 0.293927 | 0.283111–0.302058 | 303.61 |
| 1: integer factor IDs/operator codes | 0.290739 | 0.288075–0.294166 | 299.58 |
| 2: numeric-safe validation shortcut | 0.340551 | 0.337048–0.341902 | 303.95 |
| 3a: header/body extraction | 0.305460 | 0.303730–0.305808 | 303.46 |
| 3b: count-based lexical coverage | 0.340267 | 0.331412–0.347429 | 303.81 |
| Current baseline, final control | 0.306765 | 0.292747–0.318929 | 303.97 |

The compact-ID candidate's ranges overlap the initial baseline. The header/body variant overlaps the final control. Neither establishes a gain. The numeric-validation and count-based coverage variants were slower than both baseline medians. No candidate advanced to the retained 10 MB tensor fixture or scalar control, and no further tuning was attempted.

## What was implemented in scratch copies

1. **Compact factor IDs and operator codes.** A pure lexical conversion builds an integer stream and distinct-factor vocabulary. Existing memoisation is keyed by integer ID; factor decoding remains lazy, and arithmetic/type checks retain their order. This tests representation and dispatch without adding eager identifier validation. Conversion costs are included in timing.
2. **Conservative validation specialization.** After retrieving/evaluating a cached factor, only an actual atomic number bypasses the repeated deep typed-token scan. Symbolic factors retain the ordinary check, and product/final checks remain. This is a numeric-safe specialization, not general symbolic validation memoisation. A proposed held-structure cache was not implemented: unchanged expression structure alone does not exclude conditional evaluation-rule side effects. The measured specialization was slower and was discarded before broad correctness validation.
3. **Fewer whole-input copies, two separate variants.** Variant 3a normalises CRLF, trims only leading/trailing newline characters, then splits into header and remaining body without splitting/rejoining every line. A tiny Wolfram probe confirmed that ordinary `StringSplit` drops leading/trailing empty lines but retains internal empty lines; limited splitting alone behaves differently at the beginning. Variant 3b leaves header handling unchanged and replaces token-joining coverage comparison with matched/non-whitespace character counts. It operates on the original nonoverlapping token matches, without stripping whitespace before lexing. The variants were measured independently, never combined.

## Correctness and memory limits of this evidence

All pilot imports returned without messages caught by `Check` or `Failure` results. **No exact baseline/candidate comparison or full regression suite was run in this round**, because none passed the performance promotion gate. These rejected prototypes are not correctness-certified. The existing implementation and its previously verified behaviour were preserved; no candidate was installed in the repository.

RSS is sampled every 50 ms and may miss short peaks; the table covers measured import phases only. Kernel counters before import, after import and after releasing the result are retained in JSON. Output history was disabled. There were no interleaved fingerprints or leaf counts, and only the last result was serialised outside its timer; its final released-memory counter therefore includes that serialisation. These small-fixture observations do not establish scaling or the cause of the earlier OOM.

## Provenance

- Production converter SHA-256 remained `feba8c3a854fec1ebea43fb78ac1dce1d1cd7c798fdf5a2094ad9def1cafd321`. Parser tests, README and DEVELOPER documentation also remained byte-identical to the round's snapshot.
- Local evidence: `/tmp/cfc-import-three-20261003-150431`. The companion JSON preserves source hashes, candidate diffs, raw timings/kernel counters, process/RSS records, fixture hashes and the StringSplit probe. Temporary files may eventually be cleaned.
- Invocation: `python3 /tmp/cfc-import-three-20261003-150431/run_one.py VERSION small`, in the table's order. Exact generated kernel commands are preserved in process records. The harness and reused monitor hashes are recorded.
- Kernels ran sequentially with the established limits: 300 seconds per phase, 6 GiB RSS and at least 2 GiB available system memory. No limit was hit; every timed process exited zero.
- Only this Markdown report and its JSON companion were added. No FORM calculation, full-bubble workload, production edit or commit was made.
