#!/usr/bin/env python3
"""Compare two generated FORM programs in isolated directories, sequentially."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import statistics
import subprocess
import time


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('baseline', type=Path)
    parser.add_argument('candidate', type=Path)
    parser.add_argument('--output', required=True, type=Path, help='new report directory')
    parser.add_argument('--workers', type=int, default=1, help='1: FORM; >1: TFORM -wN')
    parser.add_argument('--pairs', type=int, default=5)
    parser.add_argument('--timeout', type=float, default=60)
    args = parser.parse_args()
    if args.workers < 1 or args.pairs < 2 or args.timeout <= 0:
        parser.error('Use positive workers/timeout and at least two pairs.')
    engine = shutil.which('form' if args.workers == 1 else 'tform')
    if not engine:
        parser.error('Requested FORM/TFORM executable is not on PATH.')
    args.output.mkdir(parents=True, exist_ok=False)
    records = []
    expected = None
    for label, source in [('baseline', args.baseline), ('candidate', args.candidate)]:
        work = args.output / label
        work.mkdir()
        text = source.read_text()
        # Generated programs have one dedicated result path, referenced four times.
        paths = re.findall(r'(?m)^#(?:create|write|close) <([^>]+)>', text)
        if len(paths) != 4 or len(set(paths)) != 1:
            parser.error('Expected an unmodified generated program with one result path.')
        text = text.replace('<' + paths[0] + '>', '<result.out>')
        (work / 'job.frm').write_text(text)

    for iteration in range(args.pairs + 1):
        labels = ['baseline', 'candidate'] if iteration % 2 == 0 else ['candidate', 'baseline']
        for label in labels:
            work = args.output / label
            result = work / 'result.out'
            result.unlink(missing_ok=True)
            command = [engine] + ([] if args.workers == 1 else [f'-w{args.workers}']) + ['job.frm']
            started = time.perf_counter()
            try:
                process = subprocess.run(command, cwd=work, capture_output=True, timeout=args.timeout)
            except subprocess.TimeoutExpired as error:
                (work / f'{iteration}.stdout').write_bytes(error.stdout or b'')
                raise SystemExit(f'{label} timed out; no speedup claim is valid.') from error
            seconds = time.perf_counter() - started
            (work / f'{iteration}.stdout').write_bytes(process.stdout)
            (work / f'{iteration}.stderr').write_bytes(process.stderr)
            if process.returncode or not result.exists():
                raise SystemExit(f'{label} failed; inspect retained logs.')
            data = result.read_bytes()
            digest = hashlib.sha256(data).hexdigest()
            if expected is None:
                expected = digest
            if digest != expected:
                raise SystemExit('Result files differ, including mapping marker; inspect retained outputs.')
            record = dict(iteration=iteration, warmup=iteration == 0, variant=label,
                          seconds=seconds, bytes=len(data), sha256=digest)
            records.append(record)
            with (args.output / 'runs.jsonl').open('a') as stream:
                stream.write(json.dumps(record) + '\n')
            print(json.dumps(record), flush=True)
    medians = {label: statistics.median(r['seconds'] for r in records
               if r['variant'] == label and not r['warmup']) for label in ['baseline', 'candidate']}
    report = dict(executable=engine, workers=args.workers, pairs=args.pairs,
                  load_average=os.getloadavg(), logical_cpus=os.cpu_count(), medians=medians,
                  speedup=medians['baseline'] / medians['candidate'],
                  wall_time_reduction=1 - medians['candidate'] / medians['baseline'],
                  result_sha256=expected)
    (args.output / 'summary.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
