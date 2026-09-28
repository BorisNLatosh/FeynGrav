#!/usr/bin/env python3
"""Converter and runtime tests. Installation is always mocked."""
import argparse
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import sys


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--suite', choices=('all', 'core', 'form', 'runtime', 'integration'), default='all')
    args = parser.parse_args()
    here = Path(__file__).resolve().parent
    kernel = shutil.which('WolframKernel')
    if not kernel:
        parser.error('WolframKernel must be on PATH.')
    form = shutil.which('form') if args.suite != 'core' else None
    if args.suite != 'core' and not form:
        parser.error('The selected suite requires form on PATH; core does not.')
    suites = ('core', 'form', 'runtime', 'integration') if args.suite == 'all' else (args.suite,)
    with tempfile.TemporaryDirectory(prefix='calcform-tests-') as directory:
        for suite in suites:
            work = Path(directory) / suite
            work.mkdir()
            env = dict(os.environ, CFC_TEST_DIR=str(work))

            def run_kernel(phase):
                completed_marker = work / 'test-completed'
                completed_marker.unlink(missing_ok=True)
                subprocess.run([kernel, '-noinit', '-script', str(here / (phase + '.wls'))],
                               cwd=work, env=env, check=True)
                if not completed_marker.exists():
                    raise SystemExit(f'{phase} exited before completing its assertions.')

            if suite == 'core':
                run_kernel('Core')
                run_kernel('Parser')
                run_kernel('Transactions')
                run_kernel('Installer')
                continue
            if suite == 'runtime':
                prepare_runtime(work, form)
                run_kernel('Runtime')
                continue
            run_kernel('Export' if suite == 'form' else 'GravityExport')
            if suite == 'form':
                run_kernel('FormStages')
            for source in sorted(work.rglob('*.frm')):
                completed = subprocess.run([form, '-q', str(source)], cwd=work,
                                           text=True, capture_output=True)
                if completed.returncode:
                    raise SystemExit(f'FORM failed for {source.name}:\n{completed.stdout}\n{completed.stderr}')
            run_kernel('Import')
            if suite == 'integration':
                run_kernel('Loading')
                # The full bubble is exported only after smaller jobs have run.
                run_kernel('BubbleExport')
    print('All selected converter tests passed.')


def prepare_runtime(work, form):
    # Fake executables test failures; no system packages are changed.
    fake = "#!" + sys.executable + "\n" + r'''import os, pathlib, sys, time
mode = pathlib.Path(sys.argv[0]).name
source = pathlib.Path(sys.argv[1]) if len(sys.argv) > 1 else None
if mode == 'probehang' or (mode == 'hang' and (source is None or source.name != 'probe.frm')):
    pathlib.Path('pid.txt').write_text(str(os.getpid()))
    time.sleep(30)
    sys.exit(0)
if source is not None and source.name == 'probe.frm':
    pathlib.Path('probe.out').write_text('3' if mode == 'probebad' else '2')
    print('FORM 4.3')
    sys.exit(0)
if mode == 'shortwait':
    time.sleep(.15)
    pathlib.Path('finished.txt').write_text('finished')
    sys.exit(0)
if mode == 'exit':
    print('intentional test failure', file=sys.stderr)
    sys.exit(7)
if mode == 'flood':
    for _ in range(64):
        os.write(1, b'x' * 4096)
        os.write(2, b'y' * 4096)
    sys.exit(0)
if mode == 'malformed':
    import re
    header = re.search(r'"(CFC1 [0-9a-f]+)"', source.read_text()).group(1)
    pathlib.Path('job.out').write_text(header + '\n1+')
if mode == 'mismatch':
    pathlib.Path('job.out').write_text('CFC1 wrong\n1')
'''
    for mode in ('probehang', 'probebad', 'exit', 'missing', 'malformed', 'mismatch', 'hang', 'flood', 'shortwait'):
        path = work / mode
        path.write_text(fake)
        path.chmod(0o755)
    (work / 'nonexecutable').write_text('This is not executable.\n')
    space = work / 'space directory'
    space.mkdir()
    (space / 'form executable').symlink_to(form)


if __name__ == '__main__':
    main()
