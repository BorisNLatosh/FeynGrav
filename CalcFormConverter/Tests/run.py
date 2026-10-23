#!/usr/bin/env python3
"""Converter, facade and runtime tests. Installation is always mocked."""
import hashlib
import argparse
import datetime
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import sys

FACADE_PHASES = ('Adapter', 'Loading', 'Standalone', 'Reliability', 'Facade')
COMPLETION_KEYS = {'Suite', 'Passed', 'Failed', 'Status'}


def require_completion(path, phase):
    """Require one fresh, well-formed, successful suite completion record."""
    try:
        data = json.loads(path.read_text(encoding='utf-8'))
    except (OSError, ValueError) as exc:
        raise SystemExit(f'{phase} did not produce a readable completion record: {exc}')
    if not isinstance(data, dict) or set(data) != COMPLETION_KEYS:
        raise SystemExit(f'{phase} completion record has the wrong fields: {data!r}')
    if data['Suite'] != phase:
        raise SystemExit(f'{phase} completion record names {data["Suite"]!r}.')
    for key in ('Passed', 'Failed'):
        value = data[key]
        if type(value) is not int or value < 0:
            raise SystemExit(f'{phase} completion record has invalid {key}: {value!r}')
    if data['Passed'] <= 0:
        raise SystemExit(f'{phase} completion record has no completed assertions.')
    if data['Failed'] != 0 or data['Status'] != 'Completed':
        raise SystemExit(f'{phase} did not complete successfully: {data!r}')


def append_phase_record(evidence, entry):
    with (evidence / 'phase-log.jsonl').open('a', encoding='utf-8') as stream:
        stream.write(json.dumps(entry, sort_keys=True) + '\n')


def run_facade_phase(kernel, script, work, env, phase, evidence):
    """Run one facade phase, retaining raw logs and the completion record."""
    completion = work / f'suite-completed-{phase}.json'
    record = evidence / f'completion-{phase}.json'
    missing = evidence / f'completion-{phase}.missing'
    stdout_path = evidence / f'phase-{phase}.stdout.log'
    stderr_path = evidence / f'phase-{phase}.stderr.log'
    completion.unlink(missing_ok=True)
    for old in evidence.glob(f'completion-{phase}.*'):
        old.unlink()
    for old in evidence.glob(f'phase-{phase}.*'):
        old.unlink()
    entry = {'Phase': phase, 'Started': datetime.datetime.now(datetime.timezone.utc).isoformat()}
    try:
        process = subprocess.Popen([kernel, '-noinit', '-script', str(script)],
                                   cwd=work, env=env, stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE, text=True)
    except OSError as exc:
        entry.update({'Status': 'LaunchFailed', 'Error': str(exc)})
        append_phase_record(evidence, entry)
        raise SystemExit(f'{phase} could not launch: {exc}')
    stdout, stderr = process.communicate()
    stdout_path.write_text(stdout or '', encoding='utf-8')
    stderr_path.write_text(stderr or '', encoding='utf-8')
    rc = process.returncode
    entry.update({'PID': process.pid, 'Exit': rc, 'Stdout': stdout_path.name,
                  'Stderr': stderr_path.name})
    if completion.exists():
        shutil.copyfile(completion, record)
        entry['Completion'] = record.name
    else:
        missing.write_text('completion record missing\n', encoding='utf-8')
        entry['Completion'] = missing.name
    if stdout:
        print(f'--- {phase} stdout ---')
        print(stdout, end='')
    if stderr:
        print(f'--- {phase} stderr ---')
        print(stderr, end='')
    try:
        if rc != 0:
            raise SystemExit(f'{phase} exited with status {rc}.')
        require_completion(completion, phase)
    except SystemExit as exc:
        entry.update({'Status': 'Failed', 'Failure': str(exc)})
        append_phase_record(evidence, entry)
        raise
    entry['Status'] = 'Completed'
    append_phase_record(evidence, entry)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--suite', choices=('all', 'core', 'form', 'runtime', 'integration', 'facade'), default='all')
    parser.add_argument('--facade-phase', action='append', choices=FACADE_PHASES, default=None,
                        help='Run only this facade phase; repeat for several (requires --suite facade).')
    parser.add_argument('--evidence-dir', type=Path, default=None,
                        help='Optional destination for facade phase logs and raw completion records.')
    args = parser.parse_args()
    if args.facade_phase is not None:
        if args.suite != 'facade':
            parser.error('--facade-phase requires --suite facade.')
        if len(set(args.facade_phase)) != len(args.facade_phase):
            parser.error('--facade-phase must not repeat a phase.')
        facade_phases = tuple(args.facade_phase)
    else:
        facade_phases = FACADE_PHASES
    evidence = None
    if args.evidence_dir is not None:
        evidence = args.evidence_dir.expanduser().resolve()
        if evidence.exists() and not evidence.is_dir():
            parser.error('--evidence-dir must be a directory.')
        evidence.mkdir(parents=True, exist_ok=True)
    here = Path(__file__).resolve().parent
    kernel = shutil.which('WolframKernel')
    if not kernel:
        parser.error('WolframKernel must be on PATH.')
    if args.suite == 'facade':
        fixture = os.environ.get('FG_CONTRACTOR_TEST_FIXTURE')
        if not fixture or not Path(fixture).is_file():
            parser.error('--suite facade requires FG_CONTRACTOR_TEST_FIXTURE to name an existing test double.')
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

            if suite == 'facade':
                facade = here.parent.parent / 'FeynGravSimplify' / 'Tests'
                for phase in facade_phases:
                    script = facade / (phase + '.wls')
                    if not script.is_file():
                        raise SystemExit(f'Facade suite script missing: {script}')
                    completion = work / f'suite-completed-{phase}.json'
                    completion.unlink(missing_ok=True)
                    if evidence is not None:
                        run_facade_phase(kernel, script, work, env, phase, evidence)
                    else:
                        subprocess.run([kernel, '-noinit', '-script', str(script)],
                                       cwd=work, env=env, check=True)
                        require_completion(completion, phase)
                continue
            if suite == 'core':
                run_kernel('Core')
                run_kernel('Parser')
                run_kernel('SharedParser')
                run_kernel('TreeParser')
                run_kernel('Transactions')
                run_kernel('Installer')
                continue
            if suite == 'runtime':
                check_colour_reference(here, work, form)
                prepare_runtime(work, form)
                run_kernel('Runtime')
                run_kernel('Epsilon')
                run_kernel('DiracColour')
                run_kernel('DiracAlgebra')
                run_kernel('ColourAlgebra')
                run_kernel('PropagatorGroups')
                run_kernel('RationalCoefficients')
                run_kernel('PropagatorCancellation')
                run_kernel('DimensionCoefficients')
                run_kernel('DimensionPreconditioning')
                run_kernel('StageSymmetry')
                run_kernel('TensorDecomposition')
                run_kernel('LoopCoefficientReduction')
                run_kernel('LoopDenominatorGroups')
                run_kernel('LoopCoefficientParameters')
                run_kernel('ScalarMasterReduction')
                run_kernel('ExportPlanning')
                run_kernel('MasterOutputSharing')
                run_kernel('MasterRationalCoefficients')
                run_kernel('PolarisationCoefficients')
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
                run_kernel('DiracColourRules')
                run_kernel('ColourRules')
                # The full bubble is exported only after smaller jobs have run.
                run_kernel('BubbleExport')
    print('All selected converter and facade tests passed.')


def check_colour_reference(here, work, form):
    """Check the pinned unchanged procedure without downloads or installation."""
    reference = here.parent / 'ThirdParty' / 'FORMColour' / 'SUn.prc'
    assert hashlib.sha256(reference.read_bytes()).hexdigest() == (
        '05548386b1e5a2872224a78bad6a424fb13f812ee6ab35d89d1c77a32f7034c3')
    engines = [(form, [])]
    tform = shutil.which('tform')
    if tform:
        engines.append((tform, ['-w2']))
    for executable, args in engines:
        subprocess.run([executable, *args, '-q', '-D',
                        'CFCSUNFILE=' + str(reference),
                        str(here / 'ColourPreflight' / 'ColourProcedure.frm')],
                       cwd=work, check=True, timeout=60)
        actual = (work / 'ColourProcedure.out').read_bytes()
        expected = (here / 'ColourPreflight' / 'Expected.out').read_bytes()
        if actual != expected:
            raise SystemExit('Unchanged colour procedure output differs from the fixture.')


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
