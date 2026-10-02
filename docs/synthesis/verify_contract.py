#!/usr/bin/env python3
"""Check draft artifact formats and the independent ordinary-Fortran twin."""

import argparse
import copy
import json
from pathlib import Path
import subprocess
import tempfile

from jsonschema import Draft202012Validator
from referencing import Registry, Resource


def check_schemas(root):
    schemas = [json.loads((root / name).read_text()) for name in
               ("manifest-v1.schema.json", "source-map-v1.schema.json")]
    for schema in schemas:
        Draft202012Validator.check_schema(schema)
    registry = Registry().with_resources(
        (schema["$id"], Resource.from_contents(schema)) for schema in schemas)
    validator = Draft202012Validator(schemas[0], registry=registry)
    sha = "0" * 64
    artifact = {"path": "example.fsy", "sha256": sha}
    tool = {"name": "checker", "version": "1", "executable_sha256": sha,
            "options_sha256": sha}
    span = {"start_byte": 0, "end_byte": 1}
    claim = {"id": "identity.1", "class": "exact-identity",
             "semantic_sha256": sha, "span": span, "assumptions_sha256": sha,
             "dependencies": [], "status": "UNKNOWN", "required_proof": True,
             "evidence": [{"kind": "numerical-probe", "reason": "finite sample"}]}
    value = {"schema_version": 1, "contract": "synthesis-draft-1",
             "source": artifact, "producer": tool, "policy_sha256": sha,
             "claims": [claim], "generated": []}
    validator.validate(value)
    count = 1
    for status, kind in (("PROVED", "checked-proof"),
                         ("DISPROVED", "checked-counterexample"),
                         ("DISPROVED", "checked-negation")):
        wrong = copy.deepcopy(value)
        wrong["claims"][0]["status"] = status
        if not list(validator.iter_errors(wrong)):
            raise AssertionError(f"{status} accepted numerical-only evidence")
        right = copy.deepcopy(wrong)
        right["claims"][0]["evidence"] = [
            {"kind": kind, "reason": "independent check",
             "artifact": artifact, "checker": tool}]
        validator.validate(right)
        incomplete = copy.deepcopy(right)
        del incomplete["claims"][0]["evidence"][0]["checker"]
        if not list(validator.iter_errors(incomplete)):
            raise AssertionError("checked evidence accepted without checker")
        count += 3
    for path in ("/absolute.fsy", "../escape.fsy", "a/../escape.fsy",
                 "a\\escape.fsy"):
        wrong = copy.deepcopy(value)
        wrong["source"]["path"] = path
        if not list(validator.iter_errors(wrong)):
            raise AssertionError(f"unsafe artifact path accepted: {path}")
        count += 1
    wrong = copy.deepcopy(value)
    wrong["schema_version"] = 2
    if not list(validator.iter_errors(wrong)):
        raise AssertionError("incompatible schema version accepted")
    source_map = {"schema_version": 1, "contract": "synthesis-draft-1",
                  "original": artifact, "generated": artifact,
                  "mappings": [{"original_span": span, "generated_span": span,
                                "origin": "expression"}]}
    Draft202012Validator(schemas[1], registry=registry).validate(source_map)
    print(f"PASS schemas: {count + 2} evidence/path/version/map cases")


def check_twin(root, fo):
    module, program = (root / "oscillator-twin.f90").read_text().split(
        "program check_oscillator_twin", 1)
    with tempfile.TemporaryDirectory(prefix="synthesis-contract-", dir="/var/tmp") as tmp:
        project = Path(tmp)
        (project / "src").mkdir()
        (project / "app").mkdir()
        (project / "src/oscillator_synthesis.f90").write_text(module)
        (project / "app/synthesis_twin.f90").write_text(
            "program check_oscillator_twin" + program)
        (project / "fpm.toml").write_text(
            'name = "synthesis_twin"\nversion = "0.1.0"\n[build]\n'
            'auto-executables = false\nauto-tests = false\n[[executable]]\n'
            'name = "synthesis_twin"\nsource-dir = "app"\n'
            'main = "synthesis_twin.f90"\n')
        subprocess.run([fo], cwd=project, check=True)
        for mode, diagnostic in (("", "OK synthesis scalar twin"),
                                 ("zero", "oscillator.assume.1"),
                                 ("negative", "oscillator.assume.1"),
                                 ("nan", "oscillator.input.q"),
                                 ("infinite", "oscillator.input.p")):
            command = [fo, "exec", "synthesis_twin"] + ([mode] if mode else [])
            result = subprocess.run(command, cwd=project, text=True,
                                    stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            if (result.returncode == 0) != (mode == ""):
                raise AssertionError(f"unexpected exit for {mode}: {result.stdout}")
            if diagnostic not in result.stdout:
                raise AssertionError(f"missing diagnostic for {mode}: {result.stdout}")
        print("PASS twin: three numerical points and four guarded refusals")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--fo", default="fo", help="current fo executable")
    args = parser.parse_args()
    directory = Path(__file__).resolve().parent
    check_schemas(directory)
    check_twin(directory, args.fo)
