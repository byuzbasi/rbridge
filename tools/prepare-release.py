"""Prepare local publication artifacts without Git operations or uploads."""
import argparse
import csv
import gzip
import hashlib
import io
import json
from pathlib import Path
import re
import shutil
import tarfile


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def normalize_archive(source, destination):
    source, destination = Path(source), Path(destination)
    if destination.exists():
        raise SystemExit(f"Refusing to overwrite {destination}")
    destination.parent.mkdir(parents=True, exist_ok=True)
    with tarfile.open(source) as original:
        with destination.open("wb") as handle, gzip.GzipFile(fileobj=handle, mode="wb", mtime=0) as compressed:
            with tarfile.open(fileobj=compressed, mode="w", format=tarfile.PAX_FORMAT) as output:
                for member in original.getmembers():
                    data = original.extractfile(member).read() if member.isfile() else None
                    if member.name.endswith("/DESCRIPTION"):
                        text = data.decode("utf-8")
                        text = re.sub(r"^(Packaged: .*?; ).*$", r"\1Bahadir Yuzbasi", text, flags=re.M)
                        data = text.encode("utf-8")
                        member.size = len(data)
                    member.uid = member.gid = member.mtime = 0
                    member.uname = member.gname = ""
                    member.pax_headers = {}
                    output.addfile(member, io.BytesIO(data) if data is not None else None)
        with tarfile.open(destination) as normalized:
            for member in original.getmembers():
                if member.isfile() and not member.name.endswith("/DESCRIPTION"):
                    if original.extractfile(member).read() != normalized.extractfile(member.name).read():
                        raise SystemExit(f"Unexpected content change: {member.name}")
    return {"original_sha256": sha(source), "public_sha256": sha(destination),
            "normalization": "tar ownership/timestamps and DESCRIPTION packager; other file contents unchanged"}


def stage(args):
    destination = Path(args.destination)
    if destination.exists():
        raise SystemExit(f"Refusing to overwrite {destination}")
    ignored = shutil.ignore_patterns("*.o", "*.so", "*.dll", ".DS_Store", ".RData", ".Rhistory", ".Rproj.user", ".git")
    shutil.copytree(args.package, destination, ignore=ignored)
    shutil.copytree(args.tools, destination / "tools", ignore=ignored)
    reported = destination / "benchmarks" / "reported"
    reference = destination / "benchmarks" / "reference"
    reported.mkdir(parents=True)
    reference.mkdir()
    shutil.copy2(args.legacy_driver, reported / "frozen-driver.R")
    run = Path(args.completed_run)
    run_copy = reported / "run"
    run_copy.mkdir()
    with (run / "MANIFEST.tsv").open() as handle:
        source_rows = list(csv.DictReader(handle, delimiter="\t"))
    for row in source_rows:
        source = run / row["path"]
        if source.stat().st_size != int(row["bytes"]) or sha(source) != row["sha256"]:
            raise SystemExit(f"Invalid completed-run file: {row['path']}")
        target = run_copy / row["path"]
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, target)
    session = run_copy / "sessionInfo.txt"
    session.write_text(re.sub(r"(?<!\S)(?:[A-Za-z]:)?/[^\s;]+", "system-R-library", session.read_text()))
    with (run_copy / "MANIFEST.tsv").open("w", newline="") as handle:
        writer = csv.writer(handle, delimiter="\t")
        writer.writerow(["path", "bytes", "sha256"])
        for p in sorted(run_copy.rglob("*")):
            if p.is_file() and p.name != "MANIFEST.tsv":
                writer.writerow([str(p.relative_to(run_copy)), p.stat().st_size, sha(p)])
    (reported / "manifest-provenance.json").write_text(json.dumps({
        "original_manifest_sha256": sha(run / "MANIFEST.tsv"),
        "public_manifest_sha256": sha(run_copy / "MANIFEST.tsv"),
        "change": "generic local R-framework paths in sessionInfo.txt replaced; all shards and configuration unchanged"
    }, indent=2) + "\n")
    reference_meta = normalize_archive(args.baseline_archive, reference / "rbridge_1.0.3-first-repair.tar.gz")
    (reference / "source-provenance.json").write_text(json.dumps(reference_meta, indent=2) + "\n")
    (destination / ".gitignore").write_text((destination / ".gitignore").read_text() + "\noutput/\nlibrary/\n")
    (destination / "benchmarks" / "README.md").write_text(
        "# Reproducing the recorded engine benchmark\n\n"
        "The published timing evidence is in `inst/doc/benchmarks/`: 48 comparisons, "
        "1440 individual timings, summary tables, graphics and a SHA-256 manifest. "
        "The seed, full grid and software/hardware provenance are in `provenance.json`. "
        "The 96 original synthetic timing shards are in `reported/run/`; their manifest "
        "is regenerated only to redact local R-framework paths in sessionInfo.txt. "
        "The original/public manifest hashes and transformation are recorded separately. "
        "All shard and configuration bytes are unchanged.\n\n"
        "The reference source in `reference/` is the first repaired 1.0.3 candidate. "
        "Only package-builder and archive-owner metadata were normalized; numerical "
        "source files are unchanged. Its checksum transformation is recorded separately. "
        "It is not the original CRAN 1.0.2 source. Both engines share version 1.0.3; "
        "source and installed-engine hashes distinguish them.\n\n"
        "Install the reference into a separate library, and install the current source "
        "into another library. Development dependencies are digest, jsonlite and "
        "microbenchmark for timings, and knitr/ggplot2 for analysis. Package runtime "
        "dependencies remain those in DESCRIPTION. No tool installs dependencies automatically.\n\n"
        "Run development commands from the repository root with `Rscript --vanilla`. "
        "For a new comparison, first run `Rscript --vanilla tools/benchmark-package.R design BASELINE_LIBRARY "
        "CANDIDATE_LIBRARY NEW_OUTPUT`, then run a small `smoke` in another new directory. "
        "Set OMP_NUM_THREADS, OPENBLAS_NUM_THREADS, MKL_NUM_THREADS, BLIS_NUM_THREADS, "
        "VECLIB_MAXIMUM_THREADS and NUMEXPR_NUM_THREADS to 1 before starting R. "
        "The full run uses the design output and --resume; it is a user-run workload. "
        "Resume requires the same configuration, driver, engines and environment.\n\n"
        "`reported/frozen-driver.R` preserves the exact driver used for the recorded "
        "benchmark. Its legacy hash ordering needs LC_ALL=en_US.UTF-8. The current "
        "development driver uses byte ordering and a new fingerprint schema. "
        "Rebuilding an engine establishes a new signature; cross-platform replay "
        "does not reproduce the exact installed binary hashes or the measured times.\n\n"
        "Analyze a verified completed run with `Rscript --vanilla tools/summarize-benchmark.R RUN NEW_ANALYSIS`. "
        "This command checks manifests and numerical gates and does not fit models. "
        "The recorded comparison uses one machine, one data seed and a fixed engine order. "
        "It measures serial implementation speed and is not a convergence or accuracy study.\n"
    )
    files = sorted(p for p in destination.rglob("*") if p.is_file())
    with (destination / "PUBLIC_MANIFEST.tsv").open("w", newline="") as handle:
        writer = csv.writer(handle, delimiter="\t")
        writer.writerow(["path", "bytes", "sha256"])
        writer.writerows([str(p.relative_to(destination)), p.stat().st_size, sha(p)] for p in files)
    print(f"Prepared {len(files)} public files in {destination}")


parser = argparse.ArgumentParser(description=__doc__)
commands = parser.add_subparsers(dest="mode", required=True)
normal = commands.add_parser("normalize")
normal.add_argument("source")
normal.add_argument("destination")
release = commands.add_parser("stage")
for option in ("package", "tools", "destination", "baseline-archive", "legacy-driver", "completed-run"):
    release.add_argument("--" + option, required=True)
args = parser.parse_args()
if args.mode == "normalize":
    print(json.dumps(normalize_archive(args.source, args.destination), indent=2))
else:
    stage(args)
