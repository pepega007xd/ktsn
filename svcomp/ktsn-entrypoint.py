#!/usr/bin/python3

import os
import sys

from subprocess import run

OPAM_SYMLINK = "/tmp/_opam"

SELF = os.path.dirname(os.path.realpath(__file__))

OPAM_PATH = os.path.join(SELF, "_opam")
FRAMAC_BIN = os.path.join(SELF, "_opam/bin/frama-c")


def print_version():
    cmd = [FRAMAC_BIN, "-ktsn-version"]
    run(cmd)


def run_ktsn(source_files, options):
    cmd = [
        FRAMAC_BIN,
        "-ktsn",
        "-ktsn-svcomp-mode",
        "-ktsn-no-catch-exceptions",
        *options,
        *source_files,
    ]

    print("[SV-wrapper] Running Frama-C with command: ", " ".join(cmd))
    sys.stdout.flush()

    process = run(cmd, universal_newlines=True)
    return process.returncode


def setup():
    """
    We need to create symlink to _opam in /tmp to deal with absolute paths".
    """
    if not os.path.exists(OPAM_SYMLINK):
        try:
            os.symlink(OPAM_PATH, OPAM_SYMLINK)
        except Exception:
            print("Initialization of _opam symlink failed")
            raise Exception


def get_sources():
    source_files = []
    options = []
    for arg in sys.argv[1:]:
        if arg.endswith(".i") or arg.endswith(".c"):
            source_files.append(arg)
        else:
            options.append(arg)
    return source_files, options


if __name__ == "__main__":
    setup()
    if "--version" in sys.argv:
        print_version()
        exit(0)
    else:
        sources, options = get_sources()
        res = run_ktsn(sources, options)
        exit(res)
