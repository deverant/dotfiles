#!/usr/bin/env python
# -*- coding: utf-8 -*-

# A crude implementation to copy files to their correct
# places in the users home directory.

import re
import os
import sys
import errno
import shutil
import optparse
import datetime

# A list of regular expressions used to skip files while
# generating the file manifest for copying
IGNORE_REGEX = [r'README\.md', r'dotfiles\.py', r'^\.', r'~$']

HOME_DIR = os.path.expanduser("~")
DOTFILES_MANIFEST = ".dotfiles.manifest"

IGNORE_REGEX = re.compile("|".join(IGNORE_REGEX))

def get_file_manifest(path):
    """Generates a file manifest by going through all the files
    found under a path that does not match the given IGNORE_REGEX
    """
    files = []
    for root, dirnames, filenames in os.walk(path, topdown=True):
        for d in filter(lambda x: IGNORE_REGEX.search(x), dirnames):
            dirnames.remove(d)
        for f in filenames:
            if not IGNORE_REGEX.search(f):
                d = root.replace(path, "")
                target_file = os.path.join(d[1:], f) if d else f
                files.append((os.path.join(root,f), "." + target_file))

    return files


def write_manifest(filename, manifest):
    with open(filename, 'w') as f:
        f.write("# .dotfiles.manifest %s\n\n"%datetime.datetime.now())
        for m in manifest:
            f.write(m+"\n")


def read_manifest(filename):
    manifest = set()
    if not os.path.isfile(filename):
        return manifest # no filename found so return empty set
    with open(filename, 'r') as f:
        for line in f:
            line = line.strip()
            if not line.startswith("#") and len(line) > 0:
                manifest.add(line)

    return manifest


def ensure_dir(path):
    """Ensure that a given path exists. Because of race conditions
    we just try to create the directory and if we fail because the
    directory already exists we just silently ignore the error.
    """
    d = os.path.dirname(path)
    try:
        os.makedirs(d)
    except OSError as exception:
        if exception.errno != errno.EEXIST:
            raise


def parse_args(arguments):
    """Parse any arguments given and check validity if needed"""
    parser = optparse.OptionParser()
    parser.add_option("-n", "--dry-run", action="store_true", dest="dryrun",
                      help="Simulate a dotfiles run but make no changes to the filesystem.")

    (options, args) = parser.parse_args(arguments)

    return (options, args)


def main(argv):
    (options, args) = parse_args(argv[1:])

    old_files = read_manifest(DOTFILES_MANIFEST)

    files = get_file_manifest(os.getcwd())

    # copy new files in place
    for (src, target) in files:
        target = os.path.join(HOME_DIR, target)
        old_files.discard(target)

        print "copy:", src, "-->", target
        if not options.dryrun:
             ensure_dir(target)
             shutil.copy2(src, target)

    # remove old files from last manifest
    for filename in old_files:
        print "remove:", filename
        if not options.dryrun and os.path.isfile(filename):
            os.unlink(filename)

    if not options.dryrun:
        write_manifest(DOTFILES_MANIFEST,
                       (os.path.join(HOME_DIR,x[1]) for x in files))


if __name__ == "__main__":
    main(sys.argv) # run from command-line
