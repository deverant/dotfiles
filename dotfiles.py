#!/usr/bin/env python
# -*- coding: utf-8 -*-

# A crude implementation to copy files to their correct
# places in the users home directory.

import re
import os
import sys
import errno
import shutil

# A list of regular expressions used to skip files while
# generating the file manifest for copying
IGNORE_REGEX=[r'README.md', r'dotfiles.py', r'^\.', r'~$']

home_dir = os.path.expanduser("~")

IGNORE_REGEX = re.compile("|".join(IGNORE_REGEX))

def get_file_manifest(path):
    files = []
    for root, dirnames, filenames in os.walk(path, topdown=True):
        for d in filter(lambda x: IGNORE_REGEX.match(x), dirnames):
            dirnames.remove(d)
        for f in filenames:
            if not IGNORE_REGEX.match(f):
                files.append((root, root.replace(path,""), f))

    return files

def ensure_dir(path):
    d = os.path.dirname(path)
    try:
        os.makedirs(d)
    except OSError as exception:
        if exception.errno != errno.EEXIST:
            raise


def main(argv):
    files = get_file_manifest(os.getcwd())

    for root,rel,f in files:
        if rel:
            target = "."+os.path.join(rel[1:], f)
        else:
            target = "."+f
        src = os.path.join(root, f)
        target = os.path.join(home_dir, target)
        print "copy",src,"-->",target
        ensure_dir(target)
        shutil.copy2(os.path.join(root,f), target)

if __name__=="__main__":
    main(sys.argv)
