#!/usr/bin/env python
'''
Clean up for distribution
'''
import glob
import os

toBeRemoved = []
for root, dirs, files in os.walk("."):
    if root.find(".hg") >= 0:
        continue
    toBeRemoved += glob.glob(root + "/*~")
    toBeRemoved += glob.glob(root + "/buildlog.txt")
    toBeRemoved += glob.glob(root + "/dsfinal.txt")
    toBeRemoved += glob.glob(root + "/dsin.txt")
    toBeRemoved += glob.glob(root + "/dslog.txt")
    toBeRemoved += glob.glob(root + "/dsmodel.c")
    toBeRemoved += glob.glob(root + "/dymosim.exe")
    toBeRemoved += glob.glob(root + "/dymosim.exp")
    toBeRemoved += glob.glob(root + "/dymosim.lib")
    toBeRemoved += glob.glob(root + "/*.mat")

for file in toBeRemoved:
    os.remove(file)
