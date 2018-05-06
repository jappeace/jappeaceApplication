#!/usr/bin/env python
"""
This file gives configuration for publishing by parsing pelican.conf
and then add more torough work such as immage preprocessing.

The reason for this split is faster build times when experimenting
"""
# -*- coding: utf-8 -*- #

import os

# Read pelicanconf
filename = "./pelicanconf.py"
filedata = open(filename, "rb").read()
exec(compile(filedata,  filename, 'exec'))

# extend configuration
PLUGINS += ['optimize_images']


filename = "./redditconf.py"
filedata = open(filename, "rb").read()
exec(compile(filedata,  filename, 'exec'))

SITEURL = "https://jappieklooster.nl"
