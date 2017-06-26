#! /bin/bash

# Sometimes stuf is confusing with drafts.
# Deleting everything prevents that
rm -R output/*

pelican -D --ignore-cache
ln -s ../images output/drafts/images
firefox localhost:8000

