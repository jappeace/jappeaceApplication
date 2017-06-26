#! /bin/bash

rm -R output/*
pelican content -s publishconf.py
rsync -avc --delete output/ root@jappieklooster.nl:/var/www/html/
