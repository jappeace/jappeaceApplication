#! /bin/bash

pelican content -s publishconf.py
rsync -avc --delete output/ root@jappieklooster.nl:/var/www/html/
