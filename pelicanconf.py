#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = 'Jappie J. T. Klooster'
SITENAME = 'Jappie Journal'
SITEURL = ''

PATH = 'content'

TIMEZONE = 'Europe/Paris'

DEFAULT_LANG = 'en'

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None

# Links and social cruft
# NOTE: These aren't called just LINKS and SOCIAL because those are assumed by
# the default theme to be 2-tuples, but I need more info.
LINKS_EX = [(
    'blog',
    'everything/',
    "Isn't it this site?",
    'blog'
)]

SOCIAL_EX = [(
    'email',
    'mailto:superpwnzormegaman@gmail.com',
    "I\'M NOT A ROBOT",
    'email'
), (
    'github',
    'https://github.com/jappeace/',
    "Me telling computers to do stuff",
    'github'
)]

DEFAULT_PAGINATION = False

# Uncomment following line if you want document-relative URLs when developing
#RELATIVE_URLS = True

THEME = 'theme'

# For the landing page
TEMPLATE_PAGES = {
    '../theme/templates/home.html': 'index.html',
}

INDEX_SAVE_AS = 'everything/index.html'
INDEX_URL = 'everything/'
