#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals
import re
import os

AUTHOR = 'Jappie J. T. Klooster'
SITENAME = 'Jappie'
SITEURL = '' # disabled otherwise debug will link wrongly (enabled in publish)

PATH = 'content'
STATIC_PATHS = ['files', 'images', 'raw-html']
# we obviously don't want to put any articles in any of the static paths.
# pelican needs to be told this explicetly
# this allows us to put raw html in any of these paths.
ARTICLE_EXCLUDES = list(STATIC_PATHS) # copy to make sure it's not modified

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
LINKS_EX = [
    ( 'About 📂'
    , '/pages/about.html'
    , "About me"
    , 'about'
    ),
    ( 'Hire 🐧'
    , 'http://penguin.engineer'
    , "Jappie for hire"
    , 'hire'
    ),
    ( 'Raster 🚀'
    , 'https://raster.click/'
    , 'Easy rosters for restaurants startup'
    , 'Raster'
    )
]

SOCIAL_EX = [(
    'Email ✉',
    'mailto:hi@jappie.me',
    "Contact me",
    'email'
)
# (
#     'Github',
#     'https://github.com/jappeace/',
#     "The graveyard grows",
#     'github'
# ),
# # (
#     'patreon',
#     'https://www.patreon.com/user?u=4695714',
#     'Consider supporting my endeavors',
#     'patreon'
# )
#
]

DEFAULT_PAGINATION = False

# Uncomment following line if you want document-relative URLs when developing
#RELATIVE_URLS = True

THEME = 'theme'

PLUGIN_PATHS = ["./pelican-plugins"]
PLUGINS = [
    'assets',
    'org_reader',
    'minify'
]

def regex_replace(string, find, replace):
    """A non-optimal implementation of a regex filter"""
    return re.sub(find, replace, string)
def add_abbr_tags(string):
    """Used in various places to put in abbr tags when there are more capital letters"""
    return regex_replace(string, "\ ([A-Z][A-Z0-9]{1,})\ ", " <abbr>\\1</abbr> ")

JINJA_FILTERS = {'regex_replace':regex_replace, 'add_abbr_tags':add_abbr_tags}

# the best date format is obviously signifying each number with the right word
# chinese happens to do that very concisely
DEFAULT_DATE_FORMAT = '%d日 %m月 %Y年'

ORG_READER_EMACS_LOCATION = "emacs"
ORG_READER_EMACS_SETTINGS = os.path.abspath('lisp/config.el')
ORG_READER_BACKEND = "'pelican-html"
SASS_PATH = "theme/static/css"

MINIFY = {
    'remove_comments': True,
    'remove_empty_space': True,
    'remove_optional_attribute_quotes': False,
    'pre_tags': ['ul', 'pre']
}

TYPOGRIFY = True

FEED_ATOM = "atom"

MARKDOWN = {
    'extension_configs': {
        'markdown.extensions.codehilite': {'css_class': 'highlight'},
        'markdown.extensions.extra': {},
        'markdown.extensions.meta': {},
        'markdown.extensions.toc': {},
        'markdown.extensions.tables': {},
    },
    'output_format': 'html5',
}
