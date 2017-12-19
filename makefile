run:
	rm -R "output/*" || true
	SASS_PATH=theme/static/css pelican -D --ignore-cache
	ln -s "../images" "output/drafts/images"
	xdg-open "localhost:8000"

