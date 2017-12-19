run:
	rm -R "output/*" || true
	pelican -D --ignore-cache
	ln -s "../images" "output/drafts/images"
	xdg-open "localhost:8000"

