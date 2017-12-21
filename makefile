clean: 
	rm -R "output/*" || true
run: clean
	pelican -D --ignore-cache
	ln -s "../images" "output/drafts/images"
	xdg-open "localhost:8000"

deploy: clean
	git diff-index --quiet HEAD -- || (echo "branch dirty, commit first" && false)
	pelican content -s publishconf.py
	git push &
	rsync -avc --delete output/ root@jappieklooster.nl:/var/www/html/
