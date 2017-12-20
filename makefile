clean: 
	rm -R "output/*" || true
run: clean
	pelican -D --ignore-cache
	ln -s "../images" "output/drafts/images"
	xdg-open "localhost:8000"

deploy: clean
	pelican content -s publishconf.py
	rsync -avc --delete output/ root@jappieklooster.nl:/var/www/html/
