clean: 
	rm -R "output/*" || true
run: clean
	pelican -D
	ln -s "../images" "output/drafts/images" || true
	xdg-open "localhost:8000"

deploy: clean
	git diff-index --quiet HEAD -- || (echo "branch dirty, commit first" && false)
	pelican content -s publishconf.py
	git push &
	echo "google-site-verification: google4043c908cce5ef76.html" > output/google4043c908cce5ef76.html # google verification, required for crawling analytics
	echo "User-agent: * \n Disallow:" > output/robots.txt # appearantly google wants this, allow access to everything
	rsync -avc --delete output/ root@jappieklooster.nl:/var/www/html/
