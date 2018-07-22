REMOTE=jappieklooster.nl
clean:
	# clean output dir
	rm -R "output/" || true
	mkdir -p "output"
	# kill pelican server if it's running (cause it needs to reload output)
	./kilserver.sh

run: clean
	pelican -D # --ignore-cache # I have no idea what this cache does
	ln -s "../images" "output/drafts/images" || true
	xdg-open "http://localhost:8000"

deploy: clean
	echo "Deploying to  $(REMOTE)"
	git diff-index --quiet HEAD -- || (echo "branch dirty, commit first" && false)
	git push &
	cp root/* output/
	rsync -avc --delete nginx/ root@$(REMOTE):/etc/nginx/
	ssh root@$(REMOTE) "systemctl restart nginx"
	pelican content -s publishconf.py
	rsync -avc --delete output/ root@$(REMOTE):/var/www/html/
