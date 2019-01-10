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

sync-git: 
	echo "Deploying to  $(REMOTE)"
	git diff-index --quiet HEAD -- || (echo "branch dirty, commit first" && false)
	git push &

deploy-root: sync-git
	rsync -avc --delete output/ root@$(REMOTE):/var/www/jappieklooster.nl/
deploy: clean sync-git
	cp root/* output/
	rsync -avc --delete nginx/ root@$(REMOTE):/etc/nginx/
	ssh root@$(REMOTE) "systemctl restart nginx"
	pelican content -s publishconf.py
	make deploy-root
	make deploy-penguin

deploy-penguin:
	rsync -avc --delete penguin/ root@$(REMOTE):/var/www/penguin.engineer/
