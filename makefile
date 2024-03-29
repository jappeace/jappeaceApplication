REMOTE=jappieklooster.nl
clean:
	chmod -R +rw "output"
	# clean output dir
	rm -R "output/" || true
	mkdir -p "output"
	# kill pelican server if it's running (cause it needs to reload output)
	./kilserver.sh || true

run: clean
	pelican -D # --ignore-cache # I have no idea what this cache does
	ln -s "../images" "output/drafts/images" || true
	xdg-open "http://localhost:8000"

sync-git: 
	echo "Deploying to  $(REMOTE)"
	git diff-index --quiet HEAD -- || (echo "branch dirty, commit first" && false)
	git push &

talks: 
	nix-build ./talks/presentation.nix
	cp -fLR result output/talks
	chmod -R +rw "output"

submodule:
	git submodule sync

.PHONY: talks
