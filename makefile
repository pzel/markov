.PHONY: run publish
R := git@github.com:pzel/markov.git

run: build
	-pkill -f SimpleHTTPServer; sleep 1;
	python -m SimpleHTTPServer 9999 &
	while inotifywait -q -e modify *.elm index.html; do \
	  $(MAKE) build  ;\
	done

build:
	elm make Main.elm --output=main.js

launchpad:
	rm -rf ./launchpad
	mkdir -p launchpad
	(git clone $(R) -b gh-pages ./launchpad)

publish: build launchpad
	cp index.html main.js launchpad
	(cd launchpad &&\
	git add -A &&\
	 git commit --allow-empty -m "Update $$(date +%Y%m%d%H%M%S)" &&\
	git push -f origin gh-pages)
