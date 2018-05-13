clean:
	rm -rf dist/

build:
	elm make --output dist/main.elm.js src/Main.elm

dev:
	elm live --debug --output dist/main.elm.js src/Main.elm

deploy:
	make clean && \
		git checkout master && \
		git merge develop --no-edit && \
		make build && \
		git add dist/ && \
		git commit -m "staging: Build" && \
		git push origin HEAD && \
		git checkout develop
