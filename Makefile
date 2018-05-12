clean:
	rm -rf dist/

build:
	elm make --output dist/main.elm.js src/Main.elm

dev:
	elm live --debug --output dist/main.elm.js src/Main.elm
