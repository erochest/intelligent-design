
all: init test docs package

init:
	cabal sandbox init
	cabal install --enable-tests --only-dependencies
	cabal configure --enable-tests

test: build

# docs:
# generate api documentation
#
# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# install:
# generate executable and put it into `/usr/local`
#
# deploy:
# prep and push

clean:
	cabal clean

distclean: clean
	cabal sandbox delete

configure: clean
	cabal configure --enable-tests

deps: clean
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

build:
	cabal build

rebuild: clean configure build

.PHONY: test docs