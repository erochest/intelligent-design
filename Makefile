
SRC=$(shell find src -name '*.hs')

CABAL=cabal
FLAGS=--enable-tests

all: init test docs package

init:
	${CABAL} sandbox init
	make deps

test: build
	cabal test --test-option=--color

specs: build
	./dist/build/intelligent-design-specs/intelligent-design-specs

run:
	${CABAL} run

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

tags: ${SRC}
	hasktags --ctags *.hs src

hlint:
	hlint *.hs src specs

clean:
	${CABAL} clean

distclean: clean
	${CABAL} sandbox delete

configure: clean
	cabal configure ${FLAGS}

deps: clean
	${CABAL} install --only-dependencies --allow-newer --constraint=monad-control==0.3.3.0  ${FLAGS}
	make configure

build:
	cabal build

restart: distclean init build

rebuild: clean configure build

.PHONY: all init test run clean distclean configure deps build rebuild hlint
