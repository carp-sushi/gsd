.PHONY: all format build test lint run clean

all: format build

format:
	@fourmolu -q -i src/*.hs app/*.hs test/*.hs

build:
	@stack build

test:
	@stack test

lint:
	@hlint src/*.hs app/*.hs

run:
	@stack run -- gsd.cfg

clean:
	@stack purge
