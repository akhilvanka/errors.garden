.PHONY: build run clean

build:
	stack build

run:
	stack run

dev:
	stack run --file-watch

clean:
	stack clean
