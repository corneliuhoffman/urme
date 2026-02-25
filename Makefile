.PHONY: build clean install setup

build: setup
	dune build
	cp _build/default/bin/main.exe bin/experience-agent

clean:
	dune clean
	rm -f bin/experience-agent

setup:
	opam install yojson cohttp-lwt-unix lwt uuidm -y

install:
	dune build
	cp _build/default/bin/main.exe bin/experience-agent
	@echo "Binary installed at $(CURDIR)/bin/experience-agent"
