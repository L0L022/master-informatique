build:
	nix-build release.nix

run: build
	result/bin/tp2 sac0

repl:
	nix-shell --pure shell.nix --run \
		"cabal repl"

shell:
	nix-shell shell.nix

shell-pure:
	nix-shell --pure shell.nix

.PHONY: build run repl shell shell-pure external-shell
