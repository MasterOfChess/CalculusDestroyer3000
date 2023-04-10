build:
	dune build

parse:
	dune utop interp

utop:
	dune utop expr

.PHONY: main
main:
	dune exec ./main.exe

clean:
	dune clean
