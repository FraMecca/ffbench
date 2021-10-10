all: prepare target

prepare:
	rm main -rf
target:
	dune build
	ln -sf _build/default/main.exe ./main
