
.PHONY: run build clean

all: bin/parser_messages.ml
	menhir --compile-errors parser.messages bin/parser.mly > bin/parser_messages.ml

bin/parser_messages.ml: parser.messages
	menhir --compile-errors parser.messages bin/parser.mly > bin/parser_messages.ml

parser.messages:
	menhir --infer --list-errors bin/parser.mly > parser.messages

run: all
	dune exec lambda

build: all
	dune exec lambda
