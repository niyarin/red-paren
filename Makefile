# install chicken-scheme
# and run chicken-install r7rs
all:
	mkdir -p bin
	csc -R r7rs -X r7rs -static  src/rparen.scm -o ./bin/rparen
