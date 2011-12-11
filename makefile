client: client.ml
	ocamlc -o client -thread -cclib -lthreads str.cma unix.cma threads.cma client.ml

server: protocol.mll server.ml room.ml room.mli
	ocamllex protocol.mll
	ocamlc -c room.mli room.ml
	ocamlc -o server -thread -cclib -lthreads \
	unix.cma threads.cma room.cmo protocol.ml server.ml

top: 
	ocamlmktop -thread -custom -o top -cclib -lthreads \
	unix.cma \
	threads.cma \
	graphics.cma \
	nums.cma \
	str.cma \
	bigarray.cma \
	dbm.cma

.PHONY : clean

clean:
	-rm *.cmo *.cma *.cmi protocol.ml server top client