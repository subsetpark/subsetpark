gen:
	lpad-gen

draft:
	LPAD_DRAFT=true lpad-gen

clean:
	rm -rf site/*

ci:
	./bin/lpad-gen
