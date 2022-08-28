.PHONY: deploy

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp

# Build -----------------------------------------------------------------------
lisps := $(shell ffind '\.(asd|lisp|ros)$$')

build/antipodes: $(lisps)
	sbcl-raw --disable-debugger --load 'build/build.lisp'
	mv antipodes build/antipodes

deploy: build/antipodes
	scp build/antipodes jam:/opt/antipodes/antipodes
