.PHONY: deploy update-deps

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp

# Build -----------------------------------------------------------------------
lisps := $(shell ffind '\.(asd|lisp|ros)$$')

build/antipodes: $(lisps)
	ros build build/antipodes.ros

# update-deps:
# 	hg -R /home/sjl/cl-losh pull -u
# 	hg -R /home/sjl/beast pull -u

# /opt/antipodes/antipodes: update-deps build/antipodes
# 	rm /opt/antipodes/antipodes
# 	cp build/antipodes /opt/antipodes/antipodes

# deploy: build/antipodes
# 	rsync --exclude=build/antipodes --exclude=.hg -avz . antipodes:/home/sjl/antipodes
# 	ssh silt make -C /home/sjl/antipodes /opt/antipodes/antipodes
