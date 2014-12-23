PROJECT = couchbaserl

DEPS = vbucketerl gproc

dep_vbucketerl = git https://github.com/tsharju/vbucketerl HEAD
dep_gproc = git https://github.com/uwiger/gproc 0.3.1

include erlang.mk

tags:
	find . -name "*.[he]rl" -print | etags -

.PHONY: tags
