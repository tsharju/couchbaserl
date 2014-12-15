PROJECT = couchbaserl

DEPS = vbucketerl

dep_vbucketerl = git https://github.com/tsharju/vbucketerl v0.1.0

include erlang.mk

tags:
	find . -name "*.[he]rl" -print | etags -

.PHONY: tags
