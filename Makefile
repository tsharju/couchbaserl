PROJECT = couchbaserl

DEPS = vbucketerl

dep_vbucketerl = git https://github.com/tsharju/vbucketerl HEAD

include erlang.mk

tags:
	find . -name "*.[he]rl" -print | etags -

.PHONY: tags
