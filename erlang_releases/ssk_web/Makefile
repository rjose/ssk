PROJECT = ssk_web
DEPS = lager cowboy lager_syslog

dep_cowboy = git git://github.com/ninenines/cowboy.git 1.0.1
dep_lager_syslog = git git://github.com/basho/lager_syslog master

.PHONY: release clean-release
 
release: clean-release all
	relx -o rel/$(PROJECT)
 
clean-release:
	rm -rf rel/$(PROJECT)

include erlang.mk

# Compile flags
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}'
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
