PROJECT = ssk_web
DEPS = lager cowboy lager_syslog

dep_cowboy = git git://github.com/ninenines/cowboy.git 1.0.1
dep_lager_syslog = git git://github.com/basho/lager_syslog master

include erlang.mk

# Compile flags
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}'
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
