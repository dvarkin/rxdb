PROJECT = rxdb
PROJECT_DESCRIPTION = RxDB project
PROJECT_VERSION = 0.0.1

DEPS = cowboy cowboy_swagger mixer jiffy

dep_mixer          = git https://github.com/inaka/mixer.git 0.1.4
dep_cowboy_swagger = git https://github.com/inaka/cowboy-swagger.git

include erlang.mk
