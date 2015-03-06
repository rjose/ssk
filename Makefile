#============================================================================
# SSK Makefile
#
# Builds all ssk images and manages all containers and releases.
#============================================================================

#-------------------------------------------------------------------------------
# "help" is the default target
#-------------------------------------------------------------------------------
default: help


#=======================================
# Targets: Building docker images
#=======================================

#-------------------------------------------------------------------------------
# Builds base image
#-------------------------------------------------------------------------------
.PHONY: docker-base
docker-base:
	docker build -t sskit/base:1 ./docker/base


#-------------------------------------------------------------------------------
# Builds dev image
#-------------------------------------------------------------------------------
.PHONY: docker-dev
docker-dev:
	docker build -t sskit/dev:1 ./docker/dev

#-------------------------------------------------------------------------------
# Builds monitor image
#-------------------------------------------------------------------------------
.PHONY: docker-monitor
docker-monitor:
	docker build -t sskit/monitor:1 ./docker/monitor

#=======================================
# Targets: Running docker containers
#=======================================

#-------------------------------------------------------------------------------
# Runs an image
#
# CL Args:
#    * n: name of container (like "monitor")
#    * i: name of docker image (like "sskit/base:1")
#-------------------------------------------------------------------------------
.PHONY: run
run:
	docker run --name ${n} --rm -i -t ${i}

#-------------------------------------------------------------------------------
# Like run, but mounts current directory into /working
#
# CL Args:
#    * n: name of container (like "monitor")
#    * i: name of docker image (like "sskit/base:1")
#-------------------------------------------------------------------------------
.PHONY: dev-run
dev-run:
	docker run --name ${n} --rm -v `pwd`:/working -i -t ${i}

#-------------------------------------------------------------------------------
# Runs shell in existing docker container
#
# CL Args:
#    * n: name of container (like "monitor")
#-------------------------------------------------------------------------------
.PHONY: shell
shell:
	docker exec -it ${n} /bin/bash

#-------------------------------------------------------------------------------
# Stops a running container
#
# CL Args:
#    * n: name of container (like "monitor")
#-------------------------------------------------------------------------------
.PHONY: stop
stop:
	docker stop ${n}

#=======================================
# Targets: n-pass
#
# Sets up containers for each pass
#=======================================

#-------------------------------------------------------------------------------
# Starts containers for pass2
#
# This runs two dev containers (dev1 and dev2) and starts up a
# monitor, exposing port 9292 for kibana and port 9200 so kibana can
# make HTTP requests of elasticsearch.
# -------------------------------------------------------------------------------
.PHONY: pass2-start
pass2-start:
	docker run -p 127.0.0.1:9292:9292 -p 127.0.0.1:9200:9200 --name monitor --hostname=monitor \
		-v `pwd`:/working --rm sskit/monitor:1&
	sleep 2
	docker run --name dev1 --hostname=dev1 --link monitor:monitor --rm sskit/dev:1&
	docker run --name dev2 --hostname=dev2 --link monitor:monitor --rm sskit/dev:1&

#-------------------------------------------------------------------------------
# Stops pass2 containers
#-------------------------------------------------------------------------------
.PHONY: pass2-stop
pass2-stop:
	docker stop monitor dev1 dev2

#-------------------------------------------------------------------------------
# Starts containers for pass3
# -------------------------------------------------------------------------------
.PHONY: pass3-start
pass3-start:
	docker run -p 127.0.0.1:9292:9292 -p 127.0.0.1:9200:9200 --name monitor --hostname=monitor \
		-v `pwd`:/working --rm sskit/monitor:1&
	sleep 2
	docker run -v `pwd`:/working --name dev1 --hostname=dev1 --link monitor:monitor --rm sskit/dev:1&


#-------------------------------------------------------------------------------
# Stops pass3 containers
#-------------------------------------------------------------------------------
.PHONY: pass3-stop
pass3-stop:
	docker stop monitor dev1

#=======================================
# Targets: Misc
#=======================================

#-------------------------------------------------------------------------------
# Prints help message
#-------------------------------------------------------------------------------
.PHONY: help
help:
	@echo -e "\n${BoldOn}SSK Make Targets${Normal}"

	@echo -e "\nMISC"
	@echo -e "\t${BoldOn}help${Normal}:\t\tShows this message"
	@echo -e "\t${BoldOn}doc${Normal}:\t\tBuilds ssk documentation (n-pass and spec)"
	@echo -e "\t${BoldOn}clean-doc${Normal}:\tRemoves generated doc files"
	@echo -e "\t${BoldOn}clean-images${Normal}:\tRemoves all docker containers and images"

	@echo -e "\nDOCKER IMAGES"
	@echo -e "\t${BoldOn}docker-base${Normal}:\tBuilds 'base' docker image (for dev)"
	@echo -e "\t${BoldOn}docker-dev${Normal}:\tBuilds 'dev' docker image"
	@echo -e "\t${BoldOn}docker-monitor${Normal}:\tBuilds 'monitor' docker image"

	@echo -e "\nDOCKER CONTAINERS"
	@echo -e "\t${BoldOn}run${Normal}:\t\tRuns a docker image"
	@echo -e "\t\t\t${ItalicOn}make n=base i=sskit/base:1 run${Normal}\n"

	@echo -e "\t${BoldOn}dev-run${Normal}:\tRuns a docker image, mounting working directory in /working"
	@echo -e "\t\t\t${ItalicOn}make n=base i=sskit/base:1 dev-run${Normal}\n"

	@echo -e "\t${BoldOn}shell${Normal}:\t\tStarts shell in running docker container"
	@echo -e "\t\t\t${ItalicOn}make n=base shell${Normal}\n"

	@echo -e "\t${BoldOn}stop${Normal}:\t\tStops a docker container"
	@echo -e "\t\t\t${ItalicOn}make n=base stop${Normal}"

	@echo -e "\nN-PASS"
	@echo -e "\t${BoldOn}pass2-start${Normal}:\tStarts 'monitor', 'dev1', and 'dev2'"
	@echo -e "\t${BoldOn}pass2-stop${Normal}:\tStops 'monitor', 'dev1', and 'dev2'"
	@echo


#-------------------------------------------------------------------------------
# Builds all documentation
#
# Some will be under n-pass, some will be under spec.
#-------------------------------------------------------------------------------
.PHONY: doc
doc:
	asciidoctor ./n-pass/index.adoc

#-------------------------------------------------------------------------------
# Removes generated doc files
#-------------------------------------------------------------------------------
.PHONY: clean-doc
clean-doc:
	rm ./n-pass/*.html

#-------------------------------------------------------------------------------
# Removes all docker images
#-------------------------------------------------------------------------------
.PHONY: clean-images
clean-images:
	docker rm $$(docker ps -a -q)
	docker rmi $$(docker images -q)


#=======================================
# Constants
#=======================================

#-------------------------------------------------------------------------------
# Prints help message
#-------------------------------------------------------------------------------
BoldOn = \e[1m
ItalicOn = \e[3m
Normal = \e[0m
