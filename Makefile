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

.PHONY: pass-2-start
pass-2-start:
	sudo docker run -p 127.0.0.1:9292:9292 -p 127.0.0.1:9200:9200 --name monitor --rm sskit/monitor:1&
	sudo docker run --name dev1 --link monitor:monitor --rm sskit/dev:1&
	sudo docker run --name dev2 --link monitor:monitor --rm sskit/dev:1&

.PHONY: pass-2-stop
pass-2-stop:
	sudo docker stop monitor dev1 dev2

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
	@echo -e "\t${BoldOn}pass2${Normal}:\t\tStarts 'monitor', 'dev1', and 'dev2'"
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
