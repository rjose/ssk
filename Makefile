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
# Targets: For convenience
#=======================================

#-------------------------------------------------------------------------------
# Runs base image
#-------------------------------------------------------------------------------
.PHONY: run-base
run-base:
	make n=base i=sskit/base:1 run

#-------------------------------------------------------------------------------
# Starts shell into container named "base"
#-------------------------------------------------------------------------------
.PHONY: shell-base
shell-base:
	make n=base shell

#-------------------------------------------------------------------------------
# Stops container named "base"
#-------------------------------------------------------------------------------
.PHONY: stop-base
stop-base:
	make n=base stop


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
	@echo -e "\t${BoldOn}clean-images${Normal}:\tRemoves all docker containers and images"

	@echo -e "\nBUILDING DOCKER IMAGES"
	@echo -e "\t${BoldOn}docker-base${Normal}:\tBuilds base docker image (for dev)"

	@echo -e "\nRUNNING DOCKER CONTAINERS"
	@echo -e "\t${BoldOn}run${Normal}:\t\tRuns a docker image"
	@echo -e "\t\t\t${ItalicOn}make n=base i=sskit/base:1 run${Normal}\n"

	@echo -e "\t${BoldOn}shell${Normal}:\t\tStarts shell in running docker container"
	@echo -e "\t\t\t${ItalicOn}make n=base shell${Normal}\n"

	@echo -e "\t${BoldOn}stop${Normal}:\t\tStops a docker container"
	@echo -e "\t\t\t${ItalicOn}make n=base stop${Normal}"

	@echo -e "\nFOR DEVELOPMENT"
	@echo -e "\t${BoldOn}run-base${Normal}:\tRuns sskit/base image in a container named 'base'"
	@echo -e "\t${BoldOn}shell-base${Normal}:\tStarts shell into a container named 'base'"
	@echo -e "\t${BoldOn}stop-base${Normal}:\tStops container named 'base'"


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
