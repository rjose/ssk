
BoldOn = \e[1m
ItalicOn = \e[3m
Normal = \e[0m

#-------------------------------------------------------------------------------
# Prints help message
#-------------------------------------------------------------------------------
.PHONY: help
help:
	@echo -e "\n${BoldOn}SSK Make Targets${Normal}"

	@echo -e "\nMISC"
	@echo -e "\t${BoldOn}help${Normal}:\t\tShows this message"
	@echo -e "\t${BoldOn}clean-images${Normal}:\tRemoves all docker containers and images"

	@echo -e "\nBUILDING DOCKER IMAGES\n"
	@echo -e "\t${BoldOn}docker-base${Normal}:\tBuilds base docker image (for dev)"

	@echo -e "\nRUNNING DOCKER CONTAINERS\n"
	@echo -e "\t${BoldOn}run${Normal}:\t\tRuns a docker image"
	@echo -e "\t\t\t${ItalicOn}make n=base i=sskit/base:1 run${Normal}\n"

	@echo -e "\t${BoldOn}shell${Normal}:\t\tStarts shell in running docker container"
	@echo -e "\t\t\t${ItalicOn}make n=base shell${Normal}\n"

	@echo -e "\t${BoldOn}stop${Normal}:\t\tStops a docker container"
	@echo -e "\t\t\t${ItalicOn}make n=base stop${Normal}\n"


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

#-------------------------------------------------------------------------------
# Runs base image
#-------------------------------------------------------------------------------
.PHONY: run-base
run-base:
	make n=base i=sskit/base:1 run

#-------------------------------------------------------------------------------
# Builds base image
#-------------------------------------------------------------------------------
.PHONY: docker-base
docker-base:
	docker build -t sskit/base:1 ./docker/base

#-------------------------------------------------------------------------------
# Removes all docker images
#-------------------------------------------------------------------------------
.PHONY: clean-images
clean-images:
	docker rm $$(docker ps -a -q)
	docker rmi $$(docker images -q)
