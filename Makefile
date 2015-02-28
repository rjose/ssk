#-------------------------------------------------------------------------------
# Prints help message
#-------------------------------------------------------------------------------
.PHONY: help
help:
	@echo "help: Shows this message"
	@echo "docker-base: Builds base docker image"
	@echo "run: n=<name> i=<docker image>, runs a docker image"
	@echo "shell: n=<name>, starts shell in running docker container"
	@echo "stop: n=<name>, stops a docker container"
	@echo "clean-images: Removes all docker containers and images"

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
