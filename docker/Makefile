help:
	@echo "docker-ssk-web:   Build docker image"
	@echo "run-ssk-web:      Run ssk-web image"

docker-ssk-web:
	@sudo docker build -t="rjose/ssk-web" -f Dockerfile-web .

run-ssk-web:
	@sudo docker run --rm -p 80 -t -i rjose/ssk-web /usr/sbin/nginx

run-ssk-web-shell:
	@sudo docker run --rm -t -i rjose/ssk-web /bin/bash
