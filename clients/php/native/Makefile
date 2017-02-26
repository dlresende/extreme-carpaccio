DOCKER_COMPOSE ?= $(shell which docker-compose)
PHP ?= $(shell $(DOCKER_COMPOSE) run php which php)
COMPOSER ?= $(shell $(DOCKER_COMPOSE) run php which composer)
CASPERJS ?= $(shell $(DOCKER_COMPOSE) run testing which casperjs)
THIS_MAKEFILE := $(abspath $(lastword $(MAKEFILE_LIST)))

.PHONY: start
start:
	$(DOCKER_COMPOSE) up -d

.PHONY: stop
stop:
	$(DOCKER_COMPOSE) down

.PHONY: restart
restart:
	$(MAKE) -f $(THIS_MAKEFILE) stop
	$(MAKE) -f $(THIS_MAKEFILE) start

.PHONY: up
up: vendor

composer.lock: composer.json
	$(DOCKER_COMPOSE) run php $(PHP) $(COMPOSER) update

vendor: composer.lock
	$(DOCKER_COMPOSE) run php $(PHP) $(COMPOSER) install

.PHONY: functional-tests
functional-tests:
	$(DOCKER_COMPOSE) run testing $(CASPERJS) test /home/project/tests/functionals/casperjs/

.PHONY: unit-tests
unit-tests:
	$(DOCKER_COMPOSE) run php $(PHP) vendor/bin/atoum

.PHONY: tests
tests: functional-tests unit-tests
