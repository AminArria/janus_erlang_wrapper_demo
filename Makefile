.PHONY: ops ops_stop run

ops:
	docker-compose up -d

ops_stop:
	docker-compose down

run:
	./rebar3 shell
