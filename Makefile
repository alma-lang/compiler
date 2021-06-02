
.PHONY=test
test:
	deno test --watch lib/es6_global/src/

.PHONY=watch
watch:
	rescript build -w

.PHONY=build
build:
	rescript build

.PHONY=clean
clean:
	rescript clean
