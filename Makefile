
.PHONY=test
test:
	deno test --watch lib/es6_global/src/

.PHONY=cli
cli:
	deno run lib/es6_global/src/Cli.js

.PHONY=watch
watch:
	rescript build -w

.PHONY=build
build:
	rescript build

.PHONY=clean
clean:
	rescript clean
