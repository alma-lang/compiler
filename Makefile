
.PHONY=test
test:
	deno test --watch lib/es6_global/src/

.PHONY=cli
cli:
	deno run lib/es6_global/src/Cli.js

.PHONY=repl
repl:
	deno run lib/es6_global/src/Cli.js repl

.PHONY=watch
watch:
	rescript build -w

.PHONY=build
build:
	rescript build

.PHONY=clean
clean:
	rescript clean
