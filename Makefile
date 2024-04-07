test_examples_arm64:
	_build/install/default/bin/joe -arm examples/ack.ml
	./examples/ack.exe

	_build/install/default/bin/joe -arm examples/fact.ml
	./examples/fact.exe

	_build/install/default/bin/joe -arm examples/fib.ml
	./examples/fib.exe

	_build/install/default/bin/joe -arm examples/echo.ml
	./examples/echo.exe

	_build/install/default/bin/joe -arm examples/gcd.ml
	./examples/gcd.exe

test_examples_vm:
	_build/install/default/bin/joe -vm examples/echo.ml
	_build/install/default/bin/joe -vm examples/ack.ml
	_build/install/default/bin/joe -vm examples/gcd.ml

	_build/install/default/bin/vm -exec examples/echo.ml
	_build/install/default/bin/vm -exec examples/ack.ml
	_build/install/default/bin/vm -exec examples/gcd.ml