REBAR=../../build-tools/rebar
YATERL_DEVTOOL=../../builds/yaterl-devtool

help:
	@echo "YATErl building tool."
	@echo "Usage: "
	@echo "       ./make {compile|clean}"        
	@echo

compile: 
	${REBAR} compile
	${YATERL_DEVTOOL} pack ../../builds/examples/resolver.escript resolver

clean:
	${REBAR} clean

FORCE:
