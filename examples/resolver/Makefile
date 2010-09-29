REBAR=../../build-tools/rebar

help:
	@echo "YATErl building tool."
	@echo "Usage: "
	@echo "       ./make {compile|clean}"        
	@echo

compile: 
	${REBAR} compile
	../../builds/yaterl_devtool pack ../../builds/examples/resolver.escript resolver

clean:
	${REBAR} clean

FORCE:
