REBAR=../../build-tools/rebar

help:
	@echo "YATErl building tool."
	@echo "Usage: "
	@echo "       ./make {compile|clean}"        
	@echo

compile: FORCE
	${REBAR} compile
	../../builds/yaterl_devtool pack ../../builds/examples/registration.escript registration

clean:
	${REBAR} clean

FORCE:
