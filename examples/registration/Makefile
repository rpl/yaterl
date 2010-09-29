REBAR=../../build-tools/rebar
YATERL_DEVTOOL=../../builds/yaterl-devtool

help:
	@echo "YATErl building tool."
	@echo "Usage: "
	@echo "       ./make {compile|clean}"        
	@echo

compile: FORCE
	${REBAR} compile
	${YATERL_DEVTOOL} pack ../../builds/examples/registration.escript registration

clean:
	${REBAR} clean

FORCE:
