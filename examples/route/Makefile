REBAR=../../build-tools/rebar
YATERL_DEVTOOL=../../builds/yaterl-devtool

help:
	@echo "YATErl building tool."
	@echo "Usage: "
	@echo "       ./make {compile|clean}"        
	@echo
	@echo "       ./make pack"  
	@echo

compile: FORCE
	${REBAR} compile
	${YATERL_DEVTOOL} pack ../../builds/examples/route.escript yate_test_route

clean:
	${REBAR} clean

FORCE:
