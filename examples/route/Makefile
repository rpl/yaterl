REBAR=../../build-tools/rebar

help:
	@echo "YATErl building tool."
	@echo "Usage: "
	@echo "       ./make {compile|clean}"        
	@echo
	@echo "       ./make pack"  
	@echo

compile: FORCE
	${REBAR} compile
	../../builds/yaterl_devtool pack ../../builds/examples/route.escript yate_test_route

clean:
	${REBAR} clean

FORCE:
