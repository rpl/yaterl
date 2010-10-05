REBAR=../../build-tools/rebar
YATERL_DEVTOOL=../../builds/yaterl-devtool
PACK_YATERL=-y ../../builds/yaterl-0.0.1 --pack-yaterl 

help:
	@echo "YATErl building tool."
	@echo "Usage: "
	@echo "       ./make {compile|clean}"        
	@echo
	@echo "       ./make pack"  
	@echo

compile: FORCE
	${REBAR} compile

release:
	${YATERL_DEVTOOL} ${PACK_YATERL} pack ../../builds/examples/route.escript yate_test_route

clean:
	${REBAR} clean

FORCE:
