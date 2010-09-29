REBAR=../../build-tools/rebar
YATERL_DEVTOOL_PACK=../../build-tools/yaterl_devtool_pack

help:
	@echo "YATErl building tool."
	@echo "Usage: "
	@echo "       ./make {compile|clean}"        
	@echo
	@echo "       ./make test"  
	@echo
	@echo "       ./make doc"
	@echo
	@echo

test: FORCE compile
	${REBAR} ct

compile: 
	${REBAR} get-deps
	${REBAR} compile
	make pack

clean:
	${REBAR} clean

doc: FORCE
	${REBAR} doc

pack: FORCE
	${YATERL_DEVTOOL_PACK}

FORCE:
