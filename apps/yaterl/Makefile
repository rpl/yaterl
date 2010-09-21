REBAR=../../build-tools/rebar

help:
	@echo "YATErl building tool."
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
	${REBAR} compile

clean:
	${REBAR} clean

doc: FORCE
	${REBAR} doc

FORCE:
