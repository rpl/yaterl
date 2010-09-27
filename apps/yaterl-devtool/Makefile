REBAR=../../build-tools/rebar

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
	${REBAR} compile
	make pack

clean:
	${REBAR} clean

doc: FORCE
	${REBAR} doc

pack: FORCE
	./pack

FORCE:
