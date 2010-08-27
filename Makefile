REBAR=./rel/rebar

help:
	@echo "YATErl building tool."
	@echo "Usage: "
	@echo "       ./make config_default"
	@echo
	@echo "       ./make {compile|clean}"        
	@echo
	@echo "       ./make test"  
	@echo
	@echo "       ./make doc"
	@echo
	@echo

check_config: FORCE
	@test -f rebar.config || (echo ERROR: run 'make config_default' first && exit 1)

config_default:
	cp rel/rebar_default.config rebar.config

test: FORCE compile
	${REBAR} ct

compile: check_config
	${REBAR} compile

clean:
	${REBAR} clean

doc: FORCE
	${REBAR} doc

FORCE:
