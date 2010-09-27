REBAR=../../build-tools/rebar

help:
	@echo "YATErl building tool."
	@echo "Usage: "
	@echo "       ./make {compile|clean}"        
	@echo
	@echo "       ./make pack"  
	@echo

compile: 
	${REBAR} compile
	make pack

clean:
	${REBAR} clean

doc: FORCE
	${REBAR} doc

pack: FORCE
	../../apps/yaterl-devtool/bin/yaterl_devtool pack registration.escript registration

FORCE:
