REBAR=../../build-tools/rebar
PLANTUML=plantuml
RESIZE_IMGS=../../build-tools/resize_images

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
	make doc-img

doc-img:
	${PLANTUML} doc-src/plantuml/*.txt
	${RESIZE_IMGS} doc/img/*.png 
	

FORCE:
