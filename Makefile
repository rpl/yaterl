REBAR=./build-tools/rebar
PLANTUML=plantuml
RESIZE_IMGS=./build-tools/resize_images

default: release

help:
	@echo "YATErl building tool."
	@echo "       ./make {compile|clean}"        
	@echo
	@echo "       ./make test"  
	@echo
	@echo "       ./make doc"
	@echo
	@echo "       ./make {release|clean-release}"
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

release: release-yaterl release-devtool release-examples

clean-release: clean clean-devtool clean-examples
	rm -rf ./builds

release-yaterl: compile 
	${REBAR} install target=./builds/

release-devtool: compile-devtool
	mkdir -p builds
	make -C devtool release

release-examples: compile-examples
	mkdir -p builds/examples
	make -C examples/route release
	make -C examples/resolver release
	make -C examples/registration release

compile-devtool:
	make -C devtool compile

compile-examples: release-devtool
	make -C examples/route compile
	make -C examples/resolver compile
	make -C examples/registration compile

clean-devtool:
	make -C devtool clean	

clean-examples:
	make -C examples/route clean
	make -C examples/resolver clean
	make -C examples/registration clean

FORCE:
