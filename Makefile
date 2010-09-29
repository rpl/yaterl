REBAR=build-tools/rebar

default: yaterl yaterl-devtool

yaterl:
	make -C apps/yaterl compile
	${REBAR} install target=../../builds/

yaterl-devtool:
	make -C apps/yaterl-devtool compile
	mkdir -p builds
	cp apps/yaterl-devtool/bin/yaterl_devtool builds/