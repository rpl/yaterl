REBAR=build-tools/rebar

default: yaterl yaterl-devtool yaterl-examples

yaterl:
	make -C apps/yaterl compile

yaterl-devtool:
	make -C apps/yaterl-devtool compile

yaterl-examples: 
	mkdir -p builds/examples
	make -C examples/route compile
	make -C examples/resolver compile
	make -C examples/registration compile

clean:
	make -C apps/yaterl clean
	make -C apps/yaterl-devtool clean	
	make -C examples/route clean
	make -C examples/resolver clean
	make -C examples/registration clean
	rm -rf builds/*
