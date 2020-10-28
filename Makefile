.PHONY:
all:
	rebar3 compile

ejabberd:
	git clone https://github.com/processone/ejabberd
	cd ejabberd && git checkout 20.07 && \
	./autogen.sh && ./configure && make
