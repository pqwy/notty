all:
	jbuilder build @install

install:
	jbuilder install

uninstall:
	jbuilder uninstall

reinstall: uninstall install

doc:
	jbuilder build @doc

clean:
	jbuilder clean

.PHONY: all install uninstall reinstall doc clean
