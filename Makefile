include Makefile.local

targets: bindir minorb 

bindir:
	@if test ! -d bin;  then mkdir bin;  fi
	@if test ! -d bin/$(GAParch);  then mkdir bin/$(GAParch);  fi

minorb-make: 
	(cd $(MINORBDIR); make)

bin/$(GAParch)/minorb.bin: minorb-make
	install --strip $(MINORBDIR)/minorb.bin bin/$(GAParch)

minorb: bin/$(GAParch)/minorb.bin 

clean-minorb:
	(cd $(MINORBDIR); make clean)


doc:
	(cd doc; make)

clean-doc:
	(cd doc; make clean)

clean: clean-doc clean-minorb bin-clean

bin-clean:
	rm -f bin/$(GAParch)/*

install: targets

tar:
	tar czvpf symorb.tgz --numeric-owner --exclude=symorb.tgz  --directory=.. symorb
