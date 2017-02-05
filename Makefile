SO=$(shell eval `swipl --dump-runtime-variables` && echo $$PLSOEXT)
COFLAGS=-O2 -gdwarf-2 -g3
COFLAGS=-gdwarf-2 -g3

inotify4pl.$(SO): inotify4pl.c Makefile
	swipl-ld -shared -Wall $(COFLAGS) -shared -o inotify4pl inotify4pl.c

clean:
	rm -f *~
	rm -f inotify4pl.$(SO)
