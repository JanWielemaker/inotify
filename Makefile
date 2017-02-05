SOBJ=   $(PACKSODIR)/geoip4pl.$(SOEXT)

all:    $(SOBJ)

OBJ=	c/inotify4pl.o

$(SOBJ): $(OBJ)
	mkdir -p $(PACKSODIR)
	$(LD) $(LDSOFLAGS) -o $@ $(OBJ) $(SWISOLIB)

check::
install::
clean:
	rm -f $(OBJ)
distclean: clean
	rm -f $(SOBJ)
