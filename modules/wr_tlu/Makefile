PREFIX   ?= /usr/local
STAGING  ?= 
EB       ?= no

ifeq ($(EB),no)
EB_LIB ?= -letherbone
EB_INC ?=
else
EB_LIB ?= -L$(EB)/.libs -letherbone
EB_INC ?= -I$(EB)
endif

EXTRA_FLAGS ?=
CXX         ?= g++
CXXFLAGS    ?= $(EXTRA_FLAGS) -Wall -O2 -I. $(EB_INC)

TARGETS  = libtlu.a tlu-ctl

all:	$(TARGETS)

install:
	mkdir -p $(STAGING)$(PREFIX)/bin $(STAGING)$(PREFIX)/include $(STAGING)$(PREFIX)/lib
	cp tlu-ctl $(STAGING)$(PREFIX)/bin
	cp tlu.h $(STAGING)$(PREFIX)/include
	cp libtlu.a $(STAGING)$(PREFIX)/lib

clean:
	rm -f $(TARGETS) *.o lib/*.o

tlu-ctl:	tlu-ctl.o libtlu.a
	$(CXX) $(CXXFLAGS) -o $@ $^ -Wl,-rpath,$(PREFIX)/lib $(EB_LIB)

libtlu.a:	lib/tlu.o
	rm -f $@
	ar rcs $@ $^
	ranlib $@

%.o:	%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@
