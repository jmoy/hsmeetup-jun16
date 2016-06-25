CXXFLAGS+= -O2 -Wall -std=c++11 -g
GHCFLAGS+= -O2 -Wall -rtsopts -g
GHC=ghc


ALL: ${TARGETS}
.PHONY: ALL

%: %.hs
	${GHC} ${GHCFLAGS} --make $@

clean:
	rm ${TARGETS} *.o *.hi


