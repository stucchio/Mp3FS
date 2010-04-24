FLAGS=-threaded
GHC=ghc $(FLAGS)


all: mp3fs

mp3fs: mp3fs.hs Mp3fsConverters.o Mp3fsInternal.o
	$(GHC) --make mp3fs.hs

Mp3fsInternal.o: Mp3fsInternal.hs
	$(GHC) -c Mp3fsInternal.hs

Mp3fsConverters.o: Mp3fsConverters.hs Mp3fsInternal.o
	$(GHC) -c Mp3fsConverters.hs


clean:
	rm *.hi *.o mp3fs