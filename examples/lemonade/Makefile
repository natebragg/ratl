.PHONY: clean

lemonade-c: lemonade.c
	gcc -o $@ $^ -lClp

lemonade-hs: lemonade.hs
	ghc -o $@ -main-is Lemonade $^

clean:
	rm -f lemonade-c lemonade-hs lemonade.o lemonade.hi
