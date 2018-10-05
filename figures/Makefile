.PHONY: png grayscale small

png:
	-mogrify -density 300 -format png -resize 2500x2500\> *.pdf
	-rm *~
	echo done
	
tiff:
	-mogrify -density 300 -format tiff -resize 2500x2500\> *.pdf
	-rm *~
	echo done
	
grayscale:
	cp *.pdf grayscale
	cd grayscale && make all
	