zips = $(wildcard *.zip)
tifs = $(patsubst %.zip, %.tif, $(zips))

all: $(tifs)

%.tif:%.zip
	unzip -u $<
