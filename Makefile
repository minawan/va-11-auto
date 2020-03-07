export input_directory := data
export output_directory := output

.PHONY: all thrift clean data

all: build

build: server data

thrift:
	$(MAKE) -C thrift

data:
	mkdir $(output_directory)
	$(MAKE) -C script

server: thrift
	$(MAKE) -C server

clean:
	rm -rf output/
	rm -f output.txt
	rm -rf __pycache__/
	$(MAKE) clean -C server
	$(MAKE) clean -C thrift
