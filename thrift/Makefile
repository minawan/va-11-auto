GO_PACKAGE_PREFIX = github.com/minawan/va-11-auto/thrift/gen-go/

all: action.thrift command.thrift recipe.thrift shared.thrift
	thrift -r --gen py --gen go:package_prefix=$(GO_PACKAGE_PREFIX) command.thrift

clean:
	rm -rf gen-go gen-py
