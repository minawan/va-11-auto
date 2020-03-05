GO_PACKAGE_PREFIX = github.com/minawan/va-11-auto/thrift/gen-go/

all: action.thrift command.thrift element.thrift recipe.thrift
	thrift -r --gen py --gen go:package_prefix=$(GO_PACKAGE_PREFIX) action.thrift
	thrift -r --gen py --gen go:package_prefix=$(GO_PACKAGE_PREFIX) command.thrift
	thrift -r --gen py --gen go:package_prefix=$(GO_PACKAGE_PREFIX) element.thrift

clean:
	rm -rf gen-go gen-py
