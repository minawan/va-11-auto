proto:
	protoc \
		-I third_party/googleapis \
		-I va11auto/ \
		--go_out=va11auto \
		--go_opt=paths=source_relative \
		--go-grpc_out=va11auto \
		--go-grpc_opt=paths=source_relative \
		va11auto/va_11_auto.proto
clean:
	rm -f va11auto/*.pb.go
