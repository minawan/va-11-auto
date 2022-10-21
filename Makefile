proto:
	protoc \
		-I third_party/googleapis \
		-I va11auto/ \
		--go_out=va11auto \
		--go_opt=paths=source_relative \
		--go-grpc_out=va11auto \
		--go-grpc_opt=paths=source_relative \
		va11auto/va_11_auto.proto
	python3 \
		-m grpc_tools.protoc \
		-Iva11auto \
		-Ithird_party/googleapis \
		--python_out=. \
		--grpc_python_out=. \
		va11auto/va_11_auto.proto
server: proto
	go build server.go
clean:
	rm -f \
		va11auto/*.pb.go \
		va_11_auto_pb2.py \
		va_11_auto_pb2_grpc.py \
		server
