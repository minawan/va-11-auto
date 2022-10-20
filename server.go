package main

import (
	codes "google.golang.org/genproto/googleapis/rpc/codes"
	status "google.golang.org/genproto/googleapis/rpc/status"
  pb "./va11auto"
)

var (
  port = flag.Int("port", 50051, "The server port")
)

type drinkMakerServer struct {
  pb.UnimplementedDrinkMakerServer
}

func (s *drinkMakerServer) MakeDrink(_ pb.DrinkSpec) {
  return status.New(codes.OK, "Success")
}

func newServer() *drinkMakerServer {
  return &drinkMakerServer{}
}

func main() {
  flag.Parse()
  lis, err := net.Listen("tcp", fmt.Sprintf("localhost:%d", *port))
  if err != nil {
    log.Fatalf("failed to listen: %v", err)
  }
  var opts []grpc.ServerOption
  grpcServer := grpc.NewServer(opts...)
  pb.RegisterDrinkMakerServer(grpcServer, newServer())
  grpcServer.Serve(lis)
}
