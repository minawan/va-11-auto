package main

import (
  "context"
  "flag"
  "fmt"
  "log"
  "net"

  "google.golang.org/grpc"

	codes "google.golang.org/grpc/codes"
	status "google.golang.org/genproto/googleapis/rpc/status"
  pb "va-11-auto/va11auto"
)

var (
  port = flag.Int("port", 50051, "The server port")
)

type drinkMakerServer struct {
  pb.UnimplementedDrinkMakerServer
}

func (s *drinkMakerServer) MakeDrink(context.Context, *pb.DrinkSpec) (*status.Status, error) {
  return &status.Status{Code: int32(codes.OK), Message: "Success"}, nil
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
