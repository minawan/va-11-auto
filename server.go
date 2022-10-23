package main

import (
  "context"
  "flag"
  "fmt"
  "log"
  "os"
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

func (s *drinkMakerServer) MakeDrink(_ context.Context, spec *pb.DrinkSpec) (*status.Status, error) {
  format_bool := func(value bool) string {
    if value {
      return "Y"
    }
    return "N"
  }
  drinkName := spec.DrinkName.String()
  reset := format_bool(spec.Reset_)
  slot := spec.Slot.String()
  isBig := format_bool(spec.IsBig)
  addOpt := format_bool(spec.AddOpt)
  serve := format_bool(spec.Serve)
  fmt.Println("DrinkName: " + drinkName)
  fmt.Println("Reset: " + reset)
  fmt.Println("Slot: " + slot)
  fmt.Println("IsBig: " + isBig)
  fmt.Println("AddOpt: " + addOpt)
  fmt.Println("Serve: " + serve)
  config := []byte(fmt.Sprintf("drink_name,reset,slot,double,add_opt,serve\n%s,%s,%s,%s,%s,%s\n", drinkName, reset, slot, isBig, addOpt, serve))
  if err := os.WriteFile("Input.csv", config, 0644); err != nil {
    return &status.Status{Code: int32(codes.Internal), Message: err.Error()}, err
  }
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
