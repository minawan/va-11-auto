import csv
import grpc
import tkinter as tk
import os
import va_11_auto_pb2
import va_11_auto_pb2_grpc

label_width = 10
label_height = 2
boolean_options = ['Y', 'N']
drink_options = []
with open('DrinkRecipe.csv', newline='') as csvfile:
  reader = csv.reader(csvfile)
  for row in reader:
    drink_options.append(row[0])
drink_options.remove('name')
slot_options = ['LEFT_SLOT', 'RIGHT_SLOT']

class OptionMenu():
  def __init__(self, parent, label, options):
    frame = tk.Frame(parent)
    label = tk.Label(frame, text=label, width=label_width, height=label_height)
    label.pack(side=tk.LEFT)
    var = tk.StringVar()
    var.set(options[0])
    self.var = var
    dropdown = tk.OptionMenu(frame, var, *options)
    dropdown.pack(side=tk.RIGHT)
    frame.pack(side=tk.TOP)

  def value(self):
    return self.var.get()

class App():
  def __init__(self):
    window = tk.Tk()
    window.title('va-11-auto')
    self.drink_name = OptionMenu(window, 'Drink Name', drink_options)
    self.reset = OptionMenu(window, 'Reset', boolean_options)
    self.slot = OptionMenu(window, 'Slot', slot_options)
    self.double = OptionMenu(window, 'Double', boolean_options)
    self.add_opt = OptionMenu(window, 'Add Opt.', boolean_options)
    self.serve = OptionMenu(window, 'Serve', boolean_options)
    run_button = tk.Button(window, text='Run')
    run_button.pack(side=tk.TOP)
    run_button.bind('<Button-1>', self.handle_click)
    self.window = window

  def handle_click(self, event):
    with open('Input.csv', 'w') as config:
      config.write('drink_name,reset,slot,double,add_opt,serve\n')
      config.write(','.join([self.drink_name.value(), self.reset.value(), self.slot.value(), self.double.value(), self.add_opt.value(), self.serve.value()]) + '\n')
    os.system('./run.sh')
    with grpc.insecure_channel('localhost:50051') as channel:
      stub = va_11_auto_pb2_grpc.DrinkMakerStub(channel)
      status = stub.MakeDrink(va_11_auto_pb2.DrinkSpec(drink_name=va_11_auto_pb2.DrinkSpec.ZEN_STAR, reset=False, slot=va_11_auto_pb2.DrinkSpec.LEFT_SLOT, is_big=False, add_opt=False, serve=False))
      print(status)

  def run(self):
    self.window.mainloop()

if __name__ == "__main__":
  app = App()
  app.run()
