import csv
import tkinter as tk
import os

label_texts = ['Drink Name', 'Reset', 'Slot', 'Double', 'Add Opt.', 'Serve']
boolean_selection = ['Y', 'N']
drink_selection = []
with open('DrinkRecipe.csv', newline='') as csvfile:
  reader = csv.reader(csvfile)
  for row in reader:
    drink_selection.append(row[0])
drink_selection.remove('name')
slot_selection = ['LEFT_SLOT', 'RIGHT_SLOT']
label_width = 10
label_height = 2

def initOptionMenu(selection, window, row):
  var = tk.StringVar()
  var.set(selection[0])
  dropdown = tk.OptionMenu(window, var, *selection)
  dropdown.grid(row=row, column=1)
  return var

class App():
  def __init__(self):
    self.window = tk.Tk()
    for text in label_texts:
      label = tk.Label(text=text, width=label_width, height=label_height)
      label.grid()
    self.drink_name = initOptionMenu(drink_selection, self.window, 0)
    self.reset = initOptionMenu(boolean_selection, self.window, 1)
    self.slot = initOptionMenu(slot_selection, self.window, 2)
    self.double = initOptionMenu(boolean_selection, self.window, 3)
    self.add_opt = initOptionMenu(boolean_selection, self.window, 4)
    self.serve = initOptionMenu(boolean_selection, self.window, 5)
    run_button = tk.Button(text='Run')
    run_button.grid()
    run_button.bind('<Button-1>', self.handle_click)

  def handle_click(self, event):
    with open('Input.csv', 'w') as config:
      config.write('drink_name,reset,slot,double,add_opt,serve\n')
      config.write(','.join([self.drink_name.get(), self.reset.get(), self.slot.get(), self.double.get(), self.add_opt.get(), self.serve.get()]) + '\n')
    os.system('./run.sh')

  def mainloop(self):
    self.window.mainloop()

if __name__ == "__main__":
  app = App()
  app.mainloop()
