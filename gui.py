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

window = tk.Tk()
for text in label_texts:
  label = tk.Label(text=text, width=label_width, height=label_height)
  label.grid()

drink_name = tk.StringVar()
drink_name.set(drink_selection[0])
drink_name_dropdown = tk.OptionMenu(window, drink_name, *drink_selection)
drink_name_dropdown.grid(row=0, column=1)

reset = tk.StringVar()
reset.set(boolean_selection[0])
reset_dropdown = tk.OptionMenu(window, reset, *boolean_selection)
reset_dropdown.grid(row=1, column=1)

slot = tk.StringVar()
slot.set(slot_selection[0])
slot_dropdown = tk.OptionMenu(window, slot, *slot_selection)
slot_dropdown.grid(row=2, column=1)

double = tk.StringVar()
double.set(boolean_selection[0])
double_dropdown = tk.OptionMenu(window, double, *boolean_selection)
double_dropdown.grid(row=3, column=1)

add_opt = tk.StringVar()
add_opt.set(boolean_selection[0])
add_opt_dropdown = tk.OptionMenu(window, add_opt, *boolean_selection)
add_opt_dropdown.grid(row=4, column=1)

serve = tk.StringVar()
serve.set(boolean_selection[0])
serve_dropdown = tk.OptionMenu(window, serve, *boolean_selection)
serve_dropdown.grid(row=5, column=1)

run_button = tk.Button(text='Run')
run_button.grid()

def handle_click(event):
  global drink_name, reset, slot, double, add_opt, serve
  with open('Input.csv', 'w') as config:
    config.write('drink_name,reset,slot,double,add_opt,serve\n')
    config.write(','.join([drink_name.get(), reset.get(), slot.get(), double.get(), add_opt.get(), serve.get()]) + '\n')
  os.system('./run.sh')

run_button.bind('<Button-1>', handle_click)

window.mainloop()
