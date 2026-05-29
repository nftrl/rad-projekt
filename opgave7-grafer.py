import matplotlib.pyplot as plt
import numpy as np

"""
Læser fra opgave7-data.csv og skriver til opgave7-sorterede-estimater.png og opgave7-medianer.png.
Den første linje i csv-filen er det udregnede S og de følgende 100 linjer er estimater X_i.
"""

filename = 'opgave7-data.csv'
data = np.loadtxt(filename, dtype=int)
S = data[0]
estimates = data[1:]

mse = np.sum(np.square(estimates - S)) / 100

print(f'loaded {len(data)} lines of data')
print(f'S value = {S}')
print(f'number of estimates = {len(estimates)}')
print(f'mse = {mse}')

# Sorterede estimater
fig, ax = plt.subplots()
ax.scatter([x+1 for x in range(100)], sorted(estimates), label='estimater')
ax.axhline(y=S, label='faktiske værdi')
plt.title(f'Sorterede estimater, mse = {mse}')
ax.legend()
ax.grid()
plt.savefig('opgave7-sorterede-estimater.png')
plt.show()

# Medianer fra gruperringer af 11
medians = sorted([sorted(estimates[0*i: 10*i])[5] for i in range(1,10)])

fig, ax = plt.subplots()
ax.scatter([x+1 for x in range(9)], medians, label='medianer')
ax.axhline(y=S, label='faktiske værdi')
plt.title(f'Sorterede medianer fra grupperinger af størrelse 11')
ax.legend()
ax.grid()
plt.savefig('opgave7-medianer.png')
plt.show()
