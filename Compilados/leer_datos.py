import numpy as np
import matplotlib.pyplot as plt

datos = np.loadtxt("datos_euler.txt")

x = datos[:, 0]
y_euler = datos[:, 1]
y_exacta = datos[:, 2]
error = datos[:, 3]

plt.plot(x, y_euler, label="Euler (n=100,000)")
plt.plot(x, y_exacta, label="Solucion exacta(e^x)")

plt.legend()
plt.xlabel("x")
plt.ylabel("y")
plt.title("Comparacion metodo de euler")

plt.show()