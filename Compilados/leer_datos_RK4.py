import matplotlib.pyplot as plt
import numpy as np

datos = np.loadtxt("Datos_RK4.txt")

x = datos[:, 0]
y = datos[:, 1]
y_euler = datos[:, 2]
y_exacta = datos[:, 3]


plt.plot(x, y, label="RK4 con n=6")
plt.plot(x, y_exacta, label="e^-x^2")
plt.plot(x, y_euler, label='Euler con n=6')


plt.legend()
plt.xlabel("x")
plt.ylabel("y")
plt.title("RK4 Metodo")

plt.show()


