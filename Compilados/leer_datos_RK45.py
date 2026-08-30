import numpy as np
import matplotlib.pyplot as plt

datos = np.loadtxt("datos_RK45.txt")

x = datos[:, 0]
y = datos[:, 1]
y_exacta = datos[:,2]

plt.plot(x, y, label='RK45')

#plt.plot(x, y_exacta, label='Solucion exacta')

plt.legend()
plt.xlabel("x")
plt.ylabel("y")
plt.title("RK45 metodo")

plt.show()

