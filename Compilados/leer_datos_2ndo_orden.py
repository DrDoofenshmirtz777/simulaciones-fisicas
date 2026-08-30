import numpy as np
import matplotlib.pyplot as plt

datos = np.loadtxt("Resortito.txt")

x = datos[:, 0]
y_euler = datos[:, 1]
y_RK4 = datos[:, 2]
error_abs = datos[:, 3]
error_relativo_euler = datos[:, 4]
error_relativo_RK4 = datos[:, 5]
x_exacta = datos[:, 6]
error_real_euler = datos[:, 7]
error_real_RK4 = datos[:,8]



datos_RK45 = np.loadtxt("Resortito_RK45.txt")
x_RK45 = datos_RK45[:, 0]
y_RK45 = datos_RK45[:, 1]
error_relativo_RK45 = datos_RK45[:, 4]

plt.plot(x, y_euler, label='Euler')
plt.plot(x, y_RK4, label='RK4')
plt.plot(x, error_abs, label='error absoluto')
plt.plot(x, error_relativo_euler, label='Error relativo Euler')
plt.plot(x, error_relativo_RK4, label='Error relativo RK4')
plt.plot(x, x_exacta, label='Solucion exacta')
plt.plot(x, error_real_euler, label='Error Real Euler')
plt.plot(x, error_real_RK4, label='Error real RK4')
plt.plot(x_RK45, y_RK45, label='RK45')
plt.plot(x_RK45, error_relativo_RK45, label='Error relativo RK45')

plt.legend()
plt.xlabel('tiempo')
plt.ylabel('posicion')

plt.title('Ecuacion de 2ndo orden')

#plt.ylim(-2, 2)
#plt.xlim(295, 300)
plt.show()