import numpy as np
import matplotlib.pyplot as plt

datos = np.loadtxt("Pendulo_doble_elastico.txt")

t = datos[:, 0]
theta1_euler = datos[:, 2]
theta2_euler = datos[:, 4]
theta1_RK4 = datos[:, 10]
theta2_RK4 = datos[:, 12]

datos_RK45 = np.loadtxt("Pendulo_RK45.txt")
t_RK45 = datos_RK45[:, 0]
theta1_RK45 = datos_RK45[:, 2]
theta2_RK45 = datos_RK45[:, 4]

plt.plot(t, theta1_euler, label='Theta 1 (Euler)')
plt.plot(t, theta1_RK4, label='Theta 1 (RK4)')
plt.plot(t_RK45, theta1_RK45, label='Theta 1 (RK45)')

plt.plot(t, theta2_euler, label='Theta 2 (Euler)')
plt.plot(t, theta2_RK4, label='Theta 2 (RK4)')
plt.plot(t_RK45, theta2_RK45, label='Theta 2 (RK45)')

plt.legend()
plt.xlabel('tiempo')
plt.ylabel('posicion angular')
plt.title('Pendulo Doble Elastico')

plt.show()