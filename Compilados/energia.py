import numpy as np
import matplotlib.pyplot as plt

datos = np.loadtxt("Resortito.txt")
t = datos[:, 0]
y_euler = datos[:, 1]
y_RK4 = datos[:, 2]
v_euler = datos[:, 9]
v_RK4 = datos[:, 10]

datos_RK45 = np.loadtxt("Resortito_RK45.txt")
t_RK45 = datos_RK45[:, 0]
y_RK45 = datos_RK45[:, 1]
v_RK45 = datos_RK45[:, 2] 


k = 2.0
m = 1.0

energia_euler = 0.5 * m * (v_euler**2) + 0.5 * k * (y_euler**2)
energia_RK4 = 0.5 * m * (v_RK4**2) + 0.5 * k * (y_RK4**2)
energia_RK45 = 0.5 * m * (v_RK45**2) + 0.5 * k * (y_RK45**2)
energia_exacta = np.ones_like(t) * (0.5 * k * (1.0**2)) 

plt.plot(t, energia_euler, label='Energía Euler')
plt.plot(t, energia_RK4, label='Energía RK4')
plt.plot(t_RK45, energia_RK45, label='Energía RK45')
plt.plot(t, energia_exacta, label='Energía Exacta')

plt.legend()
plt.xlabel('Tiempo')
plt.ylabel('Energía Total')
plt.title('Comparacion energia Metodos')
plt.show()