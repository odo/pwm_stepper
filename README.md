# pwm_stepper

Welcome to the pwm_stepper AtomVM application.

This is an experimantal projekt that uses [AtomVM](http://atomvm.net)
on an [ESP32-S3](https://docs.espressif.com/projects/esp-dev-kits/en/latest/esp32s3/esp32-s3-devkitc-1/index.html#) to drive a stepper motor via a PWM.
Hardware:

- ESPRESSIF ESP32-S3-DEVKITC-1-N8R8
- Pmod STEP Stepper Motor Driver
- JOY-IT NEMA11-01
- JOY-IT SBC-POW-BB

![hardware_setup](https://github.com/odo/pwm_stepper/blob/main/doc/pwm_stepper.png?raw=true)

The idea is to use the pulse width modulation (PWM) signal to provide exact timing for the motor driver.
This is achieved by bridging the output pin and the input pin (see red circle in the photo).
By setting an interrupt on the input pin raising we can use the pwm signal to get precise timing to drive the motor.
