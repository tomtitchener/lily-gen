#lang racket

; dynamics.rkt: differentials (ordinary differential equations, ODEs)

;; clone from
;; https://github.com/3b1b/videos/blob/master/_2019/diffyq/solve_pendulum_ode_sample_code.py

#|
import numpy as np

# Physical constants
g = 9.8
L = 2
mu = 0.1

THETA_0 = np.pi / 3  # 60 degrees
THETA_DOT_0 = 0  # No initial angular velocity

# Definition of ODE
def get_theta_double_dot(theta, theta_dot):
    return -mu * theta_dot - (g / L) * np.sin(theta)

# Solution to the differential equation
def theta(t):
    # Initialize changing values
    theta = THETA_0
    theta_dot = THETA_DOT_0
    delta_t = 0.01  # Some time step
    for time in np.arange(0, t, delta_t):
        # Take many little time steps of size delta_t
        # until the total time is the function's input
        theta_double_dot = get_theta_double_dot(
            theta, theta_dot
        )
        theta += theta_dot * delta_t
        theta_dot += theta_double_dot * delta_t
    return theta
|#

(define gravity 9.8)        ;; m/sec^2
(define pendulum_length 2)  ;; m?
(define air_resistance 0.1) ;; ? scaling of velocity 

(define initial_angle (/ pi 3)) ;; theta_0, 60: > (/ (radians->degrees pi) 3) 60.0
(define initial_velocity 0)     ;; theta_dot_0

(define (get-next-acceleration current_angle current_velocity friction)
  (- (* -1 friction current_velocity)
     (* (/ gravity pendulum_length) (sin current_angle))))

#;(define (all_angles_and_velocities total_time delta_time friction)
  (let loop ([current_time 0]
             [current_angle initial_angle]  ;; theta: angle in radians, nb: from vertical (rest)
             [current_velocity initial_velocity] ;; theta_dot: angular velocity
             [ret (list (cons (radians->degrees initial_angle) initial_velocity))]) ;; initial pair
    (cond
      [(>= current_time total_time)
       (reverse ret)]
      [else
       (let* ([next_angle (+ current_angle (* current_velocity delta_time))]
              [next_acceleration (get-next-acceleration current_angle current_velocity friction)]
              [next_velocity (+ current_velocity (* next_acceleration delta_time))])
         (loop (+ current_time delta_time)
               next_angle
               next_velocity
               (cons (cons (radians->degrees next_angle) next_velocity) ret)))])))

;; total_time: positive seconds, exact
;; delta_time: rational 0.0 < n < 1.0
;; initial_degrees:  -180.0 <= n <= 180.0)
;; initial_velocity: rational
;; list of pairs of angles (in degrees) and velocity (m/sec), one for each unit time
;;
;; 1) without cycles, the sign of velocity alternates negative then positive for graph
;;    motion to the left vs. right
;; 2) when initial_velocity is 0 and friction is > 0, angles in output are always
;;    between -180.0 and 180.0 due to resistance, making resistance 0 seems to expose
;;    noise with some swings exceeding the initial_degrees
;; 3) when initial_velocity is -1 and friction is 0.1, the first couple swings exceed
;;    initial_degrees both negative and positive before shrinking
;; 4) when initial_velocity is 1 and friction is 0.1, the first swing goes up briefly before
;;    -9.8 m/s of gravity overcomes it and the next couple swings exceed initial_degrees both
;;    negative and positive before shrinking
;; 5) when initial_velocity is -4.13 the pendulum wraps more than two times before velocity
;;    goes from negative to positive at -498.1287469355684 degrees:
;;    dynamics.rkt> (/ (degrees->radians -498.1287469355684) pi) -2.767381927419825
;;    and it settles out toward -360 or two cycles away
(define (all_angles_and_velocities total_time delta_time initial_degrees initial_vel friction)
    (-> exact-positive-integer?
        (and/c (>/c 0) (</c 1))
        (and/c (>=/c -180.0) (<=/c 180.0))
        rational?
        rational?
        (listof (cons/c exact-integer? rational?)))
  (let loop ([current_time 0]
             [current_radians (degrees->radians initial_degrees)] ;; theta: angle radians from vert
             [current_vel initial_vel] ;; theta_dot: velocity m/sec^2
             [ret (list (cons initial_degrees initial_vel))]) ;; initial pair
    (cond
      [(>= current_time total_time)
       (reverse ret)]
      [else
       (let* ([next_angle (+ current_radians (* current_vel delta_time))]
              [next_acceleration (get-next-acceleration current_radians current_vel friction)]
              [next_vel (+ current_vel (* next_acceleration delta_time))])
         (loop (+ current_time delta_time)
               next_angle
               next_vel
               (cons (cons (radians->degrees next_angle) next_vel) ret)))])))



