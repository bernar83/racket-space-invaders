(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 4)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 20)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; =================
;; Data definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 INVADER-X-SPEED))                 ;not landed, moving right
(define I2 (make-invader 150 HEIGHT (- INVADER-X-SPEED)))        ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) INVADER-X-SPEED))       ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
(define LOI-1 empty)
(define LOI-2 (cons I1 empty))
(define LOI-3 (list I1 I2))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: false
;; - compound: (cons Invader ListOfInvader)
;; - reference: (first loi) is Invader
;; - self-reference: (rest loi) is ListOfInvader


;; ListOfMissile is one of:
;; empty
;; - (cons Missile ListOfMissile)
(define LOM-1 empty)
(define LOM-2 (cons M1 empty))
(define LOM-3 (list M1 M2))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: false
;; - compound: (cons Missile ListOfMissile)
;; - reference: (first lom) is Missile
;; - self-reference: (rest lom) is ListOfMissile


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

(define N0 0)




;; =================
;; Functions:

;; Game -> Game
;; start the world with (main (make-game empty empty (make-tank (/ WIDTH 2) 0)))
;; 
(define (main g)
  (big-bang g                                ; Game
            (on-tick     tock)               ; Game -> Game
            (to-draw     render)             ; Game -> Image
            (stop-when   landed)
            (on-release  on-space); Game -> Boolean
            (on-key      on-press)           ; Game KeyEvent -> Game
            ))         ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next game state
(check-expect (tock (make-game empty empty (make-tank (/ WIDTH 2) 0))) (make-game (cons (make-invader (+ 100 INVADER-X-SPEED) (+ 0 INVADER-Y-SPEED) INVADER-X-SPEED) empty)
                                                                                  empty
                                                                                  (make-tank (/ WIDTH 2) 0)))
(check-expect (tock (make-game LOI-2 empty (make-tank (/ WIDTH 2) 0))) (make-game (cons (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) INVADER-X-SPEED) empty)
                                                                                  empty
                                                                                  (make-tank (/ WIDTH 2) 0)))
;(check-expect (tock (make-game empty (make-missile 100 35) (make-tank (/ WIDTH 2) 0))) (make-game empty (make-missile 100 (+ 35 MISSILE-SPEED)) (make-tank (/ WIDTH 2) 0)))

;(define (tock g) G0) ;stub

(define (tock g)
  (check-hit (make-game (next-invaders (game-invaders g))
                        (next-missiles (game-missiles g))
                        (next-tank (game-tank g)))))


;; Game -> Game
;; Check if any list of missiles hits and remove both if so
(check-expect (check-hit (make-game (cons (make-invader 250 10 INVADER-X-SPEED) empty) (cons (make-missile 10 250) empty) (make-tank 10 0)))
              (make-game (cons (make-invader 250 10 INVADER-X-SPEED) empty) (cons (make-missile 10 250) empty) (make-tank 10 0)))
(check-expect (check-hit (make-game (cons (make-invader 140 250 INVADER-X-SPEED) empty) (cons (make-missile (- 140 HIT-RANGE) (+ 250 HIT-RANGE)) empty) (make-tank 10 0)))
              (make-game empty empty (make-tank 10 0)))
(check-expect (check-hit (make-game (cons (make-invader 10 10 INVADER-X-SPEED) empty) (cons (make-missile 10 (+ 10 HIT-RANGE)) empty) (make-tank 10 0)))
              (make-game empty empty (make-tank 10 0)))
(check-expect (check-hit (make-game (cons (make-invader 200 450 INVADER-X-SPEED) empty) (cons (make-missile (+ 200 HIT-RANGE) (- 450 HIT-RANGE)) empty) (make-tank 10 0)))
              (make-game empty empty (make-tank 10 0)))

;(define (check-hit g) G1) ;stub

(define (check-hit g)
  (make-game (loop-invaders (game-invaders g) (game-missiles g))
             (loop-missiles (game-invaders g) (game-missiles g))
             (game-tank g)))


;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; process through list of invaders to see if it hits any missiles
;; <check-expects from check-hit>

;(define (loop-invaders loi lom) loi) ;stub

(define (loop-invaders loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (invader-hit? (first loi) lom) 
             (loop-invaders (rest loi) lom)
             (cons (first loi) (loop-invaders (rest loi) lom)))]))


;; Invader ListOfMissile -> Boolean
;(define (invader-hit? i m) false) ;stub

(define (invader-hit? i lom)
  (cond [(empty? lom) false]
        [else
         (if (hit? i (first lom))
             true
             (invader-hit? i (rest lom)))]))


;; Invader ListOfMissiles -> Boolean
;; produce true if missile is in range of invader, false otherwise

(define (hit? invader missile)
  (and (and (<= (- (invader-x invader) HIT-RANGE) (missile-x missile))
            (>= (+ (invader-x invader) HIT-RANGE) (missile-x missile)))
       (and (<= (- (invader-y invader) HIT-RANGE) (missile-y missile))
            (>= (+ (invader-y invader) HIT-RANGE) (missile-y missile)))))


;; ListOfInvaders ListOfMissiles -> ListOfMissiles
;; process through list of missiles to see if it hits any invaders
;; <check-expects from check-hit>

;(define (loop-missiles loi lom) lom) ;stub

(define (loop-missiles loi lom)
  (cond [(empty? lom) empty]
        [else
         (if (missile-hit? (first lom) loi) 
             (loop-missiles loi (rest lom))
             (cons (first lom) (loop-missiles loi (rest lom))))]))


;; Missile ListOfInvader -> Boolean
;; produce true if x,y of missile is within range of invader's hit range

(define (missile-hit? m loi)
  (cond [(empty? loi) false]
        [else
         (if (hit? (first loi) m)
             true
             (missile-hit? m (rest loi)))]))


;; ListOfInvader -> ListOfInvader
;; produce the next list of invaders
(check-expect (next-invaders empty) (cons (make-invader (+ 100 INVADER-X-SPEED) (+ 0 INVADER-Y-SPEED) INVADER-X-SPEED) empty)) 

;(define (next-invaders loi) LOI-1) ;stub

(define (next-invaders loi)
  (move-invaders (make-the-invaders loi)))


;; ListOfInvader -> ListOfInvader
;; check if invader rate is correct to produce invader
(check-expect (make-the-invaders empty) (cons (make-invader 100 0 INVADER-X-SPEED) empty))
(check-expect (make-the-invaders (cons (make-invader 100 67 INVADER-X-SPEED) empty)) (cons (make-invader 100 67 INVADER-X-SPEED) empty))

;(define (check-invade-rate loi) I1) ;stub

(define (make-the-invaders loi)
  (cond [(empty? loi) (cons (make-invader 100 0 INVADER-X-SPEED) empty)]
        [else
         (if (check-rate (random INVADE-RATE))
             (list* (make-invader (random WIDTH) 0 INVADER-X-SPEED) loi)
             loi)]))


;; Number -> Boolean
;; produce true if invade-rate is equal to generated number
(check-expect (check-rate 13) true)
(check-expect (check-rate 19) false)

;(define (check-rate n) false) ;stub

(define (check-rate n)
  (= n 13))


;; ListOfInvader -> ListOfInvader
;; move invaders
(check-expect (move-invaders empty) empty)
(check-expect (move-invaders (cons (make-invader 100 0 INVADER-X-SPEED) empty)) (cons (make-invader (+ 100 INVADER-X-SPEED) (+ 0 INVADER-Y-SPEED) INVADER-X-SPEED) empty))

;(define (move-invaders loi) I1) ;stub

(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (add-speed (first loi))
               (move-invaders (rest loi)))]))


;; Invader -> Invader
;; add speed to invader's x,y coordinates and switch dx when it hits wall
(check-expect (add-speed I1) (make-invader (+ (invader-x I1) INVADER-X-SPEED) (+ (invader-y I1) INVADER-Y-SPEED) INVADER-X-SPEED))
(check-expect (add-speed (make-invader 298.5 250 INVADER-X-SPEED)) (make-invader (+ 298.5 INVADER-X-SPEED) (+ 250 INVADER-Y-SPEED) INVADER-X-SPEED)) ;going to the wall
(check-expect (add-speed (make-invader 1.5 100 (- INVADER-X-SPEED))) (make-invader 0 (+ 100 INVADER-Y-SPEED) (- INVADER-X-SPEED)))
(check-expect (add-speed (make-invader 300 250 INVADER-X-SPEED)) (make-invader WIDTH 250 (- INVADER-X-SPEED))) ;tries to go past wall
(check-expect (add-speed (make-invader 0 100 (- INVADER-X-SPEED))) (make-invader 0 100 INVADER-X-SPEED))

;(define (add-speed i) I1) ;stub

(define (add-speed invader)
  (cond [(> (+ (invader-dx invader) (invader-x invader)) WIDTH) (make-invader WIDTH (invader-y invader) (- (invader-dx invader)))]
        [(< (+ (invader-dx invader) (invader-x invader)) 0) (make-invader 0 (invader-y invader) (- (invader-dx invader)))]
        [else
         (make-invader (+ (invader-dx invader) (invader-x invader)) (+ INVADER-Y-SPEED (invader-y invader)) (invader-dx invader))]))


;; ListOfMissile -> ListOfMissile
;; produce the next list of missile
(check-expect (next-missiles empty) empty)
(check-expect (next-missiles LOM-2) (cons (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)) empty))
(check-expect (next-missiles (cons (make-missile 45 5) empty)) empty)
(check-expect (next-missiles (cons (make-missile 45 5) (cons (make-missile 140 200) empty))) (cons (make-missile 140 (- 200 MISSILE-SPEED)) empty))

;(define (next-missiles lom) LOM-1) ;stub

(define (next-missiles lom)
  (remove-missiles (move-missiles lom)))


;; ListOfMissile -> ListOfMissile
;; Move list of missiles y coord
;; <use check-expects from next-missiles>
(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (add-missile-speed (first lom))
               (next-missiles (rest lom)))]))


;; Missile -> Missile
;; produce the next missile's y coord
(check-expect (add-missile-speed (make-missile 450 120)) (make-missile 450 (- 120 MISSILE-SPEED)))

;(define (add-missile-speed m) M1) ;stub

(define (add-missile-speed m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; ListOfMissile -> ListOfMissile
;; remove missile from list of missiles if move past top screen
(check-expect (remove-missiles empty) empty)
(check-expect (remove-missiles (cons (make-missile 67 -5) empty)) empty)
(check-expect (remove-missiles (cons (make-missile 120 65) (cons (make-missile 65 -5) empty))) (cons (make-missile 120 65) empty))

;(define (remove-missiles lom) LOM-1) ;stub

(define (remove-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (past-zero? (first lom))
             (remove (first lom) (remove-missiles (rest lom))) 
             (cons (first lom) (remove-missiles (rest lom))))]))


;; Missile -> Boolean
;; produce true if missile y coord is less than 0 and false otherwise
(check-expect (past-zero? (make-missile 171 0)) false)
(check-expect (past-zero? (make-missile 82 10)) false)
(check-expect (past-zero? (make-missile 23 -10)) true)

;(define (past-zero? m) false) ;stub

(define (past-zero? m)
  (< (missile-y m) 0))


;; Tank -> Tank
;; produce the next tank, if tank is greater than width than produce tank at width, if tank x is less than 0 then produce tank at 0
(check-expect (next-tank (make-tank 170 0)) (make-tank 170 0))
(check-expect (next-tank (make-tank -2 -1)) (make-tank 0 -1))
(check-expect (next-tank (make-tank 302 1)) (make-tank WIDTH 1))

;(define (next-tank t) T0) ;stub

(define (next-tank t)
  (cond [(< (tank-x t) 0) (make-tank 0 (tank-dir t))]
        [(> (tank-x t) WIDTH) (make-tank WIDTH (tank-dir t))]
        [else
         (make-tank (tank-x t) (tank-dir t))]))


;; Game -> Image
;; render the game state 
(check-expect (render G1) (place-image TANK (tank-x (game-tank G1)) HEIGHT BACKGROUND))
(check-expect (render G2) (place-image TANK (tank-x (game-tank G1)) HEIGHT
                                       (place-image INVADER (invader-x (first (game-invaders G2))) (invader-y (first (game-invaders G2)))
                                                    (place-image MISSILE (missile-x (first (game-missiles G2))) (missile-y (first (game-missiles G2))) BACKGROUND))))

;(define (render g) BACKGROUND) ;stub

(define (render g)
  (render-tank-on (game-tank g) (render-invaders-on (game-invaders g) (render-missiles (game-missiles g)))))


;; Tank Image -> Image
;; produce tank on the given image
(check-expect (render-tank-on T1 (place-image INVADER (invader-x (first (game-invaders G2))) (invader-y (first (game-invaders G2)))
                                                    (place-image MISSILE (missile-x (first (game-missiles G2))) (missile-y (first (game-missiles G2))) BACKGROUND)))
              (place-image TANK (tank-x (game-tank G1)) HEIGHT
                                       (place-image INVADER (invader-x (first (game-invaders G2))) (invader-y (first (game-invaders G2)))
                                                    (place-image MISSILE (missile-x (first (game-missiles G2))) (missile-y (first (game-missiles G2))) BACKGROUND))))

;(define (render-tank-on t img) BACKGROUND) ;stub

(define (render-tank-on t img)
  (place-image TANK (tank-x t) HEIGHT img))


;; ListOfInvaders Image -> Image
;; produce list of invaders on given image
(check-expect (render-invaders-on LOI-2 (place-image MISSILE (missile-x (first (game-missiles G2))) (missile-y (first (game-missiles G2))) BACKGROUND))
              (place-image INVADER (invader-x (first (game-invaders G2))) (invader-y (first (game-invaders G2)))
                                                    (place-image MISSILE (missile-x (first (game-missiles G2))) (missile-y (first (game-missiles G2))) BACKGROUND)))
(check-expect (render-invaders-on empty (place-image MISSILE (missile-x (first (game-missiles G2))) (missile-y (first (game-missiles G2))) BACKGROUND))
              (place-image MISSILE (missile-x (first (game-missiles G2))) (missile-y (first (game-missiles G2))) BACKGROUND))

;(define (render-invaders-on loi img) BACKGROUND) ;stub

(define (render-invaders-on loi img)
  (cond [(empty? loi) img]
        [else
         (place-invader (first loi) (render-invaders-on (rest loi) img))]))


;; Invader Image -> Image
;; render the image of inavder on an image
(check-expect (place-invader I1 (place-image MISSILE (missile-x (first (game-missiles G2))) (missile-y (first (game-missiles G2))) BACKGROUND))
              (place-image INVADER (invader-x (first (game-invaders G2))) (invader-y (first (game-invaders G2)))
                                                    (place-image MISSILE (missile-x (first (game-missiles G2))) (missile-y (first (game-missiles G2))) BACKGROUND)))

;(define (place-invader inv img) BACKGROUND) ;stub

(define (place-invader invader img)
  (place-image INVADER (invader-x invader) (invader-y invader) img))


;; ListOfMissiles -> Image
;; produce list of missiles on background
(check-expect (render-missiles (game-missiles G2)) (place-image MISSILE (missile-x (first (game-missiles G2))) (missile-y (first (game-missiles G2))) BACKGROUND))

;(define (render-missiles lom) BACKGROUND) ;stub

(define (render-missiles lom)
  (cond [(empty? lom) BACKGROUND]
        [else
         (place-missile (first lom) (render-missiles (rest lom)))]))


;; Missile Image -> Image
;; produce the missile on an image
(check-expect (place-missile (first (game-missiles G2)) BACKGROUND) (place-image MISSILE (missile-x (first (game-missiles G2))) (missile-y (first (game-missiles G2))) BACKGROUND))

;(define (place-missile m img) BACKGROUND) ;stub

(define (place-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))


;; Game -> Boolean
;; Stop game when invader lands (y coord >= to HEIGHT)
(check-expect (landed G2) false)
(check-expect (landed G3) true)
(check-expect (landed (make-game (list I3) LOM-2 T0)) true)

;(define (landed g) false) ;stub

(define (landed g)
  (check-list (game-invaders g)))


;; ListOfInvaders -> Boolean
;; produce true if any invader is greater than height
(check-expect (check-list empty) false)
(check-expect (check-list (list I1)) false)
(check-expect (check-list (list I1 I2)) true)

;(define (check-list loi) false) ;stub

(define (check-list loi)
  (cond [(empty? loi) false]
        [else
         (if (past? (first loi))
             true
             (check-list (rest loi)))]))


;; Invader -> Boolean
;; produce true if the invader is past height
(check-expect (past? I2) true)
(check-expect (past? I1) false)

;(define (past? i) false) ;stub

(define (past? invader)
  (or (> (invader-y invader) HEIGHT) (= (invader-y invader) HEIGHT)))


;; Game KeyEvent -> Game
;; increase x when going right, decrease x when going left, none otherwise
(check-expect (on-press G1 "b") (make-game (game-invaders G1) (game-missiles G1) (game-tank G1)))
(check-expect (on-press G1 "right") (make-game (game-invaders G1) (game-missiles G1) (make-tank (+ TANK-SPEED (tank-x (game-tank G1))) 1)))
(check-expect (on-press G1 "left") (make-game (game-invaders G1) (game-missiles G1) (make-tank (- (tank-x (game-tank G1)) TANK-SPEED) -1)))

;(define (on-press g sp) G0) ;stub

(define (on-press g ke)
  (cond [(key=? ke "right") (make-game (game-invaders g) (game-missiles g) (make-tank (+ TANK-SPEED (tank-x (game-tank g))) 1))]
        [(key=? ke "left")  (make-game (game-invaders g) (game-missiles g) (make-tank (- (tank-x (game-tank g)) TANK-SPEED) -1))]
        [else g]))


;; Game KeyEvent -> Game
;; Create missile when pressing space and none otherwise
(check-expect (on-space (make-game empty empty (make-tank 150 1)) " ") (make-game empty (cons (make-missile 150 (- HEIGHT 10)) empty) (make-tank 150 1)))
(check-expect (on-space (make-game empty (cons (make-missile 12 450) empty) (make-tank 12 1)) " ")
              (make-game empty (cons (make-missile 12 (- HEIGHT 10)) (cons (make-missile 12 450) empty)) (make-tank 12 1)))
(check-expect (on-space (make-game empty empty (make-tank 175 -1)) "b") (make-game empty empty (make-tank 175 -1)))

(define (on-space g ke)
   (cond [(key=? ke " ") (make-game (game-invaders g) (cons (make-missile (tank-x (game-tank g)) (- HEIGHT 10)) (game-missiles g)) (game-tank g))]
         [else g]))
