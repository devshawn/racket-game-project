;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Project) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
; Game Project
; Shawn Seymour & Zach Litzinger

; Constants
(define width 400)
(define height 300)
(define speed 10)
(define bullet-speed 1)
(define bullet-damage 5)
(define enemyspeed 1)
(define spawn-speed .05)
(define gravity 5)
(define blank-scene (empty-scene width height))

(define bullet-img (rectangle 5 15 "solid" "red"))
(define enemyimg (rectangle 20 15 "solid" "green"))
(define-struct player [x y img scale])
(define-struct enemy [x y img type health scale])
(define-struct world [player bullets enemies]) ; Player struct, list of posns, list of enemies structs
(define player-1 (make-player 200 150 (circle 10 "solid" "blue") 1))

(define (main duration)
  (big-bang (make-world player-1 empty empty) 
            [to-draw show]
            [on-tick tick .01 duration]
            [on-key key-handler]))

(define (show ws)
  (place-player ws (place-enemies (world-enemies ws) (place-bullet (world-bullets ws) blank-scene))))

; place-bullet: List of bullets, image -> Image
(define (place-bullet lob base)
  (cond
    [(empty? lob) base]
    [else (place-image bullet-img (posn-x (first lob)) (posn-y (first lob)) (place-bullet (rest lob) base))]))

(define (place-enemies loe base)
  (cond
    [(empty? loe) base]
    [else (place-image (enemy-img (first loe)) (enemy-x (first loe)) (enemy-y (first loe)) (place-enemies (rest loe) base))]))
  
(define (create-enemy loe)
  (if (< (/ (random 100) 100) spawn-speed) (add-enemy loe) loe))

(define (place-player ws base)
  (place-image (scale (player-scale (world-player ws)) (player-img (world-player ws))) (player-x (world-player ws)) (player-y (world-player ws)) base))

(define (tick ws) 
  (make-world (world-player ws) (return-bullets (offscreen-bullets (move-bullets (world-bullets ws))) (world-enemies ws)) (return-enemies (world-bullets ws) (offscreen-enemies (move-enemies (create-enemy (world-enemies ws)))))))

(define (move-enemies loe)
  (cond
    [(empty? loe) empty]
    [else (cons (make-enemy (enemy-x (first loe)) (+ (enemy-y (first loe)) enemyspeed) (enemy-img (first loe)) (enemy-type (first loe)) (enemy-health (first loe)) (enemy-scale (first loe))) (move-enemies (rest loe)))]))

(define (move-bullets lob)
  (cond
    [(empty? lob) empty]
    [else (cons (make-posn (posn-x (first lob)) (- (posn-y (first lob)) bullet-speed)) (move-bullets (rest lob)))]))

(define (move-player ws direction axis)
  (if (string=? axis "x") (make-player (+ (* speed direction) (player-x ws)) (player-y ws) (player-img ws) (player-scale ws))
      (make-player (player-x ws) (+ (* speed direction) (player-y ws)) (player-img ws) (player-scale ws))))

(define (move ws direction axis)
  (make-world (move-player (world-player ws) direction axis) (world-bullets ws) (world-enemies ws)))

(define (shoot ws)
  (make-world (world-player ws) (add-bullet ws) (world-enemies ws)))

(define (add-bullet ws)
  (cons (make-posn (player-x (world-player ws)) (- (player-y (world-player ws)) (* (player-scale (world-player ws)) (/ (image-height (player-img (world-player ws))) 2)))) (world-bullets ws)))

(define (add-enemy loe)
  (cons (make-enemy (* (random (/ width 10)) 10) -20 enemyimg 1 10 1) loe))

(define (offscreen-enemies loe)
  (cond
    [(empty? loe) empty]
    [(> (enemy-y (first loe)) (+ height (image-height (enemy-img (first loe))))) (offscreen-enemies (rest loe))]
    [else (cons (first loe) (offscreen-enemies (rest loe)))]))

(define (offscreen-bullets lob)
  (cond
    [(empty? lob) empty]
    [(< (posn-y (first lob)) (- 0 (image-height bullet-img))) (offscreen-bullets (rest lob))]
    [else (cons (first lob) (offscreen-bullets (rest lob)))]))

; COLLISION
; enemy-hit: posn, list of enemies -> list of enemies
; width: 10
; enemy: 250
; bullet: 245
; 245-255
(define (enemy-hit bullet loe)
  (cond
    [(empty? loe) empty]
    [(and (<= (posn-x bullet) (+ (enemy-x (first loe)) (image-width (enemy-img (first loe)))))
          (>= (posn-x bullet) (- (enemy-x (first loe)) (image-width (enemy-img (first loe)))))
          (<= (posn-y bullet) (+ (enemy-y (first loe)) (image-height (enemy-img (first loe)))))
          (>= (posn-y bullet) (- (enemy-y (first loe)) (image-height (enemy-img (first loe))))))
     (enemy-hit bullet (rest loe))]
    [else (cons (first loe) (enemy-hit bullet (rest loe)))]))

(define (bullet-hit lob enemy)
  (cond
    [(empty? lob) empty]
    [(and (<= (posn-x (first lob)) (+ (enemy-x enemy) (image-width (enemy-img enemy))))
          (>= (posn-x (first lob)) (- (enemy-x enemy) (image-width (enemy-img enemy))))
          (<= (posn-y (first lob)) (+ (enemy-y enemy) (image-height (enemy-img enemy))))
          (>= (posn-y (first lob)) (- (enemy-y enemy) (image-height (enemy-img enemy)))))
     (bullet-hit (rest lob) enemy)]
    [else (cons (first lob) (bullet-hit (rest lob) enemy))]))


(define (hurt-enemy enemy)
  (make-enemy (enemy-x enemy) (enemy-y enemy) (enemy-img enemy) (enemy-type enemy) (- (enemy-health enemy) bullet-damage) (enemy-scale enemy)))

(define (return-enemies lob loe)
  (cond
    [(empty? lob) loe]
    [else (return-enemies (rest lob) (enemy-hit (first lob) loe))]))

(define (return-bullets lob loe)
  (cond
    [(empty? loe) lob]
    [else (return-bullets (bullet-hit lob (first loe)) (rest loe))]))

(define (key-handler ws a-key)
  (cond
    [(key=? "w" a-key) (move ws -1 "y")]
    [(key=? "s" a-key) (move ws 1 "y")]
    [(key=? "a" a-key) (move ws -1 "x")]
    [(key=? "d" a-key) (move ws 1 "x")]
    [(key=? " " a-key) (shoot ws)]
    [else ws]))

(main 10000000)