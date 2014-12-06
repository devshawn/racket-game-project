;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname game) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
; Ludum Dare 31
; devshawn

(define width 400)
(define height 300)
(define blank-scene (empty-scene width height))
(define speed 10)
(define world-scale 2)

(define-struct player [x y])
(define-struct keys [left right up down])
(define-struct world [player keys])

(define default-player (make-player 200 150))
(define default-keys (make-keys false false false false))

(define (main duration)
  (big-bang (make-world default-player default-keys)
            [to-draw show]
            [on-tick tick 0.02 duration]
            [on-key key-handler]
            [on-release key-release-handler]))

(define (show ws)
  (place-image (scale world-scale (circle 10 "solid" "red")) (player-x (world-player ws)) (player-y (world-player ws)) (scale world-scale blank-scene)))

(define (tick ws)
  (make-world (move ws) (world-keys ws)))

(define (move ws)
  (cond
    [(keys-left (world-keys ws)) (make-player (- (player-x (world-player ws)) speed) (player-y (world-player ws)))]
    [(keys-right (world-keys ws)) (make-player (+ (player-x (world-player ws)) speed) (player-y (world-player ws)))]
    [(keys-up (world-keys ws)) (make-player (player-x (world-player ws)) (- (player-y (world-player ws)) speed))]
    [(keys-down (world-keys ws)) (make-player (player-x (world-player ws)) (+ (player-y (world-player ws)) speed))]
    [else (world-player ws)]))

(define (key-handler ws a-key)
  (cond
    [(key=? "w" a-key) (make-world (world-player ws) (set-key (world-keys ws) "w" true))]
    [(key=? "a" a-key) (make-world (world-player ws) (set-key (world-keys ws) "a" true))]
    [(key=? "s" a-key) (make-world (world-player ws) (set-key (world-keys ws) "s" true))]
    [(key=? "d" a-key) (make-world (world-player ws) (set-key (world-keys ws) "d" true))]
    [else ws]))

(define (key-release-handler ws a-key)
  (cond
    [(key=? "w" a-key) (make-world (world-player ws) (set-key (world-keys ws) "w" false))]
    [(key=? "a" a-key) (make-world (world-player ws) (set-key (world-keys ws) "a" false))]
    [(key=? "s" a-key) (make-world (world-player ws) (set-key (world-keys ws) "s" false))]
    [(key=? "d" a-key) (make-world (world-player ws) (set-key (world-keys ws) "d" false))]
    [else ws]))

; world-keys, string, boolean -> keys structure
(define (set-key lok key state)
  (cond
    [(string=? key "w") (make-keys (keys-left lok) (keys-right lok) state (keys-down lok))]
    [(string=? key "a") (make-keys state (keys-right lok) (keys-up lok) (keys-down lok))]
    [(string=? key "s") (make-keys (keys-left lok) (keys-right lok) (keys-up lok) state)]
    [(string=? key "d") (make-keys (keys-left lok) state (keys-up lok) (keys-down lok))]))

  

(main 1000000000)