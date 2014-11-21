;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Project) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
; Game Project
; Shawn Seymour & Zach Litzinger

; Test

(define width 400)
(define height 300)
(define speed 5)
(define gravity 5)
(define blank-scene (empty-scene width height))

(define-struct player [x y img scale])
(define player-1 (make-player 200 150 (circle 10 "solid" "red") 2))

(define (main duration)
  (big-bang player-1
            [to-draw show]
            [on-tick tick .1 duration]
            [on-key key-handler]))

(define (show ws)
  (place-image (scale (player-scale ws) (player-img ws)) (player-x ws) (player-y ws) blank-scene))

(define (tick ws)
  (cond
    [(< (player-y ws) height) (make-player (player-x ws) (+ (player-y ws) (* gravity (player-scale ws))) (player-img ws) (player-scale ws))]
    [else ws]))

(define (jump ws)
  (make-player (player-x ws) (- (player-y ws) speed) (player-img ws) (player-scale ws)))

(define (move ws direction)
  (make-player (+ (* speed direction) (player-x ws)) (player-y ws) (player-img ws) (player-scale ws)))

(define (key-handler ws a-key)
  (cond
    [(key=? "w" a-key) (jump ws)]
    [(key=? "a" a-key) (move ws -1)]
    [(key=? "d" a-key) (move ws 1)]
    [else ws]))

(main 100)