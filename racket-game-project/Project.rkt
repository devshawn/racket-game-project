;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Project) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
; Medievala
; 1301 Game Project
; Shawn Seymour & Zach Litzinger

; Uploaded at:
; http://github.com/devshawn/racket-game-project

; Structures
(define-struct player [x y img scale points health])
(define-struct enemy [x y img type health scale])
(define-struct keys [left right up down pause])
(define-struct world [player bullets enemies keys killed-enemies started]) ; Player struct, list of posns, list of enemies structs

; Constants
(define worldscale 1)
(define game-name "Medievala")

(define speed 4)
(define bullet-speed 1)
(define bullet-damage 5)
(define bullet-limit 2)
(define enemyspeed 1)
(define spawn-speed .03)
(define spawn-speed2 .005)
(define spawn-speed3 .005)
(define spawn-speed4 0.001)
(define spawn-speed5 0.000000001)
(define spawn-tier-enemy2 100)
(define spawn-tier-wizard 300)
(define spawn-tier-giant 600)
(define spawn-tier-secret 2000)


; Image Constants
(define blank-scene (scale 1.75 (bitmap "images/bg.png")))
(define bullet-img (bitmap "images/bullet.png"))
(define enemy1img (bitmap "images/enemy1.png"))
(define enemy2img (bitmap "images/Enemy2.png"))
(define wizardimg (bitmap "images/Wizard.png"))
(define giantimg (bitmap "images/Giant.png"))
(define endscreen (bitmap "images/intro.png"))
(define secretimg (bitmap "images/secret.png"))
(define rightheart (bitmap "images/rightheart.png"))
(define leftheart (bitmap "images/leftheart.png"))
(define startscreen (scale worldscale (scale 1.75 (bitmap "images/intro.png"))))
(define gamename (scale worldscale (text game-name 40 "white")))
(define rules (scale worldscale (above
                                 (text "You get two shots at a time! Press the spacebar to shoot!" 20 "white")
                                 (text "Use WASD or the arrows to move!" 20 "white")
                                 (beside (text "The " 20 "white") enemy1img (text " is the Knight; worth 10 points!" 20 "white"))
                                 (beside (text "The " 20 "white") enemy2img (text " is the Black Knight; worth 20 points!" 20 "white"))
                                 (beside (text "The " 20 "white") wizardimg (text " is the Wizard; worth 50 points!" 20 "white"))
                                 (beside (text "The " 20 "white") giantimg (text " is the Giant; worth 100 points!" 20 "white"))
                                 (text "Survive for as long as you can!" 20 "white")
                                 (text "Press X to start!" 20 "white")
                                 )))
(define player-1 (make-player (/ (image-width blank-scene) 2) (* (/ (image-height blank-scene) 4) 3)  (bitmap "images/player.png") 1.25 0 6))

; main: Number -> World
; Creates a world of our game that will last a given duration
(define (main duration)
  (big-bang (make-world player-1 empty empty (make-keys false false false false false) empty false) 
            [to-draw show]
            [on-tick tick .01 duration]
            [on-key key-push-handler]
            [stop-when check-end final-scene]
            [on-release key-release-handler]
            [name game-name]))

; show: World structure -> Image
; Uses helper functions to display the game
(define (show ws)
  (show-start ws (scale worldscale (place-hearts ws (place-points 36 (world-player ws) (place-player ws (place-enemies (world-enemies ws) (place-bullet (world-bullets ws) blank-scene))))))))

; show-start: World structure, image -> Image
; Shows the start screen until the game begins
(define (show-start ws base)
  (cond
    [(not (world-started ws)) (place-image gamename (/ (image-width startscreen) 2) 50 
                                           (place-image rules (/ (image-width startscreen) 2) (+ (/ (image-height startscreen) 2) 30) startscreen))]
    [else base]))

; heart-health: Player -> Image
; Makes an image of hearts depending on how much player health is left
(define (heart-health player)
  (cond [(>= (player-health player) 6) (beside leftheart rightheart leftheart rightheart leftheart rightheart)]
        [(>= (player-health player) 5) (beside rightheart leftheart rightheart leftheart rightheart)]
        [(>= (player-health player) 4) (beside leftheart rightheart leftheart rightheart)]
        [(>= (player-health player) 3) (beside rightheart leftheart rightheart)]
        [(>= (player-health player) 2) (beside leftheart rightheart)]
        [(>= (player-health player) 1) rightheart]
        [else (square 0 "solid" "white")]))

; faded-health: World structure -> Image
; Creates an image of faded hearts to go behind
; the "normal" hearts
(define (faded-health ws) 
  (overlay/align "right" "middle" (heart-health (world-player ws))
                 (beside 
                  (bitmap "images/fadedheart.png") 
                  (bitmap "images/fadedheart.png")  
                  (bitmap "images/fadedheart.png") 
                  (empty-scene 0 0))))

; place-hearts: World structure, image -> Image
; Places the heart images onto a base image
(define (place-hearts ws base)
  (place-image (faded-health ws) (- (image-width base) 50) 24 base))

; place-bullet: List of bullets, image -> Image
; Places all bullets on top of a base image
(define (place-bullet lob base)
  (cond
    [(empty? lob) base]
    [else (place-image bullet-img (posn-x (first lob)) (posn-y (first lob)) (place-bullet (rest lob) base))]))

; place-points: Number, player, image -> Image
; Places the points in the top right corner
(define (place-points font-size player base)
  (place-image (text (number->string (player-points player)) font-size "white") 
               (+ (/ (image-width (text (number->string (player-points player)) font-size "white")) 2) 5)
               24 base))

; place-enemies: List of enemies, image -> Image
; Places all enemies on top of a base image
(define (place-enemies loe base)
  (cond
    [(empty? loe) base]
    [else (place-image (enemy-img (first loe)) (enemy-x (first loe)) (enemy-y (first loe)) (place-enemies (rest loe) base))]))

; place-player: World structure, image -> Image
; Places a player on top of a base image
(define (place-player ws base)
  (place-image (scale (player-scale (world-player ws)) (player-img (world-player ws))) (player-x (world-player ws)) (player-y (world-player ws)) base))

; final-scene: World structure -> Image
; Creates the final scene on end game
(define (final-scene ws)
  (place-image (end-text (world-player ws)) (/ (image-width (scale worldscale blank-scene)) 2) (/ (image-height (scale worldscale blank-scene)) 2)
               (place-image (scale worldscale (scale 1.75 (rectangle (image-width blank-scene) (image-height blank-scene) "solid" (make-color 132 132 132 150)))) (/ (image-width blank-scene) 2) (/ (image-height blank-scene) 2) (show ws))))

; end-text: Player -> Image
; Creates the end game text from player points
(define (end-text player)
  (scale worldscale (above
                     (text "Congratulations!" 40 "white")
                     (text (string-append "You finished with " (number->string (player-points player)) " points!") 35 "white")
                     (text " " 25 "white")
                     (text (string-append "Thank you for playing " game-name "!") 30 "white"))))

; tick: World structure -> World structure
; on-tick function of our big-bang.
; Creates our world using multiple functions that change the game
(define (tick ws)
  (cond
    [(not (world-started ws)) ws]
    [(keys-pause (world-keys ws)) ws]
    [else (kill-enemy (make-world 
                       (move ws)
                       (offscreen-bullets (move-bullets (world-bullets ws)))
                       (offscreen-enemies (move-enemies (create-enemy (world-player ws) (delete-enemy-on-hit (world-player ws) (world-enemies ws)))))
                       (world-keys ws)
                       (world-killed-enemies ws)
                       (world-started ws)))]))

; move-enemies: List of enemies -> List of enemies
; Moves all enemies based on the enemyspeed constant
(define (move-enemies loe)
  (cond
    [(empty? loe) empty]
    [else (cons (make-enemy 
                 (enemy-x (first loe)) 
                 (+ (enemy-y (first loe)) enemyspeed) 
                 (enemy-img (first loe)) 
                 (enemy-type (first loe)) 
                 (enemy-health (first loe)) 
                 (enemy-scale (first loe))) 
                (move-enemies (rest loe)))]))

(check-expect (move-enemies (list (make-enemy 0 0 0 0 0 0)))  (list (make-enemy 0 enemyspeed 0 0 0 0)))
(check-expect (move-enemies (list (make-enemy 0 0 0 0 0 0) (make-enemy 0 0 0 0 0 0)))  (list (make-enemy 0 enemyspeed 0 0 0 0) (make-enemy 0 enemyspeed 0 0 0 0)))
(check-expect (move-enemies (list (make-enemy 0 6 0 0 0 0) (make-enemy 0 1 0 5 6 7)))  (list (make-enemy 0 (+ 6 enemyspeed) 0 0 0 0) (make-enemy 0 (+ 1 enemyspeed) 0 5 6 7)))


; move-bullets: List of bullets -> List of bullets
; Moves all bullets based on the bullet-speed constant
(define (move-bullets lob)
  (cond
    [(empty? lob) empty]
    [else (cons (make-posn (posn-x (first lob)) (- (posn-y (first lob)) bullet-speed)) (move-bullets (rest lob)))]))

(check-expect (move-bullets (list (make-posn 0 0)))  (list (make-posn 0 (- 0 bullet-speed))))
(check-expect (move-bullets (list (make-posn 0 0) (make-posn 0 0)))  (list (make-posn 0 (- 0 bullet-speed)) (make-posn 0 (- 0 bullet-speed))))
(check-expect (move-bullets (list (make-posn 0 6) (make-posn 0 1)))  (list (make-posn 0 (- 6 bullet-speed)) (make-posn 0 (- 1 bullet-speed))))

; move: World structure -> Player
; Checks the world-keys for keys-pushed. Moves the player
; based on which "keys" are true (pushed).
(define (move ws)
  (make-player
   (cond
     [(keys-left (world-keys ws)) (limit-player-x (- (player-x (world-player ws)) speed))]
     [(keys-right (world-keys ws)) (limit-player-x (+ (player-x (world-player ws)) speed))]
     [else (player-x (world-player ws))])
   (cond
     [(keys-up (world-keys ws)) (limit-player-y (- (player-y (world-player ws)) speed))]
     [(keys-down (world-keys ws)) (limit-player-y (+ (player-y (world-player ws)) speed))]
     [else (player-y (world-player ws))])
   (player-img (world-player ws))
   (player-scale (world-player ws))
   (count-points (world-killed-enemies ws))
   (player-health (world-player ws))))

(check-expect (move (make-world player-1 empty empty (make-keys false false false false false) empty false)) player-1)
(check-expect (move (make-world player-1 empty empty (make-keys true false false true false) empty false)) 
              (make-player (- (/ (image-width blank-scene) 2) speed) (+ (* (/ (image-height blank-scene) 4) 3) speed)  (bitmap "images/player.png") 1.25 0 6))
(check-expect (move (make-world player-1 empty empty (make-keys false true true false false) empty false))
              (make-player (+ (/ (image-width blank-scene) 2) speed) (- (* (/ (image-height blank-scene) 4) 3) speed)  (bitmap "images/player.png") 1.25 0 6))

; limit-player-x: Number -> Number
; Limits the player's movement in the x-axis
(define (limit-player-x n)
  (cond [(>= n (image-width blank-scene)) (image-width blank-scene)]
        [(<= n 0) 0]
        [else n]))

(check-expect (limit-player-x -1) 0)
(check-expect (limit-player-x 0) 0)
(check-expect (limit-player-x 5) 5)
(check-expect (limit-player-x 10000000) (image-width blank-scene))

; limit-player-y: Number -> Number
; Limits the player's movement in the y-axis
(define (limit-player-y n)
  (cond [(>= n (image-height blank-scene)) (image-height blank-scene)]
        [(<= n 0) 0]
        [else n]))

(check-expect (limit-player-y -1) 0)
(check-expect (limit-player-y 0) 0)
(check-expect (limit-player-y 5) 5)
(check-expect (limit-player-y 10000000) (image-height blank-scene))

; shoot: World structure -> World structure
; Re-makes the world structure adding a bullet
(define (shoot ws)
  (cond
    [(not (world-started ws)) ws]
    [(keys-pause (world-keys ws)) ws]
    [else (make-world (world-player ws) (add-bullet ws) (world-enemies ws) (world-keys ws) (world-killed-enemies ws) (world-started ws))]))

;can't do check-expect because of the (not (world-started ws)) condition

; add-bullet: World structure -> List of bullets
; Adds a new bullet shot from the current player position
; to the current list of bullets
(define (add-bullet ws)
  (if (< (length (world-bullets ws)) bullet-limit) (cons (make-posn 
                                                          (player-x (world-player ws)) 
                                                          (- (player-y (world-player ws)) (* (player-scale (world-player ws)) (/ (image-height (player-img (world-player ws))) 2)))) 
                                                         (world-bullets ws)) (world-bullets ws)))

(check-expect (add-bullet (make-world (make-player 1 2  (bitmap "images/player.png") 1.25 0 6) empty empty (make-keys false false false false false) empty false))
              (list (make-posn 1 -15.5)))
(check-expect (add-bullet (make-world (make-player 6 100  (bitmap "images/player.png") 1.25 0 6) empty empty (make-keys false false false false false) empty false))
              (list (make-posn 6 82.5)))
(check-expect (add-bullet (make-world (make-player 100 40  (bitmap "images/player.png") 1.25 0 6) empty empty (make-keys false false false false false) empty false))
              (list (make-posn 100 22.5)))

; add-enemy: List of enemies -> List of enemies
; Adds an enemy to the current list of enemies
(define (add-enemy name loe)
  (cond
    [(string=? name "enemy1") (cons (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) -20 enemy1img 10 5 1) loe)]
    [(string=? name "enemy2") (cons (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) -20 enemy2img 20 10 1) loe)]
    [(string=? name "wizard") (cons (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) -20 wizardimg 50 15 1) loe)]
    [(string=? name "giant") (cons (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) -20 giantimg 100 20 1) loe)]
    [(string=? name "secret") (cons (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) -20 secretimg 500 50 1) loe)]
    [else loe]))

(check-random (add-enemy "enemy1" empty)
              (list (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) -20 enemy1img 10 5 1)))
(check-random (add-enemy "enemy2" empty)
              (list (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) -20 enemy2img 20 10 1)))
(check-random (add-enemy "wizard" empty)
              (list (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) -20 wizardimg 50 15 1)))
(check-random (add-enemy "giant" empty)
              (list (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) -20 giantimg 100 20 1)))
(check-random (add-enemy "secret" empty)
              (list (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) -20 secretimg 500 50 1)))

; create-enemy: List of enemies -> List of enemies
; Creates an enemy based on the spawn-speed probability
(define (create-enemy player loe)
  (cond
    [(< (/ (random 100) 100) spawn-speed) (add-enemy "enemy1" loe)]
    [(and (>= (player-points player) spawn-tier-enemy2) (< (/ (random 100) 100) spawn-speed2)) (add-enemy "enemy2" loe)]
    [(and (>= (player-points player) spawn-tier-wizard) (< (/ (random 100) 100) spawn-speed3)) (add-enemy "wizard" loe)]
    [(and (>= (player-points player) spawn-tier-giant) (< (/ (random 100) 100) spawn-speed4)) (add-enemy "giant" loe)]
    [(and (>= (player-points player) spawn-tier-secret) (< (/ (random 100) 100) spawn-speed5)) (add-enemy "secret" loe)]
    [else loe]))

;no way to use check-expect or check-random because of many different random tiers

; offscreen-enemies: List of enemies -> List of enemies
; Removes enemies that have gone off of the visible screen (del (points ws))
(define (offscreen-enemies loe)
  (cond
    [(empty? loe) empty]
    [(> (enemy-y (first loe)) (+ (image-height blank-scene) (image-height (enemy-img (first loe))))) (offscreen-enemies (rest loe))]
    [else (cons (first loe) (offscreen-enemies (rest loe)))]))

(check-expect (offscreen-enemies empty) empty)
(check-expect (offscreen-enemies (list (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) 100000 secretimg 500 50 1))) empty)
(check-random (offscreen-enemies (list (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) 100 secretimg 500 50 1))) 
              (list (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) 100 secretimg 500 50 1)))

; offscreen-bullets: List of bullets -> List of bullets
; Removes bullets that have gone off of the visible screen
(define (offscreen-bullets lob)
  (cond
    [(empty? lob) empty]
    [(< (posn-y (first lob)) (- 0 (image-height bullet-img))) (offscreen-bullets (rest lob))]
    [else (cons (first lob) (offscreen-bullets (rest lob)))]))

(check-expect (offscreen-bullets empty) empty)
(check-expect (offscreen-bullets (list (make-posn 10 10))) (list (make-posn 10 10)))
(check-expect (offscreen-bullets (list (make-posn 10 -100))) empty)

; COLLISION

; enemy-hit: bullet, list of enemies -> List of enemies
; Checks all enemies for touching one bullet. If they touch,
; this function hurts or removes the enemy and returns the
; rest of the enemy list
(define (enemy-hit bullet loe)
  (cond
    [(empty? loe) empty]
    [(and (<= (posn-x bullet) (+ (enemy-x (first loe)) (image-width (enemy-img (first loe)))))
          (>= (posn-x bullet) (- (enemy-x (first loe)) (image-width (enemy-img (first loe)))))
          (<= (posn-y bullet) (+ (enemy-y (first loe)) (image-height (enemy-img (first loe)))))
          (>= (posn-y bullet) (- (enemy-y (first loe)) (image-height (enemy-img (first loe))))))
     (hurt-enemy (first loe) (enemy-hit bullet (rest loe)))]
    [else (cons (first loe) (enemy-hit bullet (rest loe)))]))

(check-expect (enemy-hit (make-posn 10 10) empty) empty)
(check-expect (enemy-hit (make-posn 10 10) (list (make-enemy 10 10 secretimg 500 50 1))) (list (make-enemy 10 10 secretimg 500 45 1)))
(check-expect (enemy-hit (make-posn 1000 10) (list (make-enemy 10 10 secretimg 500 50 1))) (list (make-enemy 10 10 secretimg 500 50 1)))

; bullet-hit: List of bullets, enemy -> List of bullets
; Checks all bullets for touching one enemy. If they touch,
; this function removes the bullet and returns the
; rest of the bullet list
(define (bullet-hit lob enemy)
  (cond
    [(empty? lob) empty]
    [(and (<= (posn-x (first lob)) (+ (enemy-x enemy) (image-width (enemy-img enemy))))
          (>= (posn-x (first lob)) (- (enemy-x enemy) (image-width (enemy-img enemy))))
          (<= (posn-y (first lob)) (+ (enemy-y enemy) (image-height (enemy-img enemy))))
          (>= (posn-y (first lob)) (- (enemy-y enemy) (image-height (enemy-img enemy)))))
     (bullet-hit (rest lob) enemy)]
    [else (cons (first lob) (bullet-hit (rest lob) enemy))]))

(check-expect (bullet-hit (list (make-posn 10 10)) (make-enemy 10 10 secretimg 500 50 1)) empty)
(check-expect (bullet-hit (list (make-posn 1000 10)) (make-enemy 10 10 secretimg 500 50 1)) (list (make-posn 1000 10)))
(check-expect (bullet-hit (list (make-posn 10 10)) (make-enemy 20 10 secretimg 500 50 1)) empty)

; delete-enemy-on-hit: Player, list of enemies -> List of enemies
; Deletes the enemy when hit by a player
(define (delete-enemy-on-hit player loe)
  (cond
    [(empty? loe) empty]
    [(player-enemy-touch? player (first loe)) (delete-enemy-on-hit player (rest loe))]
    [else (cons (first loe) (delete-enemy-on-hit player (rest loe)))]))

(check-expect (delete-enemy-on-hit (make-player 100 400  (bitmap "images/player.png") 1.25 0 6) (list (make-enemy 20 10 secretimg 500 50 1))) 
              (list (make-enemy 20 10 secretimg 500 50 1)))
(check-expect (delete-enemy-on-hit (make-player 20 15 (bitmap "images/player.png") 1.25 0 6) (list (make-enemy 20 10 secretimg 500 50 1))) empty)
(check-expect (delete-enemy-on-hit (make-player 10 15 (bitmap "images/player.png") 1.25 0 6) 
                                   (list (make-enemy 300 300 secretimg 500 50 1) (make-enemy 200 10 secretimg 500 50 1))) (list (make-enemy 300 300 secretimg 500 50 1) (make-enemy 200 10 secretimg 500 50 1)))

; player-hit: Player, list of enemies -> Player
; Hurts player when player touches an enemy
(define (player-hit player loe)
  (cond
    [(empty? loe) player]
    [(player-enemy-touch? player (first loe)) (make-player (player-x player) (player-y player) (player-img player) (player-scale player) (player-points player) (- (player-health player) 1))]
    [else (player-hit player (rest loe))]))

(check-expect (player-hit (make-player 100 40  (bitmap "images/player.png") 1.25 0 6)
                          (list (make-enemy 300 300 secretimg 500 50 1)))
              (make-player 100 40  (bitmap "images/player.png") 1.25 0 6))
(check-expect (player-hit (make-player 100 40  (bitmap "images/player.png") 1.25 0 6)
                          (list (make-enemy 300 300 secretimg 500 50 1) (make-enemy 100 35 secretimg 500 50 1)))
              (make-player 100 40  (bitmap "images/player.png") 1.25 0 5))
(check-expect (player-hit (make-player 100 43  (bitmap "images/player.png") 1.25 0 6)
                          (list (make-enemy 101 41 secretimg 500 50 1)))
              (make-player 100 43  (bitmap "images/player.png") 1.25 0 5))

; player-enemy-touch?: Player, enemy -> Boolean
; Checks if an enemy and a player are colliding
(define (player-enemy-touch? player enemy)
  (and (<= (player-x player) (+ (enemy-x enemy) (image-width (enemy-img enemy))))
       (>= (player-x player) (- (enemy-x enemy) (image-width (enemy-img enemy))))
       (<= (player-y player) (+ (enemy-y enemy) (/ (image-height (enemy-img enemy)) 1.5)))
       (>= (player-y player) (- (enemy-y enemy) (/ (image-height (enemy-img enemy)) 1.5)))))

(check-expect (player-enemy-touch? (make-player 100 40  (bitmap "images/player.png") 1.25 0 6) 
                                   (make-enemy 100 41 secretimg 500 50 1))
              true)
(check-expect (player-enemy-touch? (make-player 10 40  (bitmap "images/player.png") 1.25 0 6) 
                                   (make-enemy 100 401 secretimg 500 50 1))
              false)
(check-expect (player-enemy-touch? (make-player 100 40  (bitmap "images/player.png") 1.25 0 6) 
                                   (make-enemy 100 401 secretimg 500 50 1))
              false)


; hurt-enemy: enemy, a function -> a functiondered with last-
; This is a bit of an abstract function. It either
; hurts an enemy or doesn't return an enemy
; Uses "recursive" parameter to pass the recursive function
; from it's parent function enemy-hit
(define (hurt-enemy enemy recursive)
  (cond
    [(= (enemy-health enemy) 0) (cons enemy recursive)]
    [else (cons (make-enemy 
                 (enemy-x enemy) 
                 (enemy-y enemy) 
                 (enemy-img enemy) 
                 (enemy-type enemy) 
                 (- (enemy-health enemy) bullet-damage) 
                 (enemy-scale enemy)) 
                recursive)]))

; kill-enemy: World structure -> World structure
; Checks for kileld enemies and moves it to killed-enemies
; Also runs the collision functions
(define (kill-enemy ws)
  (make-world (player-hit (world-player ws) (world-enemies ws))
              (return-bullets (world-bullets ws) (world-enemies ws))
              (return-enemies (world-bullets ws) (delete-enemies (world-enemies ws)))
              (world-keys ws)
              (append (filter-enemies (world-enemies ws)) (world-killed-enemies ws))
              (world-started ws)))



; delete-enemies: List of enemies -> List of enemies
; Deletes enemy from world-enemies when enemy health is zero
(define (delete-enemies loe)
  (cond
    [(empty? loe) empty]
    [(= (enemy-health (first loe)) 0) (delete-enemies (rest loe))]
    [else (cons (first loe) (delete-enemies (rest loe)))]))

(check-expect (delete-enemies empty) empty)
(check-expect (delete-enemies (list (make-enemy 10 10 secretimg 500 0 1))) empty)
(check-expect (delete-enemies (list (make-enemy 10 10 secretimg 500 1 1))) (list (make-enemy 10 10 secretimg 500 1 1)))

; filter-enemies: List of enemies -> List of enemies
; "Moves" killed enemies to killed-enemies in the world structure
(define (filter-enemies loe)
  (cond
    [(empty? loe) empty]
    [(<= (enemy-health (first loe)) 0) (cons (first loe) (filter-enemies (rest loe)))]
    [else (filter-enemies (rest loe))]))

(check-expect (filter-enemies empty) empty)
(check-expect (filter-enemies (list (make-enemy 10 10 secretimg 500 0 1))) (list (make-enemy 10 10 secretimg 500 0 1)))
(check-expect (filter-enemies (list (make-enemy 10 10 secretimg 500 1 1))) empty)

; count-points: List of killed enemies -> Number
; Adds the points of each killed enemy
(define (count-points loke)
  (cond
    [(empty? loke) 0]
    [else (+ (enemy-type (first loke)) (count-points (rest loke)))]))

(check-expect (count-points (list (make-enemy 20 10 secretimg 500 50 1) (make-enemy 20 10 secretimg 500 50 1))) 1000)
(check-expect (count-points (list (make-enemy 20 10 secretimg 130 50 1) (make-enemy 20 10 secretimg 500 50 1))) 630)
(check-expect (count-points (list (make-enemy 20 10 secretimg -500 50 1) (make-enemy 20 10 secretimg 500 50 1))) 0)

; return-enemies: List of posns, list of enemies -> list of enemies
; Recursively uses enemy-hit to determine if any enemies have been hit
(define (return-enemies lob loe)
  (cond
    [(empty? lob) loe]
    [else (return-enemies (rest lob) (enemy-hit (first lob) loe))]))

(check-expect (return-enemies (list (make-posn 500 50)) (list (make-enemy 20 10 secretimg 500 50 1)))
              (list (make-enemy 20 10 secretimg 500 50 1)))
(check-expect (return-enemies (list (make-posn 20 10)) (list (make-enemy 20 10 secretimg 500 50 1)))
              (list (make-enemy 20 10 secretimg 500 45 1)))
(check-expect (return-enemies (list (make-posn 50 50) (make-posn 300 300)) (list (make-enemy 50 50 secretimg 500 50 1) (make-enemy 200 100 secretimg 500 50 1)))
              (list (make-enemy 50 50 secretimg 500 45 1) (make-enemy 200 100 secretimg 500 50 1)))


; return-bullets: List of posns, list of enemies -> list of posns
; Recursively uses bullet-hit to determine if any bullets hit any enemies
(define (return-bullets lob loe)
  (cond
    [(empty? loe) lob]
    [else (return-bullets (bullet-hit lob (first loe)) (rest loe))]))

(check-expect (return-bullets (list (make-posn 500 50)) (list (make-enemy 20 10 secretimg 500 50 1)))
              (list (make-posn 500 50)))
(check-expect (return-bullets (list (make-posn 20 10)) (list (make-enemy 20 10 secretimg 500 50 1)))
              empty)
(check-expect (return-bullets (list (make-posn 50 50) (make-posn 300 300)) (list (make-enemy 50 50 secretimg 500 50 1) (make-enemy 200 100 secretimg 500 50 1)))
              (list (make-posn 300 300)))


; check-end: World structure -> Boolean
; Checks if the player has been killed
; Used to trigger end game in big-bang
(define (check-end ws)
  (<= (player-health (world-player ws)) 0))

(check-expect (check-end (make-world (make-player 1 2  (bitmap "images/player.png") 1.25 0 0) empty empty (make-keys false false false false false) empty false)) true)
(check-expect (check-end (make-world (make-player 1 2  (bitmap "images/player.png") 1.25 0 3) empty empty (make-keys false false false false false) empty false)) false)
(check-expect (check-end (make-world (make-player 1 2  (bitmap "images/player.png") 1.25 0 -1) empty empty (make-keys false false false false false) empty false)) true)

; key-push-handler: World structure, key -> World structure
; Takes a world structure and changes the "world-key" based on which
; key was pushed
(define (key-push-handler ws a-key)
  (cond
    [(or (key=? "w" a-key)
         (key=? "up" a-key))(make-world 
                             (world-player ws) 
                             (world-bullets ws) 
                             (world-enemies ws) 
                             (make-keys (keys-left (world-keys ws)) (keys-right (world-keys ws)) true (keys-down (world-keys ws)) (keys-pause (world-keys ws)))
                             (world-killed-enemies ws)
                             (world-started ws))]
    [(or (key=? "s" a-key)
         (key=? "down" a-key))(make-world 
                               (world-player ws) 
                               (world-bullets ws) 
                               (world-enemies ws) 
                               (make-keys (keys-left (world-keys ws)) (keys-right (world-keys ws)) (keys-up (world-keys ws)) true (keys-pause (world-keys ws)))
                               (world-killed-enemies ws)
                               (world-started ws))]
    [(or (key=? "a" a-key)
         (key=? "left" a-key))(make-world 
                               (world-player ws) 
                               (world-bullets ws) 
                               (world-enemies ws) 
                               (make-keys true (keys-right (world-keys ws)) (keys-up (world-keys ws)) (keys-down (world-keys ws)) (keys-pause (world-keys ws)))
                               (world-killed-enemies ws)
                               (world-started ws))]
    [(or (key=? "d" a-key)
         (key=? "right" a-key))(make-world 
                                (world-player ws) 
                                (world-bullets ws) 
                                (world-enemies ws) 
                                (make-keys (keys-left (world-keys ws)) true (keys-up (world-keys ws)) (keys-down (world-keys ws)) (keys-pause (world-keys ws)))
                                (world-killed-enemies ws)
                                (world-started ws))]
    [(key=? " " a-key) (shoot ws)]
    [(and (key=? "p" a-key)
          (world-started ws)) (make-world 
                               (world-player ws) 
                               (world-bullets ws) 
                               (world-enemies ws) 
                               (make-keys (keys-left (world-keys ws)) (keys-right (world-keys ws)) (keys-up (world-keys ws)) (keys-down (world-keys ws)) (not (keys-pause (world-keys ws))))
                               (world-killed-enemies ws)
                               (world-started ws))]
    [(key=? "x" a-key) (make-world 
                        (world-player ws) 
                        (world-bullets ws) 
                        (world-enemies ws) 
                        (world-keys ws)
                        (world-killed-enemies ws)
                        true)]
    [else ws]))

; key-release-handler: World structure, key -> World structure
; Takes a world structure and changes the "world-key" based on which
; key was pushed
(define (key-release-handler ws a-key)
  (cond
    [(or (key=? "w" a-key)
         (key=? "up" a-key))(make-world 
                             (world-player ws) 
                             (world-bullets ws) 
                             (world-enemies ws) 
                             (make-keys (keys-left (world-keys ws)) (keys-right (world-keys ws)) false (keys-down (world-keys ws)) (keys-pause (world-keys ws)))
                             (world-killed-enemies ws)
                             (world-started ws))]
    [(or (key=? "s" a-key)
         (key=? "down" a-key))(make-world 
                               (world-player ws) 
                               (world-bullets ws) 
                               (world-enemies ws) 
                               (make-keys (keys-left (world-keys ws)) (keys-right (world-keys ws)) (keys-up (world-keys ws)) false (keys-pause (world-keys ws)))
                               (world-killed-enemies ws)
                               (world-started ws))]
    [(or (key=? "a" a-key)
         (key=? "left" a-key))(make-world 
                               (world-player ws) 
                               (world-bullets ws) 
                               (world-enemies ws) 
                               (make-keys false (keys-right (world-keys ws)) (keys-up (world-keys ws)) (keys-down (world-keys ws)) (keys-pause (world-keys ws)))
                               (world-killed-enemies ws)
                               (world-started ws))]
    [(or (key=? "d" a-key)
         (key=? "right" a-key))(make-world 
                                (world-player ws) 
                                (world-bullets ws) 
                                (world-enemies ws) 
                                (make-keys (keys-left (world-keys ws)) false (keys-up (world-keys ws)) (keys-down (world-keys ws)) (keys-pause (world-keys ws)))
                                (world-killed-enemies ws)
                                (world-started ws))]
    [else ws]))

(main 10000000000)