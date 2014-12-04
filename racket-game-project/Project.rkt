; Game Project
; Shawn Seymour & Zach Litzinger

; TO-DO List
; End Game
; Start-screen: rules
; Enemy speed: add to structure
; Items / power-ups [optional]
; Easter egg

; Constants
(define speed 4)
(define bullet-speed 1)
(define bullet-damage 5)
(define bullet-limit 2)
(define enemyspeed 1)
(define spawn-speed .03)
(define spawn-speed2 .005)
(define spawn-speed3 .005)
(define spawn-speed4 0.001)
(define gravity 5)
(define blank-scene (scale 1.75 (bitmap "images/bg.png")));(rectangle width height "solid" "lightblue"))
(define worldscale 1)
(define font 36)
(define bullet-img (bitmap "images/bullet.png"))
(define enemy1img (bitmap "images/enemy1.png"))
(define enemy2img (bitmap "images/enemy2.png"))
(define wizardimg (bitmap "images/wizard.png"))
(define giantimg (bitmap "images/giant.png"))
(define endscreen (bitmap "images/intro.png"))
(define-struct player [x y img scale points health])
(define-struct enemy [x y img type health scale])
(define-struct keys [left right up down pause])
(define-struct world [player bullets enemies keys killed-enemies]) ; Player struct, list of posns, list of enemies structs
(define player-1 (make-player (/ (image-width blank-scene) 2) (* (/ (image-height blank-scene) 4) 3)  (bitmap "images/player.png") 1.25 0 6))

; main: Number -> World
; Creates a world of our game that will last a given duration
(define (main duration)
  (big-bang (make-world player-1 empty empty (make-keys false false false false false) empty) 
            [to-draw show]
            [on-tick tick .01 duration]
            [on-key key-push-handler]
            [stop-when check-end final-scene]
            [on-release key-release-handler]))

; show: World structure -> Image
; Uses helper functions to display the game
(define (show ws)
  (scale worldscale (place-hearts (place-points font (world-player ws) (place-player ws (place-enemies (world-enemies ws) (place-bullet (world-bullets ws) blank-scene)))))))

(define fadedhealth (overlay/xy (bitmap "images/fadedheart.png") 30 0 (overlay/xy (bitmap "images/fadedheart.png") 30 0 (overlay/xy (bitmap "images/fadedheart.png") 30 0 (empty-scene 0 0)))))

(define (place-hearts ws)
  (place-image fadedhealth (- (image-width ws) 50) 24 ws))
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

; tick: World structure -> World structure
; on-tick function of our big-bang.
; Creates our world using multiple functions that change the game
(define (tick ws)
  (cond
    [(keys-pause (world-keys ws)) ws]
    [else (kill-enemy (make-world 
              (player-hit (move ws) (world-enemies ws))
              (return-bullets (offscreen-bullets (move-bullets (world-bullets ws))) (world-enemies ws))
              (return-enemies (world-bullets ws) (offscreen-enemies (move-enemies (create-enemy (delete-enemy-on-hit (world-player ws) (world-enemies ws))))))
              (world-keys ws)
              (world-killed-enemies ws)))]))

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

; move-bullets: List of bullets -> List of bullets
; Moves all bullets based on the bullet-speed constant
(define (move-bullets lob)
  (cond
    [(empty? lob) empty]
    [else (cons (make-posn (posn-x (first lob)) (- (posn-y (first lob)) bullet-speed)) (move-bullets (rest lob)))]))

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

; limit-player-x: Number -> Number
; Limits the player's movement in the x-axis
(define (limit-player-x n)
  (cond [(>= n (image-width blank-scene)) (image-width blank-scene)]
        [(<= n 0) 0]
        [else n]))

; limit-player-y: Number -> Number
; Limits the player's movement in the y-axis
(define (limit-player-y n)
  (cond [(>= n (image-height blank-scene)) (image-height blank-scene)]
        [(<= n 0) 0]
        [else n]))

; shoot: World structure -> World structure
; Re-makes the world structure adding a bullet
(define (shoot ws)
  (cond
    [(keys-pause (world-keys ws)) ws]
    [else (make-world (world-player ws) (add-bullet ws) (world-enemies ws) (world-keys ws) (world-killed-enemies ws))]))

; add-bullet: World structure -> List of bullets
; Adds a new bullet shot from the current player position
; to the current list of bullets
(define (add-bullet ws)
  (if (< (length (world-bullets ws)) bullet-limit) (cons (make-posn 
         (player-x (world-player ws)) 
         (- (player-y (world-player ws)) (* (player-scale (world-player ws)) (/ (image-height (player-img (world-player ws))) 2)))) 
        (world-bullets ws)) (world-bullets ws)))

; add-enemy: List of enemies -> List of enemies
; Adds an enemy to the current list of enemies
(define (add-enemy name loe)
  (cond
    [(string=? name "enemy1") (cons (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) -20 enemy1img 10 5 1) loe)]
    [(string=? name "enemy2") (cons (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) -20 enemy2img 20 10 1) loe)]
    [(string=? name "wizard") (cons (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) -20 wizardimg 50 15 1) loe)]
    [(string=? name "giant") (cons (make-enemy (* (random (floor (/ (image-width blank-scene) 10))) 10) -20 giantimg 100 20 1) loe)]
    [else loe]))

; create-enemy: List of enemies -> List of enemies
; Creates an enemy based on the spawn-speed probability
(define (create-enemy loe)
  (cond
    [(< (/ (random 100) 100) spawn-speed) (add-enemy "enemy1" loe)]
    [(< (/ (random 100) 100) spawn-speed2) (add-enemy "wizard" loe)]
    [(< (/ (random 100) 100) spawn-speed3) (add-enemy "enemy2" loe)]
    [(< (/ (random 100) 100) spawn-speed4) (add-enemy "giant" loe)]
    [else loe]))

; offscreen-enemies: List of enemies -> List of enemies
; Removes enemies that have gone off of the visible screen (del (points ws))
(define (offscreen-enemies loe)
  (cond
    [(empty? loe) empty]
    [(> (enemy-y (first loe)) (+ (image-height blank-scene) (image-height (enemy-img (first loe))))) (offscreen-enemies (rest loe))]
    [else (cons (first loe) (offscreen-enemies (rest loe)))]))

; offscreen-bullets: List of bullets -> List of bullets
; Removes bullets that have gone off of the visible screen
(define (offscreen-bullets lob)
  (cond
    [(empty? lob) empty]
    [(< (posn-y (first lob)) (- 0 (image-height bullet-img))) (offscreen-bullets (rest lob))]
    [else (cons (first lob) (offscreen-bullets (rest lob)))]))

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

; delete-enemy-on-hit
(define (delete-enemy-on-hit player loe)
  (cond
    [(empty? loe) empty]
    [(and (<= (player-x player) (+ (enemy-x (first loe)) (image-width (enemy-img (first loe)))))
          (>= (player-x player) (- (enemy-x (first loe)) (image-width (enemy-img (first loe)))))
          (<= (player-y player) (+ (enemy-y (first loe)) (/ (image-height (enemy-img (first loe))) 1.5)))
          (>= (player-y player) (- (enemy-y (first loe)) (/ (image-height (enemy-img (first loe))) 1.5))))
     (delete-enemy-on-hit player (rest loe))]
    [else (cons (first loe) (delete-enemy-on-hit player (rest loe)))]))

; player-hit: Player, list of enemies -> Player
; Hurts player when hit
(define (player-hit player loe)
  (cond
    [(empty? loe) player]
    [(and (<= (player-x player) (+ (enemy-x (first loe)) (image-width (enemy-img (first loe)))))
          (>= (player-x player) (- (enemy-x (first loe)) (image-width (enemy-img (first loe)))))
          (<= (player-y player) (+ (enemy-y (first loe)) (/ (image-height (enemy-img (first loe))) 1.5)))
          (>= (player-y player) (- (enemy-y (first loe)) (/ (image-height (enemy-img (first loe))) 1.5))))
     (make-player (player-x player) (player-y player) (player-img player) (player-scale player) (player-points player) (- (player-health player) 1))]
    [else (player-hit player (rest loe))]))

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

(define (kill-enemy ws)
  (cond
    [else (make-world (world-player ws)
                 (world-bullets ws)
                 (delete-enemies (world-enemies ws))
                 (world-keys ws)
                 (append (filter-enemies (world-enemies ws)) (world-killed-enemies ws)))]))

(define (delete-enemies loe)
  (cond
    [(empty? loe) empty]
    [(= (enemy-health (first loe)) 0) (delete-enemies (rest loe))]
    [else (cons (first loe) (delete-enemies (rest loe)))]))

(define (filter-enemies loe)
  (cond
    [(empty? loe) empty]
    [(= (enemy-health (first loe)) 0) (cons (first loe) (filter-enemies (rest loe)))]
    [else (filter-enemies (rest loe))]))

(define (count-points loke)
  (cond
    [(empty? loke) 0]
    [else (+ (enemy-type (first loke)) (count-points (rest loke)))]))

(define (pause ws)
  (cond
    [(keys-pause (world-keys ws)) (make-world 
                                (world-player ws) 
                                (world-bullets ws) 
                                (world-enemies ws) 
                                (make-keys (keys-left (world-keys ws)) (keys-right (world-keys ws)) (keys-up (world-keys ws)) (keys-down (world-keys ws)) false)
                                (world-killed-enemies ws))]
    [else (make-world 
                                (world-player ws) 
                                (world-bullets ws) 
                                (world-enemies ws) 
                                (make-keys (keys-left (world-keys ws)) (keys-right (world-keys ws)) (keys-up (world-keys ws)) (keys-down (world-keys ws)) true)
                                (world-killed-enemies ws))]))

; return-bullets: List of posns, list of enemies -> list of enemies
; Recursively uses enemy-hit to determine if any enemies have been hit
(define (return-enemies lob loe)
  (cond
    [(empty? lob) loe]
    [else (return-enemies (rest lob) (enemy-hit (first lob) loe))]))

; return-bullets: List of posns, list of enemies -> list of posns
; Recursively uses bullet-hit to determine if any bullets hit any enemies
(define (return-bullets lob loe)
  (cond
    [(empty? loe) lob]
    [else (return-bullets (bullet-hit lob (first loe)) (rest loe))]))

(define (check-end ws)
  (cond
    [(<= (player-health (world-player ws)) 0) true]
    [else false]))

(define (final-scene ws)
  (place-image (scale worldscale (scale 1.75 (rectangle (image-width blank-scene) (image-height blank-scene) "solid" (make-color 132 132 132 150)))) (/ (image-width blank-scene) 2) (/ (image-height blank-scene) 2) (show ws)))

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
                             (world-killed-enemies ws))]
    [(or (key=? "s" a-key)
         (key=? "down" a-key))(make-world 
                               (world-player ws) 
                               (world-bullets ws) 
                               (world-enemies ws) 
                               (make-keys (keys-left (world-keys ws)) (keys-right (world-keys ws)) (keys-up (world-keys ws)) true (keys-pause (world-keys ws)))
                               (world-killed-enemies ws))]
    [(or (key=? "a" a-key)
         (key=? "left" a-key))(make-world 
                               (world-player ws) 
                               (world-bullets ws) 
                               (world-enemies ws) 
                               (make-keys true (keys-right (world-keys ws)) (keys-up (world-keys ws)) (keys-down (world-keys ws)) (keys-pause (world-keys ws)))
                               (world-killed-enemies ws))]
    [(or (key=? "d" a-key)
         (key=? "right" a-key))(make-world 
                                (world-player ws) 
                                (world-bullets ws) 
                                (world-enemies ws) 
                                (make-keys (keys-left (world-keys ws)) true (keys-up (world-keys ws)) (keys-down (world-keys ws)) (keys-pause (world-keys ws)))
                                (world-killed-enemies ws))]
    [(key=? " " a-key) (shoot ws)]
    [(key=? "p" a-key) (pause ws)]
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
                             (world-killed-enemies ws))]
    [(or (key=? "s" a-key)
         (key=? "down" a-key))(make-world 
                               (world-player ws) 
                               (world-bullets ws) 
                               (world-enemies ws) 
                               (make-keys (keys-left (world-keys ws)) (keys-right (world-keys ws)) (keys-up (world-keys ws)) false (keys-pause (world-keys ws)))
                               (world-killed-enemies ws))]
    [(or (key=? "a" a-key)
         (key=? "left" a-key))(make-world 
                               (world-player ws) 
                               (world-bullets ws) 
                               (world-enemies ws) 
                               (make-keys false (keys-right (world-keys ws)) (keys-up (world-keys ws)) (keys-down (world-keys ws)) (keys-pause (world-keys ws)))
                               (world-killed-enemies ws))]
    [(or (key=? "d" a-key)
         (key=? "right" a-key))(make-world 
                                (world-player ws) 
                                (world-bullets ws) 
                                (world-enemies ws) 
                                (make-keys (keys-left (world-keys ws)) false (keys-up (world-keys ws)) (keys-down (world-keys ws)) (keys-pause (world-keys ws)))
                                (world-killed-enemies ws))]
    [else ws]))

(main 10000000)


;intro ~ to do

(define rules (text "Hello" 10 "white"))

(define intro 
  (place-image 
   rules (/ (image-width (bitmap "images/intro.png")) 2) (/ (image-height (bitmap "images/intro.png")) 2) (bitmap "images/intro.png")))
