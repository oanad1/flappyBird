 #lang racket/gui

(require 2htdp/image)
(require 2htdp/universe)   
(require lang/posn)

(require "abilities.rkt")
(require "random.rkt") 
(require "constants.rkt")     
;---------------------------------------checker_exports------------------------------------------------
(provide next-state)
(provide next-state-bird)
(provide next-state-bird-onspace) 
(provide change)

(provide get-pipes) (provide get-pipe-x)
(provide next-state-pipes) 
(provide add-more-pipes)
(provide clean-pipes)
(provide move-pipes)
 
(provide invalid-state?)
(provide check-ground-collision) 
(provide check-pipe-collisions)

(provide draw-frame)

(provide get-initial-state)
(provide get-bird)
(provide get-bird-y)
(provide get-bird-v-y)

; pipe
(provide get-pipes)
(provide get-pipe-x)

; score25
(provide get-score)

(provide get-abilities)
(provide get-abilities-visible)
(provide get-abilities-active)
; variables
(provide get-variables)
(provide get-variables-gravity)
(provide get-variables-momentum)
(provide get-variables-scroll-speed)


;---------------------------------------checker_exports------------------------------------------------

;STRUCTURES 
;Structure for a bird
(define-struct bird-struct (x y Vy))

;Structure for a pipe
(define-struct pipe-struct (gap-y x))

;Structure for variables
(define-struct variables-struct (gravity momentum scroll-speed))

;Structure for the game state
(define-struct state-struct (bird pipes variables abilities score))

;Structure for abilities
(define-struct abilities-struct (visible active))

;Structure for initial variables
(define initial-variables (variables-struct initial-gravity initial-momentum initial-scroll-speed))


;INITIAL STATE
;Returns the initial state of the game
;The bird's left upper corner is initially at: y = bird-initial-y, x = bird-x
;The initial score is 0
;The pipes list initially contains 1 pipe, placed outside the visible screen
;A pipe contains an upper and lower part, separated by 'pipe-self-gap'
;The upper left corner of the first pipe is at: y = (+ added-number (random random-threshold)), x = scene-width

(define (get-initial-state)
  (state-struct (bird-struct bird-x bird-initial-y 0)
                (cons (pipe-struct (+ added-number (random random-threshold)) scene-width) null)
                initial-variables (abilities-struct null null) 0))



;Getter which extracts the bird from the state structure
(define (get-bird state)
    (state-struct-bird state)
 )

;Getters which extract the x and y positions of the bird
(define (get-bird-y bird)
  (bird-struct-y bird)
 )

(define (get-bird-x bird)
  (bird-struct-x bird)
 )


;GRAVITY
;Modifies gravity parameters: adds (Vy to y) and (gravity to Vy)
(define (next-state-bird bird gravity)
      (match-let ([(bird-struct _  y Vy) bird])
    (struct-copy bird-struct bird [Vy (+ Vy gravity)] [y (+ y Vy)]))
  )


;Getter which extracts Vy from the bird structure
(define (get-bird-v-y bird)
  (bird-struct-Vy bird)
)


;MOMENTUM
;Applies momentum: the velocity of the bird becomes (-momentum)
(define (next-state-bird-onspace bird momentum)
    (struct-copy bird-struct bird [Vy (* momentum -1)]))


;CHANGE BIRD
;Calls the function next-state-bird-onspace if the key pressed is " "
;Otherwise, returns the current state
(define (change current-state pressed-key)
   (match-let ([(state-struct bird _ variables _ _) current-state])
    (cond [(key=? pressed-key " ")  (struct-copy state-struct current-state
                                      [bird (next-state-bird-onspace bird
                                          (variables-struct-momentum variables))])]
          [else current-state]))
)


;Getter which extracts the list of pipes from the state structure
(define (get-pipes state)
   (state-struct-pipes state))


;Getters which extract the x and y of a pipe structure
(define(get-pipe-x pipe)
  (pipe-struct-x pipe))

(define(get-pipe-y pipe)
  (pipe-struct-gap-y pipe))



;MOVE PIPES
;Subtracts the scroll speed from the x of each pipe in the given list, simulating movement
(define (move-pipes pipes scroll-speed)
    (map (λ (pipe)  (match-let ([(pipe-struct gap-y x) pipe])
         (struct-copy pipe-struct pipe [x (- x scroll-speed)]))) pipes)
  )


;CLEAN PIPES
;Clears all pipes whose upper right corner got outside the visible screen
(define (clean-pipes pipes)
  (filter (λ (pipe) (if (< (+ (get-pipe-x pipe) pipe-width) 0) #f #t)) pipes)
 )


;ADD PIPES
;If there are less than 'no-pipes' pipes in the game, another pipe is added
;The new pipe has: x = pipe-width + pipe-gap + x_of_the_last_added_pipe,
;y = (+ added-number (random random-threshold))
(define (add-more-pipes pipes)
  (cond ((null? pipes) null)
        ((< (length pipes) no-pipes)
        (cons (pipe-struct (+ added-number (random random-threshold))
                         (+ pipe-width pipe-gap (get-pipe-x (car pipes)))) pipes))
        (else pipes)
  ))


;NEXT STATE FOR PIPES
;Moves pipes, clears the ones outside the screen, than adds extra pipes if necessary
(define (next-state-pipes pipes scroll-speed)
  (add-more-pipes (clean-pipes  (move-pipes pipes scroll-speed)))
  )


;Getter which extracts the score from the state structure
(define (get-score state)
  (state-struct-score state))


;CHECK GROUND COLLISION
;Returns true if the bird hits the ground (bird y > ground y)
(define (check-ground-collision bird)
 (if ( >= (+ (get-bird-y bird) bird-height) ground-y)
     #t #f
 ))


;CHECK INVALID STATE
;Checks if the bird collides with the ground or with a pipe
(define (invalid-state? state)
  (if ( or (check-ground-collision (get-bird state))
          (check-pipe-collisions (get-bird state)(get-pipes state)))
    #t #f))


;CHECK PIPE COLLISION
;Constructs the upper left corner and lower right corner of each pipe piece
;Calls check-collision-rectangles on each pipe pice and the bird 
 (define (upper-pipe-up pipe)
      (make-posn (get-pipe-x pipe) 0)
   )

  (define (upper-pipe-down pipe)
      (make-posn (+ (get-pipe-x pipe) pipe-width) (get-pipe-y pipe))
   )

  (define (lower-pipe-up pipe)
      (make-posn (get-pipe-x pipe) (+ (get-pipe-y pipe) pipe-self-gap))
   )

  (define (lower-pipe-down pipe)
      (make-posn (+ (get-pipe-x pipe) pipe-width) scene-height)
   )
  
  (define (check-pipe-collisions bird pipes)
  (let* ((b1 (make-posn (get-bird-x bird) (get-bird-y bird)))
         (b2 (make-posn (+ (posn-x b1) bird-width) (+ (posn-y b1) bird-height))))
  (foldl (λ (pipe collision) (if
                              (or (check-collision-rectangles b1 b2 (upper-pipe-up pipe) (upper-pipe-down pipe))
                                  (check-collision-rectangles b1 b2 (lower-pipe-up pipe) (lower-pipe-down pipe))) #t collision)) #f pipes)
))
  
(define (check-collision-rectangles A1 A2 B1 B2)
  (match-let ([(posn AX1 AY1) A1]
              [(posn AX2 AY2) A2]
              [(posn BX1 BY1) B1]
              [(posn BX2 BY2) B2])
    (and (< AX1 BX2) (> AX2 BX1) (< AY1 BY2) (> AY2 BY1))))



;NEXT STATE
;Returns the state of the next frame, creating the animation effect
;next-state-bird is applied on bird
;next-state-pipes is applied on pipes
;next-variables is called on variables and abbilities are applied
;next-abilities is applied on abilities
;The score is incremented with 0.1
(define (next-state state)
  (match-let ([(state-struct bird pipes variables abilities score) state])
    (let ((new-pipes (next-state-pipes pipes (variables-struct-scroll-speed variables)))
          (new-abilities (next-abilities abilities bird (variables-struct-scroll-speed variables))))

    (struct-copy state-struct state [bird  (next-state-bird bird (variables-struct-gravity variables))]
                  [pipes new-pipes]
                  [variables (next-variables initial-variables abilities)]
                  [abilities new-abilities]
                  [score (+ score 0.1)])))
)
    

;DRAW FRAME
;Draws all elements of the game in this order: active abilities over visible abilities,
;over the bird, over the ground, over the score, over the pipes, over the initial scene
;place-image uses the center of each image, so all images are shifted accordingly

;The bird will be placed at: x = x + bird-width/2, y = y + bird-height/2
(define bird (rectangle bird-width bird-height  "solid" "yellow"))

;The ground will be placed at: x = scene-width/2, y = y + ground-height/2
(define ground (rectangle scene-width ground-height "solid" "brown"))

(define initial-scene (rectangle scene-width scene-height "solid" "white"))
(define text-family (list "Gill Sans" 'swiss 'normal 'bold #f))

;Receives a real number and returns its image
;The score will be placed at text-x text-y
(define (score-to-image x)
(if SHOW_SCORE
	(apply text/font (~v (round x)) 24 "indigo" text-family)
	empty-image))


(define (draw-frame state)
  (place-active-abilities (abilities-struct-active (state-struct-abilities state))
  (place-visible-abilities (abilities-struct-visible (state-struct-abilities state))
  (place-image bird
               (+ (get-bird-x (get-bird state)) (quotient bird-width 2))
               (+ (get-bird-y (get-bird state)) (quotient bird-height 2))
   (place-image ground
   (/ scene-width 2) (+ ground-y (/ ground-height 2))
   (place-image (score-to-image (state-struct-score state)) text-x text-y
   (place-pipes (get-pipes state) initial-scene))))))
)


;PLACE PIPES
;Generates a list of images for the pipes - a pipe image is a pair made out of:
;lower (pipe-width x scene-height - pipe-self-gap - gap-y), upper(pipe-width, gap-y)
;Generates the coordinates for each pair and places the pipes on the scene 

(define (create-pipes pipes)
  (foldr (λ (pipe pipe-images) (cons (rectangle pipe-width (pipe-struct-gap-y pipe) "solid" "green")
                                     (cons (rectangle pipe-width (max (- scene-height pipe-self-gap (get-pipe-y pipe)) 0) "solid" "green")
                                 pipe-images))) null pipes)
)

 (define (get-pipe-coords pipes)
   (foldr (λ (pipe coords) (cons (make-posn (+ (/ pipe-width 2) (get-pipe-x pipe)) (/ (pipe-struct-gap-y pipe) 2))
                              (cons (make-posn (+ (/ pipe-width 2) (get-pipe-x pipe))
                                               (+ pipe-self-gap (get-pipe-y pipe) (/ (- scene-height pipe-self-gap (get-pipe-y pipe)) 2)))
                                    coords)))
          null pipes))
       
(define (place-pipes pipes scene)
  (place-images (create-pipes pipes) (get-pipe-coords pipes) scene)
)


;------------- BONUS

;FAST ABILITY
;Lasts 10 seconds, has initial null position, and modifies scroll-speed as: scroll-speed = max(5, scroll-speed - 1)
(define fast-ability
   (ability-struct (hourglass "mediumseagreen") 10 null (λ (x) (max 5 (- x 1)))))


;SLOW ABILITY
;Lasts 30 seconds, has initial null position, and modifies scroll-speed as: scroll-speed = scroll-speed + 1
(define slow-ability
  (ability-struct (hourglass "tomato") 30 null add1))

;A list of all possible abilities in the game
(define ABILITIES (list slow-ability fast-ability))


;GETTERS
;Getters for game variables
(define get-variables (λ (state) (state-struct-variables state)))
(define get-variables-gravity (λ (state) (variables-struct-gravity (state-struct-variables state))))
(define get-variables-momentum (λ (state) (variables-struct-momentum (state-struct-variables state))))
(define get-variables-scroll-speed (λ (state) (variables-struct-scroll-speed (state-struct-variables state))))


;Getter which extracts abilities from a state structure
(define (get-abilities x) (state-struct-abilities x))

;Getter which extracts visible abilities from an abilities structure
;Visible abilities are the ones which are part of the scene 
(define (get-abilities-visible x) (abilities-struct-visible x))

;Getter which extracts active abilities from an abilities structure
;Active abilities are the ones with which the bird has collided 
(define (get-abilities-active x) (abilities-struct-active x))



;CLEAN ABILITIES
;Clears abilities which are no longer visible on the screen (x + ability-width) < 0
(define (clean-abilities abilities)
    (filter (λ (ability) (if (<= 0 (+ (posn-x (ability-struct-pos ability))
                                     (image-width (ability-struct-image ability)))) #t #f))
                                        abilities)
 )


;MOVE ABILITIES
;Substracts scroll-speed from all visible abilities, shifting them to the left
(define (move-abilities abilities scroll-speed)
  (map (λ (ability) (struct-copy ability-struct ability
                                 [pos (make-posn (- (posn-x (ability-struct-pos ability)) scroll-speed)
                                                 (posn-y (ability-struct-pos ability)))]))
                                       abilities)
 )


;TIME COUNTER
;Subtracts the duration of each frame (1/fps) from the counter of each active ability
;If the time expires for an ability, it is removed from the screen
(define (time-counter abilities)
  (filter (λ (ability) (if (<= (ability-struct-time ability) 0) #f #t))
            (map (λ (ability) (struct-copy ability-struct ability
                                 [time (- (ability-struct-time ability) (/ 1.0 fps))]))
                 abilities))
 )



;NEXT VISIBLE ABILITIES
;Moves abilities and clears the ones that are outside the visible screen
;If there are less than DISPLAYED_ABILITIES on the screen, it calls fill-abilities
;and completes the number of displayed abilities

(define (next-abilities-visible visible scroll-speed)
    (let  ((new-abilities (clean-abilities (move-abilities visible scroll-speed)))
            (nr DISPLAYED_ABILITIES))
           (if (< (length new-abilities) nr)
               (fill-abilities new-abilities nr ABILITIES)
               new-abilities)
      )
 )



;CHECK ABILITY COLLISION
;Checks if the bird has a collision with an ability
;The upper and lower corner of each ability are calculated considering that
;the coordinates are given for the center of each ability

(define (ability-lower-point ability)
  (let ((x (posn-x (ability-struct-pos ability))) (y (posn-y (ability-struct-pos ability))))
          (make-posn (+ x (quotient (image-width (ability-struct-image ability)) 2))
                     (+ y (quotient (image-height (ability-struct-image ability)) 2)))
 ))


(define (ability-upper-point ability)
  (make-posn (- (posn-x (ability-struct-pos ability)) (quotient (image-width (ability-struct-image ability)) 2))
             (- (posn-y (ability-struct-pos ability)) (quotient (image-height (ability-struct-image ability)) 2)))
 )

(define (check-ability-collision bird ability)
   (let* ((b1 (make-posn (get-bird-x bird) (get-bird-y bird)))
          (b2 (make-posn (+ (posn-x b1) bird-width) (+ (posn-y b1) bird-height))))

     (check-collision-rectangles b1 b2 (ability-upper-point ability) (ability-lower-point ability))
 ))

;NEXT ABILITIES
;Calls next-abilities-visible in order to generate the new list of abilities
;The abilities which do not collide with the bird are kept in visible-abilities
;New abilities are generated to complete the number of DISPLAYED ABILITIES
;The abilities which do collide with the bird are moved to active-abilities

(define (next-abilities abilities bird scroll-speed)
  (let ((visible-abilities (next-abilities-visible
                            (abilities-struct-visible abilities) scroll-speed))
        (active-abilities (abilities-struct-active abilities)))
     
  (struct-copy abilities-struct abilities
               [visible (fill-abilities (filter-not (λ (ability) (check-ability-collision bird ability)) visible-abilities)
                                     DISPLAYED_ABILITIES ABILITIES)]
               [active (time-counter (append active-abilities
                                             (filter (λ (ability) (check-ability-collision bird ability)) visible-abilities)))])
 ))



;NEXT VARIABLES
;Applies abilities in order to recalculate the scroll speed
;Applied on initial abilities
(define (next-variables variables abilities)
   (struct-copy variables-struct variables [scroll-speed ((compose-abilities
                                               (map (λ (ability) (ability-struct-next ability))
                                                    (abilities-struct-active abilities)))
    (variables-struct-scroll-speed variables))])
 )


;PLACE VISIBLE ABILITIES
;Places visible abilities on the scene at their given coordinates.
(define (place-visible-abilities abilities scene)
	(place-images (map (λ (ability) (ability-struct-image ability)) abilities)
                      (map (λ (ability) (ability-struct-pos ability)) abilities)
                      scene))


;PLACE ACTIVE ABILITIES
;Active abilities are placed on the upper side of the screen near the score
;Images are scaled with a factor of 0.75 and positioned starting from ability-posn
;with spaces of 50px in between
;Image i is placed at (ability-posn.x - 50*i, ability-posn.y)
(define (get-positions-abilities abilities coords i)
    (if (null? abilities)
        coords
        (get-positions-abilities (cdr abilities)
                                 (cons (make-posn (- (posn-x abilities-posn) (* 50 i))
                                            (posn-y abilities-posn)) coords)
                                 (add1 i)))
 )

(define (place-active-abilities abilities scene)
	(place-images (reverse (map (λ (ability) (scale 0.75 (ability-struct-image ability))) abilities))
                      (get-positions-abilities abilities null 0) scene))

(module+ main
	(big-bang (get-initial-state)
	 [on-tick next-state (/ 1.0 fps)]
	 [to-draw draw-frame]
	 [on-key change]
	 [stop-when invalid-state?]
	 [close-on-stop #t]
	 [record? #f]))
