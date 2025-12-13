;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Game_Of_Life) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;=====================================VARIABLES/CONSTANTS============================================
(define W-SIZE 1000)
(define NumOfSqrs 20)
(define SQ-SZ (/ W-SIZE NumOfSqrs))

(define liveCell (overlay (square SQ-SZ 'outline "black") (square SQ-SZ 'solid "white")))
(define BACKGROUND (square W-SIZE 'solid "black"))
(define-struct WS (grd isActive? place-glider?))
(define-struct squares (position isLiving? neighbors))

;=======================================MAKING THE WORLD==============================================
;list of neighbors {Give the position -1 EX: (neighbors (- (posn-x ws) 1) (- (posn-y ws) 1))}
(define (set-neighbors x y x1 y1)
  (cond
    [(< x 1) (set-neighbors NumOfSqrs y x1 y1)]
    [(< y 1) (set-neighbors x NumOfSqrs x1 y1)]
    [(> x NumOfSqrs) (set-neighbors 1 y x1 y1)]
    [(> y NumOfSqrs) (set-neighbors x 1 x1 y1)]
    [(= x (if (>= (+ x1 2) (+ NumOfSqrs 1)) (- (+ x1 2) NumOfSqrs) (+ x1 2)))
     (set-neighbors (- x1 1) (+ y 1) x1 y1)]
    [(= y (if (>= (+ y1 2) (+ NumOfSqrs 1)) (- (+ y1 2) NumOfSqrs) (+ y1 2))) empty]
    [(and (= x x1) (= y y1)) (set-neighbors (+ x1 1) y x1 y1)]
    [else (cons (make-posn x y) (set-neighbors (+ x 1) y x1 y1))]))


;Creates the list of squares for the grid
(define (generate-grid x y) 
  (cond
    [(> x NumOfSqrs) (generate-grid 1 (+ y 1))]
    [(> y NumOfSqrs) empty]
    [else (cons (make-squares (make-posn x y) false (set-neighbors (- x 1) (- y 1) x y)) (generate-grid (+ x 1) y))]))

(define STARTING-GRID (generate-grid 1 1))   ;Initial grid of non-living squares

(define INIT-WS (make-WS STARTING-GRID false false))  ;Initial World State

;Draws a square if the square is lving
(define (draw-grid ws grid image)
  (local [(define (place-grid ws grid new-grid)   ;Uses local to make the whole image before displaying it.
            (cond
              [(empty? grid) new-grid]
              [(not (squares-isLiving? (first grid))) (place-grid ws (rest grid) new-grid)]
              [else (place-grid
                     ws
                     (rest grid)
                     (place-image
                     liveCell
                     (- ( * SQ-SZ (posn-x (squares-position (first grid)))) (/ SQ-SZ 2))  ;Converts x coordinate to actual x
                     (- ( * SQ-SZ (posn-y (squares-position (first grid)))) (/ SQ-SZ 2))  ;Converts y coordinate to acutal y
                     new-grid))]))]
    (place-grid ws grid image)))

;Called to make the image
(define (render ws)
  (draw-grid ws (WS-grd ws) BACKGROUND))

;======================================INPUT CONTROLS===================================================
;Handles all Key Inputs
(define (key-handler ws key)
  (cond
    [(key=? key "up")  ;If the up arrow key is pressed, changes the life state of a random square.
     (change-square
      ws
      (WS-grd ws)
      (make-posn (+ 1 (random NumOfSqrs)) (+ 1 (random NumOfSqrs))))]
    [(key=? key "r")  ;If the r key is pressed it resets the grid
     INIT-WS]
    [(key=? key " ")  ;If the space key is pressed it Activates the simulation
     (make-WS (WS-grd ws) (if (WS-isActive? ws) false true) (WS-place-glider? ws))]
    [(key=? key "1")  ;If the 1 number key is pressed, toggles glider placement
     (make-WS (WS-grd ws) (WS-isActive? ws) (if (WS-place-glider? ws) false true))]
    [else ws]))

;Converts real position to coordinate position
(define (make-simple n start end p)
  (cond
    [(and (>= n start) (<= n end)) p]
    [else (make-simple n end (+ end SQ-SZ) (+ p 1))]))

;Handles all mouse inputs
(define (mouse-handler ws x y mouse )
  (cond
    [(string=? mouse "button-down")  ;If the mouse clicks
     (cond
       ;If place glider is active, places a glider
       [(WS-place-glider? ws) (place-glider
                               ws
                               (make-glider-list
                                ws
                               (make-simple x 0 SQ-SZ 1)
                               (make-simple y 0 SQ-SZ 1))
                               (make-dead-glider-list
                                ws
                                (make-simple x 0 SQ-SZ 1)
                                (make-simple y 0 SQ-SZ 1)))]
       ;Else it just toggles the squares living state
       [else (change-square ws (WS-grd ws) (make-posn (make-simple x 0 SQ-SZ 1) (make-simple y 0 SQ-SZ 1)))])]
    [else ws]))

;========================================SIMULATION HARDWARE=============================================
;GAME OF LIFE RULES
; Neighbors < 2 and live = dead 
(define (underpopulation n)
  (< n 2))

; Neighbors = 2 or 3 and live = live
(define (maintain n)
  (or (= n 2) (= n 3)))

; Neighbors > 3 and live = dead
(define (overpopulation n)
  (> n 3))

; Neighbors = 3 and dead = live
(define (reproduction n)
  (= n 3))


;Check number of live neighbors
;counts how many neighbors are living
; Grid = the list of all the squares
; n = the number of live neighbors (should always be set to 0 when called)
(define (check-neighbors grid n)
  (cond
    [(empty? grid) n]
    [(squares-isLiving? (first grid))
     (check-neighbors (rest grid) (+ 1 n))]
    [else (check-neighbors (rest grid) n)]
    ))

;Update Board
;handles generating a new board every update 
(define (update-board ws grid)
  (local [(define (make-new-grid ws grid new-grid)
            (cond
              [(empty? grid) (make-WS new-grid (WS-isActive? ws) (WS-place-glider? ws))]  
              [(squares-isLiving? (first grid))  ;If the square is alive                   
               (cond
                 ;And the number of live neigbors is less than 2 then the square is dead
                 [(underpopulation (check-neighbors    
                                    (filter (lambda (p) (member? (squares-position p) (squares-neighbors (first grid)))) (WS-grd ws))
                                    0))
                  (make-new-grid ws
                                 (rest grid)
                                 (cons (make-squares (squares-position (first grid)) false (squares-neighbors (first grid))) new-grid))]
                 ;And the number of live neighbors is more than 3 then the square is dead
                 [(overpopulation (check-neighbors
                                   (filter (lambda (p) (member? (squares-position p) (squares-neighbors (first grid)))) (WS-grd ws))
                                   0))
                  (make-new-grid ws
                                 (rest grid)
                                 (cons (make-squares (squares-position (first grid)) false (squares-neighbors (first grid))) new-grid))]
                 ;Else the number of neighbors will be 2 or three so the square stays alive
                 [else (make-new-grid ws (rest grid) (cons (first grid) new-grid))])]
              ;Since the square is not alive
              ;If the number of neighbors = 3 then the square comes alive
              [(reproduction (check-neighbors
                              (filter (lambda (p) (member? (squares-position p) (squares-neighbors (first grid)))) (WS-grd ws))
                              0))
               (make-new-grid ws
                              (rest grid)
                              (cons (make-squares (squares-position (first grid)) true (squares-neighbors (first grid))) new-grid))]
              ;Else if dead and neigbors != 3 then the square is still dead
              [else (make-new-grid ws (rest grid) (cons (first grid) new-grid))]))]
    (make-new-grid ws grid (list ))))
   


;If the simulation is active then it updates the board every tick else it just returns the world state
(define (tock ws)
  (cond
    [(WS-isActive? ws) (update-board ws (WS-grd ws))]
    [else ws]))


;======================================TUNRING SQUARES ON OFF=============================================
;Compares 2 posn and sees if they are equal
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2)) (= (posn-y p1) (posn-y p2))))

;Removes a square from the grid
(define (replace-square ws grid crd)
  (remove (first grid) (WS-grd ws)))

;Replaces a single square from the WS and replaces it with the opposite living state
(define (change-square ws grid crd)
  (cond
    [(empty? grid)
     (make-WS
      (cons
       (make-squares
        [make-posn (posn-x crd) (posn-y crd)]
        true
        (set-neighbors (- (posn-x crd) 1) (- (posn-y crd) 1) (posn-x crd) (posn-y crd)))
       (WS-grd ws))
      (WS-isActive? ws)
      (WS-place-glider? ws))]
    [(posn=? (squares-position (first grid)) crd)
     (make-WS (append (replace-square ws grid crd)
                      (cons (make-squares crd (if (squares-isLiving? (first grid)) false true) (squares-neighbors (first grid))) empty))
              (WS-isActive? ws)
              (WS-place-glider? ws))]
    [else (change-square ws (rest grid) crd)]))

;A list of living squares for a glider
(define (make-glider-list ws x y)
  (list (make-squares (make-posn x (if (> 0 (- y 2)) (+ (- y 2) NumOfSqrs) (- y 2)))
                             true
                             (set-neighbors (- x 1)
                                            (- (if (> 0 (- y 2)) (+ (- y 2) NumOfSqrs) (- y 2)) 1)
                                            x
                                            (if (> 0 (- y 2)) (+ (- y 2) NumOfSqrs) (- y 2))))
        (make-squares (make-posn (if (> 0 (- x 1)) (+ (- x 1) NumOfSqrs) (- x 1))
                                 (if (> 0 (- y 1)) (+ (- y 1) NumOfSqrs) (- y 1)))
                             true
                             (set-neighbors (- (if (> 0 (- x 1)) (+ (- x 1) NumOfSqrs) (- x 1)) 1)
                                            (- (if (> 0 (- y 1)) (+ (- y 1) NumOfSqrs) (- y 1)) 1)
                                            (if (> 0 (- x 1)) (+ (- x 1) NumOfSqrs) (- x 1))
                                            (if (> 0 (- y 1)) (+ (- y 1) NumOfSqrs) (- y 1))))
        (make-squares (make-posn (if (> 0 (- x 1)) (+ (- x 1) NumOfSqrs) (- x 1)) y)
                     true
                     (set-neighbors (- (if (> 0 (- x 1)) (+ (- x 1) NumOfSqrs) (- x 1)) 1)
                                    (- y 1)
                                    (if (> 0 (- x 1)) (+ (- x 1) NumOfSqrs) (- x 1))
                                    y))
        (make-squares (make-posn x y)
                     true
                     (set-neighbors (- x 1)
                                    (- y 1)
                                    x
                                    y))
        (make-squares (make-posn (if (< NumOfSqrs (+ x 1)) (- (+ x 1) NumOfSqrs) (+ x 1)) y)
                     true
                     (set-neighbors (- (if (< NumOfSqrs (+ x 1)) (- (+ x 1) NumOfSqrs) (+ x 1)) 1)
                                    (- y 1)
                                    (if (< NumOfSqrs (+ x 1)) (- (+ x 1) NumOfSqrs) (+ x 1))
                                    y))))
;A list of dead squares to remove from the list to place a glider
(define (make-dead-glider-list ws x y)
  (list (make-squares (make-posn x (if (> 0 (- y 2)) (+ (- y 2) NumOfSqrs) (- y 2)))
                             false
                             (set-neighbors (- x 1)
                                            (- (if (> 0 (- y 2)) (+ (- y 2) NumOfSqrs) (- y 2)) 1)
                                            x
                                            (if (> 0 (- y 2)) (+ (- y 2) NumOfSqrs) (- y 2))))
        (make-squares (make-posn (if (> 0 (- x 1)) (+ (- x 1) NumOfSqrs) (- x 1))
                                 (if (> 0 (- y 1)) (+ (- y 1) NumOfSqrs) (- y 1)))
                             false
                             (set-neighbors (- (if (> 0 (- x 1)) (+ (- x 1) NumOfSqrs) (- x 1)) 1)
                                            (- (if (> 0 (- y 1)) (+ (- y 1) NumOfSqrs) (- y 1)) 1)
                                            (if (> 0 (- x 1)) (+ (- x 1) NumOfSqrs) (- x 1))
                                            (if (> 0 (- y 1)) (+ (- y 1) NumOfSqrs) (- y 1))))
        (make-squares (make-posn (if (> 0 (- x 1)) (+ (- x 1) NumOfSqrs) (- x 1)) y)
                     false
                     (set-neighbors (- (if (> 0 (- x 1)) (+ (- x 1) NumOfSqrs) (- x 1)) 1)
                                    (- y 1)
                                    (if (> 0 (- x 1)) (+ (- x 1) NumOfSqrs) (- x 1))
                                    y))
        (make-squares (make-posn x y)
                     false
                     (set-neighbors (- x 1)
                                    (- y 1)
                                    x
                                    y))
        (make-squares (make-posn (if (< NumOfSqrs (+ x 1)) (- (+ x 1) NumOfSqrs) (+ x 1)) y)
                     false
                     (set-neighbors (- (if (< NumOfSqrs (+ x 1)) (- (+ x 1) NumOfSqrs) (+ x 1)) 1)
                                    (- y 1)
                                    (if (< NumOfSqrs (+ x 1)) (- (+ x 1) NumOfSqrs) (+ x 1))
                                    y))))

;Combines the living glider list with the world grid without those squares
(define (place-glider ws glider-crds dead-glider-crds)
  (make-WS
   (append glider-crds
          (filter (lambda (p) (not (or (member? p glider-crds) (member? p dead-glider-crds)))) (WS-grd ws)))
   (WS-isActive? ws)
   (WS-place-glider? ws)))

;=========================================================================================================
;Main Function
(define (main ws)
  (big-bang ws
    [on-draw render]
    [on-key key-handler]
    [on-mouse mouse-handler]
    [on-tick tock .2]
    ))

(main INIT-WS)
