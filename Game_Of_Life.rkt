;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Game_Of_Life) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;=====================================VARIABLES/CONSTANTS============================================
(define W-SIZE 1000)
(define NumOfSqrs 20)
(define SQ-SZ (/ W-SIZE NumOfSqrs))

(define liveCell (overlay (square SQ-SZ 'outline "black") (square SQ-SZ 'solid "white")))
(define BACKGROUND (square W-SIZE 'solid "black"))
(define-struct WS (grd isActive?))
(define-struct squares (position isLiving? neighbors)) 

;=======================================MAKING THE WORLD==============================================
;list of neighbors {Give the position -1 EX: (neighbors (- (posn-x ws) 1) (- (posn-y ws) 1))}
(define (set-neighbors x y x1 y1)
  (cond
    [(< x 1) (set-neighbors NumOfSqrs y x1 y1)]
    [(< y 1) (set-neighbors x NumOfSqrs x1 y1)]
    [(> x NumOfSqrs) (set-neighbors 1 y x1 y1)]
    [(> y NumOfSqrs) (set-neighbors x 1 x1 y1)]
    [(= x (if
           (>= (+ x1 2) (+ NumOfSqrs 1))
           (- (+ x1 2) NumOfSqrs) (+ x1 2)))
        (set-neighbors (- x1 1) (+ y 1) x1 y1)]
    [(= y (if
           (>= (+ y1 2) (+ NumOfSqrs 1))
           (- (+ y1 2) NumOfSqrs) (+ y1 2))) empty]
    [(and (= x x1) (= y y1)) (set-neighbors (+ x1 1) y x1 y1)]
    [else (cons (make-posn x y) (set-neighbors (+ x 1) y x1 y1))]))

(define (generate-grid x y)
  (cond
    [(> x NumOfSqrs) (generate-grid 1 (+ y 1))]
    [(> y NumOfSqrs) empty]
    [else (cons (make-squares (make-posn x y) false (set-neighbors (- x 1) (- y 1) x y)) (generate-grid (+ x 1) y))]))

(define STARTING-GRID (generate-grid 1 1))

(define INIT-WS (make-WS STARTING-GRID false))

(define (draw-grid ws grid image)
  (local [(define (place-grid ws grid new-grid)
            (cond
              [(empty? grid) new-grid]
              [(not (squares-isLiving? (first grid))) (place-grid ws (rest grid) new-grid)]
              [else (place-grid
                     ws
                     (rest grid)
                     (place-image
                     liveCell
                     (- ( * SQ-SZ (posn-x (squares-position (first grid)))) (/ SQ-SZ 2))
                     (- ( * SQ-SZ (posn-y (squares-position (first grid)))) (/ SQ-SZ 2))
                     new-grid))]))]
    (place-grid ws grid image)))

(define (render ws)
  (draw-grid ws (WS-grd ws) BACKGROUND))

;======================================INPUT CONTROLS===================================================
(define (key-handler ws key)
  (cond
    [(key=? key "up")
     (change-square
      ws
      (WS-grd ws)
      (make-posn (+ 1 (random NumOfSqrs)) (+ 1 (random NumOfSqrs))))]
    [(key=? key "r")
     INIT-WS]
    [(key=? key " ")
     (make-WS (WS-grd ws) (if (WS-isActive? ws) false true))]
    [else ws]))


(define (make-simple n start end p)
  (cond
    [(and (>= n start) (<= n end)) p]
    [else (make-simple n end (+ end SQ-SZ) (+ p 1))]))

(define (mouse-handler ws x y mouse )
  (cond
    [(string=? mouse "button-down")
     (change-square ws (WS-grd ws) (make-posn (make-simple x 0 SQ-SZ 1) (make-simple y 0 SQ-SZ 1)))]
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
              [(empty? grid) (make-WS new-grid (WS-isActive? ws))]
              [(squares-isLiving? (first grid))
               (cond
                 [(underpopulation (check-neighbors
                                    (filter (lambda (p) (member? (squares-position p) (squares-neighbors (first grid)))) (WS-grd ws))
                                    0))
                  (make-new-grid ws
                                 (rest grid)
                                 (cons (make-squares (squares-position (first grid)) false (squares-neighbors (first grid))) new-grid))]
                 [(overpopulation (check-neighbors
                                   (filter (lambda (p) (member? (squares-position p) (squares-neighbors (first grid)))) (WS-grd ws))
                                   0))
                  (make-new-grid ws
                                 (rest grid)
                                 (cons (make-squares (squares-position (first grid)) false (squares-neighbors (first grid))) new-grid))]
                 [else (make-new-grid ws (rest grid) (cons (first grid) new-grid))])]
              [(reproduction (check-neighbors
                              (filter (lambda (p) (member? (squares-position p) (squares-neighbors (first grid)))) (WS-grd ws))
                              0))
               (make-new-grid ws
                              (rest grid)
                              (cons (make-squares (squares-position (first grid)) true (squares-neighbors (first grid))) new-grid))]
              [else (make-new-grid ws (rest grid) (cons (first grid) new-grid))]))]
    (make-new-grid ws grid (list ))))
   


;if active
; check-live neighbors for the first of the ws-grd using the neighbors function for lon
; - cond for the rules based on n gathered from check-neighbors
; - - replace the square based on rules
; - - append to the rest of the grid
(define (tock ws)
  (cond
    [(WS-isActive? ws) (update-board ws (WS-grd ws))]
    [else ws]))


;======================================TUNRING SQUARES ON OFF=============================================
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2)) (= (posn-y p1) (posn-y p2))))

(define (replace-square ws grid crd)
  (remove (first grid) (WS-grd ws)))


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
      (WS-isActive? ws))]
    [(posn=? (squares-position (first grid)) crd)
     (make-WS (append (replace-square ws grid crd)
                      (cons (make-squares crd (if (squares-isLiving? (first grid)) false true) (squares-neighbors (first grid))) empty)) (WS-isActive? ws))]
    [else (change-square ws (rest grid) crd)]))
   
;=========================================================================================================

(define (main ws)
  (big-bang ws
    [on-draw render]
    [on-key key-handler]
    [on-mouse mouse-handler]
    [on-tick tock .2]
    ))

(main INIT-WS)
