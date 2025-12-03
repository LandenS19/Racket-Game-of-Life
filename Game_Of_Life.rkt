;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Game_Of_Life) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;=====================================VARIABLES/CONSTANTS============================================
(define W-SIZE 1000)
(define NumOfSqrs 10)
(define SQ-SZ (/ W-SIZE NumOfSqrs))

(define liveCell (overlay (square SQ-SZ 'outline "black") (square SQ-SZ 'solid "white")))
(define BACKGROUND (square W-SIZE 'solid "black"))
(define-struct WS (grd isActive?))
(define-struct squares (position isLiving?))

;=======================================MAKING THE WORLD==============================================
 
(define (generate-grid x y)
  (cond
    [(> x NumOfSqrs) (generate-grid 1 (+ y 1))]
    [(> y NumOfSqrs) empty]
    [else (cons (make-squares (make-posn x y) false) (generate-grid (+ x 1) y))]))

(define INIT-WS (make-WS (generate-grid 1 1) false))

(define (draw-grid ws grid image)
  (cond 
    [(empty? grid) image]
    [(not (squares-isLiving? (first grid))) (draw-grid ws (rest grid) image)]
    [else (place-image
           liveCell
           (- ( * SQ-SZ (posn-x (squares-position (first grid)))) (/ SQ-SZ 2))
           (- ( * SQ-SZ (posn-y (squares-position (first grid)))) (/ SQ-SZ 2))
           (draw-grid ws (rest grid) image))])) 

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

;list of neighbors {Give the position -1 EX: (neighbors (- (posn-x ws) 1) (- (posn-y ws) 1))}
(define (neighbors x y x1 y1)
  (cond
    [(< x 1) (neighbors NumOfSqrs y x1 y1)]
    [(< y 1) (neighbors x NumOfSqrs x1 y1)]
    [(> x NumOfSqrs) (neighbors 1 y x1 y1)]
    [(> y NumOfSqrs) (neighbors x 1 x1 y1)]
    [(= x (if
           (>= (+ x1 2) 11)
           (- (+ x1 2) 10) (+ x1 2)))
        (neighbors (- x1 1) (+ y 1) x1 y1)]
    [(= y (if
           (>= (+ y1 2) 11)
           (- (+ y1 2) 10) (+ y1 2))) empty]
    [(and (= x x1) (= y y1)) (neighbors (+ x1 1) y x1 y1)]
    [else (cons (make-posn x y) (neighbors (+ x 1) y x1 y1))]))

;Check number of live neighbors
;gets the position of the cell and counts the number of live squares next to it there are
; LON-> List of Neigbors
; Grid = the list of all the squares
; crd = the cordinates of the square to check neighbors
; n = the number of live neighbors (should always be set to 0 when called)
(define (check-neighbors ws lon grid crd n)
  (cond
    [(empty? lon) n]
    [(empty? grid) (check-neighbors ws (rest lon) (WS-grd ws) crd n)]
    [(and (posn=? (squares-position (first grid)) (first lon)) (squares-isLiving? (first grid)))
     (check-neighbors ws (rest lon) (WS-grd ws) crd (+ 1 n))]
    [else (check-neighbors ws lon (rest grid) crd n)]
    ))

;Update Board
;handles generating a new board every update
(define (update-board ws grid new-grid)
  (cond
    [(empty? grid) (make-WS new-grid (WS-isActive? ws))]
    [(squares-isLiving? (first grid))
     (cond
       [(underpopulation (check-neighbors
                          ws
                          (neighbors
                           (- (posn-x (squares-position (first grid))) 1) (- (posn-y (squares-position (first grid))) 1)
                           (posn-x (squares-position (first grid))) (posn-y (squares-position (first grid))))
                          (WS-grd ws)
                          (squares-position (first grid))
                          0))
        (update-board ws (rest grid) (append (replace-square ws grid (squares-position (first grid)))
                                             (cons (make-squares (squares-position (first grid)) false) empty)))]
       [(overpopulation (check-neighbors
                         ws
                         (neighbors
                           (- (posn-x (squares-position (first grid))) 1) (- (posn-y (squares-position (first grid))) 1)
                           (posn-x (squares-position (first grid))) (posn-y (squares-position (first grid))))
                         (WS-grd ws)
                         (squares-position (first grid))
                         0))
        (update-board ws (rest grid) (append (replace-square ws grid (squares-position (first grid)))
                                             (cons (make-squares (squares-position (first grid)) false) empty)))]
       [else ws])]
    [(reproduction (check-neighbors
                    ws
                    (neighbors
                           (- (posn-x (squares-position (first grid))) 1) (- (posn-y (squares-position (first grid))) 1)
                           (posn-x (squares-position (first grid))) (posn-y (squares-position (first grid))))
                    (WS-grd ws)
                    (squares-position (first grid))
                    0))
     (update-board ws (rest grid) (append (replace-square ws grid (squares-position (first grid)))
                                          (cons (make-squares (squares-position (first grid)) true) empty)))]
    [else (update-board ws (rest grid) (append (list (first grid)) new-grid))]))
   


;if active
; check-live neighbors for the first of the ws-grd using the neighbors function for lon
; - cond for the rules based on n gathered from check-neighbors
; - - replace the square based on rules
; - - append to the rest of the grid
(define (tock ws)
  (cond
    [(WS-isActive? ws) (update-board ws (WS-grd ws) (list empty))]
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
        true)
       (WS-grd ws)) (WS-isActive? ws))]
    [(posn=? (squares-position (first grid)) crd)
     (make-WS (append (replace-square ws grid crd)
                      (cons (make-squares crd (if (squares-isLiving? (first grid)) false true)) empty)) (WS-isActive? ws))]
    [else (change-square ws (rest grid) crd)]))
   
;=========================================================================================================

(define (main ws)
  (big-bang ws
    [on-draw render]
    [on-key key-handler]
    [on-mouse mouse-handler]
    [on-tick tock .1]
    ;[stop-when game-over]
    ))

(main INIT-WS)
