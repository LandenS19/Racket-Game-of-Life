;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Game_Of_Life) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define W-SIZE 1000)
(define NumOfSqrs 100)
(define SQ-SZ (/ W-SIZE NumOfSqrs))

(define liveCell (overlay (square SQ-SZ 'outline "black") (square SQ-SZ 'solid "red")))
(define BACKGROUND (square W-SIZE 'solid "black"))
(define-struct WS (grd))
(define-struct squares (position isLiving?))

(define (generate-grid x y)
  (cond
    [(> x NumOfSqrs) (generate-grid 1 (+ y 1))]
    [(> y NumOfSqrs) empty]
    [else (cons (make-squares (make-posn x y) false) (generate-grid (+ x 1) y))]))

(define INIT-WS (make-WS (generate-grid 1 1)))

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
    [(> x (+ x1 2)) (neighbors x1 (+ y 1) x1 y1)]
    [(> y (+ y1 2)) empty]
    [else (cons (make-posn x y) (neighbors (+ x 1) y x1 y1))]))

;Check number of live neighbors
;gets the position of the cell and counts the number of live squares next to it there are
; LON-> List of Neigbors
(define (check-neighbors lon )
  (cond
    []
    []
    ))


(define (draw-grid ws grid image)
  (cond
    [(empty? grid) image]
    [(not (squares-isLiving? (first grid))) (draw-grid ws (rest grid) image)]
    [else (place-image liveCell (- ( * SQ-SZ (posn-x (squares-position (first grid)))) (/ SQ-SZ 2)) (- ( * SQ-SZ (posn-y (squares-position (first grid)))) (/ SQ-SZ 2)) (draw-grid ws (rest grid) image))])) 

(define (render ws)
  (draw-grid ws (WS-grd ws) BACKGROUND))

(define (key-handler ws key)
  (cond
    [(key=? key "up")
     (change-square ws (WS-grd ws) (make-posn (+ 1 (random NumOfSqrs)) (+ 1 (random NumOfSqrs))))]
    [(key=? key "r")
     INIT-WS]
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

(define (tock ws)
  ws)

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
       (WS-grd ws)))]
    [(posn=? (squares-position (first grid)) crd)
     (make-WS (append (replace-square ws grid crd)
                      (cons (make-squares crd (if (squares-isLiving? (first grid)) false true)) empty)))]
    [else (change-square ws (rest grid) crd)]))
   


(define (main ws)
  (big-bang ws
    [on-draw render]
    [on-key key-handler]
    [on-mouse mouse-handler]
    [on-tick tock 1]
    ;[stop-when game-over]
    ))

(main INIT-WS)