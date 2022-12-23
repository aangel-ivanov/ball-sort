;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ball-sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "lib-tubes.rkt")

;; A Game is (make-game Nat Nat (listof (listof Sym)))
(define-struct game (tubesize maxcolours tubes))

;;; Constants

(define emptygame
  (make-game 0 5
             (list empty empty empty empty empty)))

(define emptygame2
  (make-game 10 3 empty))

(define emptygame3
  (make-game 10 3 (list empty empty)))

(define smallgame1
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define smallgame2
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define smallinvalidgame1
  (make-game 2 1
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))


(define smallinvalidgame2
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'blue)
                   (list))))

(define smallinvalidgame3
  (make-game 2 2
             (list (list 'blue 'red 'blue)
                   (list 'red)
                   (list))))


(define smallgamefinal
  (make-game 2 2
             (list (list)
                   (list 'blue 'blue)
                   (list 'red 'red))))


(define mediumgame
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   (list))))

(define mediumgamestuck
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   )))

(define largergame
  (make-game 3 3
             (list (list 'blue 'red 'red)
                   (list 'yellow 'blue 'yellow)
                   (list 'red 'yellow 'blue)
                   (list))))

(define biggame
  (make-game 5 3
             (list (list 'blue 'blue 'red 'red 'yellow)
                   (list 'red 'red 'yellow 'blue 'red)
                   (list 'yellow 'blue 'blue 'yellow 'yellow)
                   (list)
                   (list))))

(define biggame2
  (make-game 5 3
             (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                   (list 'red 'red 'yellow 'blue 'red)
                   (list)
                   (list 'blue 'blue 'red 'red 'yellow)
                   (list))))

(define biggamesolve
  (make-game 5 3
             (list (list 'blue 'blue 'blue 'blue 'blue)
                   (list 'red 'red 'red 'red 'red)
                   (list 'yellow 'yellow 'yellow 'yellow 'yellow)
                   (list)
                   (list))))

(define hugegame
  (make-game 4 9
             (list (list 'purple 'pink 'yellow 'blue)
                   (list 'blue 'green 'purple 'white)
                   (list 'orange 'yellow 'black 'blue)
                   (list 'white 'orange 'orange 'pink)
                   (list 'pink 'red 'red 'black)
                   (list 'yellow 'green 'orange 'blue)
                   (list 'white 'purple 'red 'yellow)
                   (list 'green 'red 'green 'black)
                   (list 'purple 'black 'white 'pink)
                   (list)
                   (list))
             ))
;;;;;

;; (valid-game? gm) produces true if gm is a valid game--that is, if each list
;;    has at most tubesize symbols and all colours satisfy the counting conditions.

;; Examples:
(check-expect (valid-game? emptygame) true)
(check-expect (valid-game? largergame) true)
(check-expect (valid-game? smallinvalidgame1) false)
(check-expect (valid-game? smallinvalidgame2) false)

;; valid-game?: Game -> Bool
(define (valid-game? gm)
  (local [;; (check-colour? size num los) produces true if every symbol in los appears
          ;;    size times and there are no more than num different symbols in los and
          ;;    false otherwise.
          ;; check-colour: Nat Nat (listof Sym) -> Bool
          (define (check-colour? size num los)
            (cond [(empty? los) true]
                  ; return false if num is 0 and los is non-empty
                  [(zero? num) false]
                  [else (local [(define without-curr
                                  (filter (lambda (s)
                                            (not (symbol=? (first los) s))) los))]
                          ; return false if there are not size occurences of curr
                          (and (= (- (length los) (length without-curr)) size)
                               (check-colour? size (sub1 num) without-curr)))]))]
    (and
     ;; ensure no tubes have length more than tubesize
     (empty? (filter (lambda (curr-len) (> curr-len (game-tubesize gm)))
                     (map length (game-tubes gm))))

     ;; ensure all colours are valid
     (local [(define merged-tubes (foldr (lambda (t1 t2) (append t1 t2)) '() (game-tubes gm)))]
       (or (empty? merged-tubes)
           (and (<= (length merged-tubes) (* (game-tubesize gm) (game-maxcolours gm)))
                (check-colour? (game-tubesize gm) (game-maxcolours gm) merged-tubes)))))))



;; (finished-game? gm) produces true if the game gm is finished--that is, when
;;    removing completed tubes yields either empty or just empty tubes.

;; Examples:
(check-expect (finished-game? emptygame) true)
(check-expect (finished-game? emptygame2) true)
(check-expect (finished-game? hugegame) false)

;; finished-game?: Game -> Bool
(define (finished-game? gm)
  (local [;; (remove-completed gm) produces the game that results from removing any
          ;;    completed tubes (tubes that are full of the same coloured ball) from gm.
          ;; remove-completed: Game -> Game
          (define (remove-completed gm)
            (local [; game parameters
                    (define tubesize (game-tubesize gm))
                    (define maxcolours (game-maxcolours gm))
                    (define tubes (game-tubes gm))

                    ;; (same-colours? los) returns true if los contains only one colour
                    ;;    and false otherwise.
                    ;; same-colours?: (listof Sym) -> Bool
                    (define (same-colours? los)
                      (cond [(empty? los) true]
                            [(empty? (rest los)) true]
                            [(not (symbol=? (first los) (second los))) false]
                            [else (same-colours? (rest los))]))

                    ;; (tube-full? tube) returns true if tube is full.
                    ;; tube-full?: (listof Sym) -> Bool
                    (define (tube-full? tube)
                      (cond [(= tubesize (length tube)) true]
                            [else false]))

                    ;; (tubes-removed tubes) returns the number of completed tubes in tubes
                    ;; (listof (listof Sym)) -> Nat
                    (define (tubes-removed tubes)
                      (cond [(empty? tubes) 0]
                            [(and (tube-full? (first tubes))
                                  (same-colours? (first tubes)))
                             (+ 1 (tubes-removed (rest tubes)))]
                            [else (tubes-removed (rest tubes))]))

                    ;; (update-tubes tubes) produces tubes without any full tubes
                    ;; (listof (listof Sym)) -> (listof (listof Sym))
                    (define (update-tubes tubes)
                      (cond [(empty? tubes) empty]
                            [(and (tube-full? (first tubes))
                                  (same-colours? (first tubes)))
                             (update-tubes (rest tubes))]
                            [else (cons (first tubes) (update-tubes (rest tubes)))]))]

              ; make a new game without any completed tubes
              (make-game tubesize (- maxcolours (tubes-removed tubes)) (update-tubes tubes))))]

    ; return true if all tubes are either empty or completed
    (empty? (filter (lambda (tube) (not (empty? tube))) (game-tubes (remove-completed gm))))))


;; (all-equiv? log1 log2) produces true if log1 contains exactly the same
;;    games as log2 up to equivalency of games and false otherwise.

;; Examples:
(check-expect (all-equiv? (list emptygame) (list emptygame2)) false)
(check-expect (all-equiv? (list emptygame emptygame2 smallgame1 biggame)
                          (list biggame smallgame1 emptygame emptygame2))
              true)

;; all-equiv?: (listof Game) (listof Game) -> Bool
(define (all-equiv? log1 log2)
  (local [;; (equiv-game? gm1 gm2) produces true if gm1 and gm2 are equivalent
          ;;    games and false otherwise.
          ;; equiv-game?: Game Game -> Bool
          (define (equiv-game? gm1 gm2)
            (local [;; (remove-tube tube tubes) removes tube from tubes
                    ;; remove-tube: (listof Sym) (listof (listof Sym)) -> (listof (listof Sym))
                    (define (remove-tube tube tubes)
                      (cond [(equal? tube (first tubes)) (rest tubes)]
                            [else (cons (first tubes) (remove-tube tube (rest tubes)))]))

                    ;; (update-tubes t1 t2) returns true if the balls are in identical order
                    ;;    in t1 and t2; and false otherwise.
                    ;; update-tubes (listof Sym) (listof Sym) -> Bool
                    (define (update-tubes t1 t2)
                      (cond [(and (empty? t1)
                                  (empty? t2)) true]
                            ; case for both tubes being non-empty
                            [(not (member? (first t1) t2)) false]
                            [else (update-tubes (remove-tube (first t1) t1)
                                                (remove-tube (first t1) t2))]))]

              ; main logic for equiv-game?
              (cond [(not (= (game-maxcolours gm1)
                             (game-maxcolours gm2))) false]
                    [(not (= (game-tubesize gm1)
                             (game-tubesize gm2))) false]
                    [(not (= (length (game-tubes gm1))
                             (length (game-tubes gm2)))) false]
                    ; check for same number of balls in identical order
                    [else (update-tubes (game-tubes gm1) (game-tubes gm2))])))]

    ; main logic for all-equiv?
    (cond [(and (empty? log1) (empty? log2)) true]
          [(or (empty? log1) (empty? log2)) false]
          [else (local [(define f (first log1))
                        (define nof1 (filter (lambda (gm) (not (equiv-game? gm f))) log1))
                        (define nof2 (filter (lambda (gm) (not (equiv-game? gm f))) log2))]

                  (and (= (length nof1) (length nof2))
                       (all-equiv? nof1 nof2)))])))

;; for testing next-games
(define (test-next-games gm expected) (all-equiv? (next-games gm) expected))

;; (next-games gm) produces a list of games that can happen from moving one ball
;;    between tubes in the game gm.

;; Examples:
(check-expect (next-games (make-game 0 0 '())) '())
(check-expect (next-games (make-game 1 2 '((red) (blue)))) '())
(check-expect (test-next-games (make-game 2 2 '((red) (blue)))
                               (list (make-game 2 2 '((blue red) ()))
                                     (make-game 2 2 '(() (red blue)))))
              true)
(check-expect (test-next-games (make-game 2 2 '((red blue) ()))
                               (list (make-game 2 2 '((blue) (red)))))
              true)
(check-expect (test-next-games (make-game 2 2 '((red blue) ()))
                               (list (make-game 2 2 '((red blue) ()))))
              false)

;; next-games: Game -> (listof Game)
(define (next-games gm)
  (local [;; (place-ball ball curr-tube add-tube lot) produces lot with the
          ;;    exception that the tube at position add-tube has ball placed on
          ;;    top of it, keeping track of the current tube with curr-tube.
          ;; place-ball: Sym Nat Nat (listof (listof Sym)) -> (listof (listof Sym))
          (define (place-ball ball curr-tube add-tube lot)
            (cond [(= curr-tube add-tube) (cons (cons ball (first lot))
                                                (rest lot))]
                  [else (cons (first lot)
                              (place-ball ball (add1 curr-tube) add-tube (rest lot)))]))

          ;; (place-everywhere ball curr-tube remove-tube gm) produces the list
          ;;    of games that can be reached when placing ball in every tube of
          ;;    gm other than remove-tube, keeping track of the current tube
          ;;    with curr-tube.
          ;; place-everywhere: Sym Nat Nat Game -> (listof Game)
          (define (place-everywhere ball curr-tube remove-tube gm)
            (cond [(= curr-tube (length (game-tubes gm))) '()]

                  ;; don't place ball here: keep going
                  [(or (= curr-tube remove-tube)
                       (= (length (list-ref (game-tubes gm) curr-tube))
                          (game-tubesize gm)))
                   (place-everywhere ball (add1 curr-tube) remove-tube gm)]

                  [else (cons (make-game (game-tubesize gm)
                                         (game-maxcolours gm)
                                         (place-ball ball 0 curr-tube (game-tubes gm)))
                         (place-everywhere ball (add1 curr-tube) remove-tube gm))]))

          ;; (remove-top curr-tube remove-tube lot) produces the list of games
          ;;    where the top of remove-tube is removed from lot and everything
          ;;    else is unchanged, keeping track of the current tube with
          ;;    curr-tube.
          ;; remove-top: Nat Nat (listof (listof Sym)) -> (listof (listof Sym))
          (define (remove-top curr-tube remove-tube lot)
            (cond [(= curr-tube remove-tube) (cons (rest (first lot)) (rest lot))]
                  [else (cons (first lot) (remove-top (add1 curr-tube) remove-tube (rest lot)))]))

          ;; (move-top curr-tube gm) moves the top ball from curr-tube to all
          ;;    possible positions.
          ;; move-top: Nat Game -> (listof Game)
          (define (move-top curr-tube gm)
            (cond ;; no more tubes to try
                  [(= curr-tube (length (game-tubes gm))) '()]

                  ;; if the current tube is empty, move on to the next tube
                  [(empty? (list-ref (game-tubes gm) curr-tube)) (move-top (add1 curr-tube) gm)]

                  ;; remove the top item from the current tube and try to place
                  ;;    it everywhere and keep looking
                  [else (local [(define tube (list-ref (game-tubes gm) curr-tube))
                                (define ball (first tube))]

                          (append (place-everywhere ball 0 curr-tube
                                                    (make-game
                                                     (game-tubesize gm)
                                                     (game-maxcolours gm)
                                                     (remove-top 0 curr-tube (game-tubes gm))))
                                  (move-top (add1 curr-tube) gm)))]))]
    (move-top 0 gm)))



;; (solve gm draw-option) determines if the game gm is solveable,
;; and will also draw each possible move depending on the draw-option

;; Examples:
(check-expect (solve smallgame1 'off) true)
(check-expect (solve mediumgamestuck 'off) false)

;; solve: Game (anyof 'off 'norm 'slow 'fast) -> Bool
(define (solve gm draw-option)
  (local
    [(define setup (puzzle-setup gm draw-option))                
     (define (solve-helper to-visit visited)
       (cond
         [(empty? to-visit) false]
         [else (local
                 [(define draw (draw-board (first to-visit) draw-option))]
                 (cond
                   [(finished-game? (first to-visit)) true]
                   [(member? (first to-visit) visited)
                    (solve-helper (rest to-visit) visited)]
                   [else
                    (local [(define nbrs (next-games (first to-visit)))
                            (define new-nbrs (filter (lambda (x) (not (member? x visited))) nbrs))
                            (define new-to-visit (append new-nbrs (rest to-visit)))
                            (define new-visited (cons (first to-visit) visited))]
                      (solve-helper new-to-visit new-visited))]))]))]

    (cond [(not (valid-game? gm)) (error "Not a valid game!")]
          [else (solve-helper (list gm) empty)])))

















