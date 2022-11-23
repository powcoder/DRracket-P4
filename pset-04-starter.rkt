;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pset-04-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; DO NOT PUT ANY PERSONALLY IDENTIFYING BEYOND YOUR CS ID IN THIS 
;; FILE. YOUR COMPUTER SCIENCE IDs WILL BE SUFFICIENT TO IDENTIFY YOU 
;; AND, IF YOU HAVE ONE, YOUR PARTNER
(require 2htdp/universe)
(require 2htdp/image)
(require spd/tags)

(@assignment pset-04);Do not edit or remove this tag
(@csid ??? ???)      ;Replace ??? with your csid,
;;                   ;second ??? is replaced with partner id if you have one

(@problem 1)
;; Design a data definition for a book. Each book has a title, an author,
;; and a number of pages.
;;
;; Your design must follow all applicable recipes, and must include all
;; appropriate @tags.  Solutions which "just work" but do not follow the design
;; recipes will receive few if any marks.


(@problem 2)
;; Use your data definition from Problem 1 to complete this problem.
;;
;; Design a data definition for an arbitrary number of books.
;;
;; Your design must follow all applicable recipes, and must include all
;; appropriate @tags.  Solutions which "just work" but do not follow the design
;; recipes will receive few if any marks.


(@problem 3)
;;
;; One form of art is to write words and draw other designs
;; using sparklers at night:
;; https://www.thinkmakeshareblog.com/how-to-write-with-sparklers/
;;
;; In this problem set, you will design a program where users can use their
;; mouse to make their own similar artwork.
;;
;; When a user clicks down with the mouse, holds the button down, and moves
;; sparkles will begin appearing at each move (this is called a mouse drag).
;; Every time a user drags the mouse sparkles should appear wherever the mouse
;; is dragged. As long as the user keeps the mouse button clicked down, the
;; mouse can be dragged around the screen to make as many sparkles as one
;; wishes.
;;
;; When the user stops dragging the mouse then new sparkles are not added when
;; the mouse moves.  When the user clicks down with the mouse again, all old
;; sparkles are removed, and dragging causes new sparkles to begin appearing
;; once more.
;;
;; Each sparkle should be created at the x and y coordinate on the screen where
;; the mouse is.
;;
;; Images in BSL (and many other computer and programming environments) have an
;; alpha value that determines an image's transparency. An image with a 0 alpha
;; value is complete see-through (transparent). The maximum an alpha value can
;; go in BSL is 255, which is completely solid, or opaque.
;;
;; Each sparkle should begin completely opaque, and fade until it reaches 0 and
;; is completely transparent, to mimic what your eyes do when you watch
;; sparklers do when waved around in the dark.
;;
;; We have begun the design with constants, data definitions, a helper function
;; (at the end of the file), and a big-bang function, all of which you MUST use
;; and CANNOT change in your solution.  We have not provided wish list entries
;; for the helpers nor have we provided the helper definitions themselves.
;;
;; Your design must follow all applicable recipes, and must include all
;; appropriate @tags.  Solutions which "just work" but do not follow the design
;; recipes will receive few if any marks.

;; Sparkle Designer!
(@htdw ListOfSparkle)

;;==================
;; Constants: 

(define HEIGHT 400) ;pixels
(define WIDTH  800) ;pixels
(define MTS (empty-scene WIDTH HEIGHT "black"))

(define FADE-RATE 3)  ;alpha units per tick

(define COLOR "red")
(define NUM-POINTS 8) ;dimensions of each star
(define IN-RAD  2)    ;see radial-star function
(define OUT-RAD 8)    ;


;;==================
;; Data Definitions:

(@htdd Sparkle)
(define-struct sparkle (x y a))
;; Sparkle is (make-sparkle Natural Natural Integer)
;; interp. a sparkle with an x and y position in screen space coordinates
;;         and an alpha transparency value ranging from 0 to 255 inclusive,
;;         where is 0 is completely transparent and 255 is opaque
(define S0 (make-sparkle (/ WIDTH 2) (/ HEIGHT 2) 255))
(define S1 (make-sparkle 1 2 3))

(@dd-template-rules compound) ; 3 fields

(define (fn-for-sparkle s)
  (... (sparkle-x s)
       (sparkle-y s)
       (sparkle-a s)))


(@htdd ListOfSparkle)
;; ListOfSparkle is one of:
;; - empty
;; - (cons Sparkle ListOfSparkle)
;; interp. a list of sparkles
(define LOS0 empty)
(define LOS1 (cons S0 (cons S1 empty)))

(@dd-template-rules one-of           ; 2 cases
                    atomic-distinct  ; empty
                    compound         ; (cons Sparkle ListOfSparkle)
                    ref              ; (first los) is Sparkle
                    self-ref)        ; (rest los) is ListOfSparkle

(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (fn-for-sparkle (first los))
              (fn-for-los (rest los)))]))





;; =================
;; Functions:



(@htdf main)
(@signature ListOfSparkle -> ListOfSparkle)
;; start the world with (main empty)

(@template htdw-main)

(define (main los)
  (big-bang los                         ; ListOfSparkle
    (on-tick   next-sparkles)           ; ListOfSparkle -> ListOfSparkle
    (to-draw   render-sparkles)         ; ListOfSparkle -> Image
    (on-mouse  reset-or-add-sparkle)))  ; ListOfSparkle Integer Integer
;                                       ;   MouseEvent -> ListOfSparkle





(@htdf star-img)
(@signature Natural -> Image)
;; produce a radial star image with given alpha transparency value
(check-expect (star-img   0) (radial-star NUM-POINTS IN-RAD OUT-RAD 0 COLOR))
(check-expect (star-img  10) (radial-star NUM-POINTS IN-RAD OUT-RAD 10 COLOR))
(check-expect (star-img 255) (radial-star NUM-POINTS IN-RAD OUT-RAD 255 COLOR))

;(define (star-img n) empty-image)  ;stub

(@template Natural)

(define (star-img n)
  (radial-star NUM-POINTS IN-RAD OUT-RAD n COLOR))

