;;;; The 12-note line

( setf *pitches* '( C C# D D# E F F# G G# A A# B ) )
( setf *durations* '( S S I I I Q. H. ) )
( setf *length* 12 )

( defmethod note ()
  ( cons ( pick *pitches* ) ( pick *durations* ) )
)

( defmethod note-str ( ( int number ) )
  ( if ( = int 0 )
    NIL
    ( cons ( note ) ( note-str ( - int 1 ) ) )
  )
)

( defmethod note-row ( )
  ( note-str *length* )
)

;; demo utility

( defmethod demo-1 ()
  ( format t "A note: ~A" ( note ) )
  ( terpri )
  ( format t "Another note: ~A" ( note ) )
  ( terpri )
  ( format t "A note row: ~A" ( note-row ) )
  ( terpri )
  ( format t "Another note row: ~A" ( note-row ) )
)

;;;; Rendering in JFugue

( defmethod note-to-jfnote ( ( l list ) )
  ( format nil "~A~(~a~) " ( car l ) ( symbol-name ( cdr l ) ) )
)

( defmethod jf-notes ( ( l list ) )
  ( mapcar #'note-to-jfnote l )
)

( defmethod concatenator ( ( l list ) )
  ( if ( = 0 ( length l ) )
       l
	   ( concatenate 'string ( car l ) ( concatenator ( cdr l ) ) )
  )
)
( defmethod trim ( ( s string ) )
  ( string-trim " " s )
)

( defmethod jf-notation ( ( l list ) &aux nr )
  ( setf l ( jf-notes l ) )
  ( setf l ( concatenator l ) )
  ( setf l ( trim l ) )
)

;; demo utility

( defmethod demo-2 ()
  ( setf nr ( note-row ) )
  ( format t "A note row: ~A" nr )
  ( terpri )
  ( format t "A note row ready for JFugue: ~A" ( jf-notation nr ) )
)

;;;; Mutation and Crossover

;; Mutation

( defmethod mutation ( ( nr list ) &aux pos x y )
  ( setf pos ( random ( length nr ) ) )
  ( setf x ( note ) )
  ( setf y ( nth pos nr ) )
  
  ( loop while ( similar-p x y )
    do ( setf x ( note ) )
  )
  
  ( setf ( nth pos nr ) x )
  nr
)

( defun similar-p ( n1 n2 )
  ( or ( equal ( car n1 ) ( car n2 ) )
       ( equal ( cdr n1 ) ( cdr n2 ) ) )
)

;; Crossover

( defmethod crossover ( ( m list ) ( f list ) &aux pos temp )
  ( setf pos ( + 1 ( random ( length m ) ) ) )
  ( setf temp ( append ( first-n m pos ) ( rest-n f pos ) ) )
  
  ( mapcar #'cons ( mapcar #'car temp ) ( mapcar #'cdr f ) )
)

( defmethod first-n ( ( nr list ) ( n number ) )
  ( if ( = n 0 )
    NIL
	( cons ( car nr ) ( first-n ( cdr nr ) ( - n 1 ) ) )
  )
)

( defmethod rest-n ( ( nr list ) ( n number ) )
  ( if ( = n 0 )
    nr
    ( rest-n ( cdr nr ) ( - n 1 ) )
  )
)

;; demo utility

( defmethod demo-3 ()
  ( setf nr ( note-row ) )
  ( format t "A note row: ~A%" nr )
  ( format t "Mutation 1: ~A%" ( setf nr ( mutation nr ) ) )
  ( format t "Mutation 2: ~A%" ( setf nr ( mutation nr ) ) )
  ( format t "Mutation 3: ~A%" ( setf nr ( mutation nr ) ) )
  ( format t "Mutation 4: ~A%" ( setf nr ( mutation nr ) ) )
  ( terpri )
  ( setf m ( note-row ) )
  ( setf f ( note-row )
  ( format t "A 'mother' note row: ~A%" nr )
  ( format t "A 'father' note row: ~A%" nr )
  ( format t "Crossover between the two: ~A%" ( crossover m f ) )
)

;;;; The Fitness Metric for Pitch

( defmethod count-repeats ( ( nr list ) &aux temp fitness )
  ( setf temp ( mapcar #'car nr ) )
  ( setf fitness 0 )
  
  ( dolist ( x temp )
	( if ( > ( count x temp ) 1 )
	  ( progn
		( setf fitness ( + ( - ( count x temp ) 1 ) fitness ) )
		( setf temp ( take-from x temp ) )
	  )
	)
  )
  fitness
)

( defmethod count-lines ( ( nr list ) &aux temp )
  ( setf temp ( mapcar #'pitch-to-val ( mapcar #'car nr ) ) )

  ( + ( count-rising-lines temp ) ( count-falling-lines temp ) )
)

( defmethod count-rising-lines ( ( l list ) &aux i j )
  ( setf i 0 )
  ( setf j 0 )
  
  ( dotimes ( k ( - ( length l ) 1 ) j )
    ( if ( > ( nth ( + k 1 ) l ) ( nth k l ) )
	  ( progn
	    ( setf i ( + i 1 ) )
		( if ( >= i 3 ) 
		  ( setf j ( + j 1 ) )
		)
	  )
	  (setf i 0)
	)
  )
)

( defmethod count-falling-lines ( ( l list ) &aux i j )
  ( setf i 0 )
  ( setf j 0 )
  
  ( dotimes ( k ( - ( length l ) 1 ) j )
    ( if ( < ( nth ( + k 1 ) l ) ( nth k l ) )
	  ( progn
	    ( setf i ( + i 1 ) )
		( if ( >= i 3 ) 
		  ( setf j ( + j 1 ) )
		)
	  )
	  (setf i 0)
	)
  )
)

( defmethod pitch-to-val ( ( s symbol ) )
  ( cdr ( assoc s *pitch-numbers* ) )
)

( defmethod pitch-fitness ( ( nr list ) )
  ( + ( count-repeats nr ) ( count-lines nr ) )
)

;; demo utility

( defmethod demo-4 ()
  ( setf nr ( note-row ) )
  ( format t "A note row: ~A" nr )
  ( terpri )
  ( format t "The number of repeats in the note row: ~A~%" ( count-repeats nr ) )
  ( format t "The number of violating lines in the note row: ~A~%" ( count-lines nr ) )
  ( format t "The pitch fitness of the note row: ~A~%" ( pitch-fitness nr ) )
  ( terpri )
  ( setf nr ( note-row ) )
  ( format t "Another note row: ~A" nr )
  ( terpri )
  ( format t "The number of repeats in the note row: ~A~%" ( count-repeats nr ) )
  ( format t "The number of violating lines in the note row: ~A~%" ( count-lines nr ) )
  ( format t "The pitch fitness of the note row: ~A~%" ( pitch-fitness nr ) )
)

;;;; The Fitness Metric for Duration

( defmethod length-to-val ( ( s symbol ) )
  ( cond
    ( ( equal s 'S )
	  ( setf s 0.5 )
	)
	( ( equal s 'I )
	  ( setf s 1 )
	)
	( ( equal s 'Q. )
	  ( setf s 3 )
	)
	( ( equal s 'H. )
	  ( setf s 6 )
	)
  )
)

( defmethod duration ( ( nr list ) )
  ( sum ( mapcar #'length-to-val ( mapcar #'cdr nr ) ) )
)

( defmethod duration-fitness ( ( nr list ) &aux dur )
  ( setf dur ( duration nr ) )
  ( cond 
    ( ( or ( = dur 12 ) ( = dur 18 ) ( = dur 24 ) )
	  0
	)
    ( ( or ( < dur 12 ) ( <= dur 15 ) )
	  ( abs ( - dur 12 ) )
	)
	( ( or ( < dur 18 ) ( <= dur 21 ) )
	  ( abs ( - dur 18 ) )
	)
	( T
	  ( abs ( - dur 24 ) )
	)
  )
)

;; demo utility

( defmethod demo-5a ()
  ( setf nr ( note-row ) )
  ( format t "A note row: ~A" nr )
  ( terpri )
  ( format t "The duration fitness of the note row: ~A" ( duration-fitness nr ) )
  ( terpri )
  ( setf nr ( note-row ) )
  ( format t "Another note row: ~A" nr )
  ( terpri )
  ( format t "The duration fitness of the note row: ~A" ( duration-fitness nr ) )
)

( defmethod fitness ( ( nr list ) )
  ( + ( pitch-fitness nr ) ( duration-fitness nr ) )
)

;; demo utility

( defmethod demo-5b ()
  ( setf nr ( note-row ) )
  ( format t "A note row: ~A" nr )
  ( terpri )
  ( format t "The fitness of the note row: ~A" ( fitness nr ) )
  ( terpri )
  ( setf nr ( note-row ) )
  ( format t "Another note row: ~A" nr )
  ( terpri )
  ( format t "The fitness of the note row: ~A" ( fitness nr ) )
)

;;;; Individual and Population Classes

;; The Individual Class

( defclass individual ()
  (
    ( row :accessor individual-row :initarg :row )
	( fitness :accessor individual-fitness :initarg :fitness )
	( number :accessor individual-number :initarg :number )
  )
)

( defmethod random-individual ( &aux row )
  ( setf row ( note-row ) )
  ( make-instance 'individual
    :row row
	:fitness ( fitness row )
	:number 0
  )
)

( defmethod new-individual ( ( nr number ) ( row list ) )
  ( make-instance 'individual
    :row row
	:fitness ( fitness row )
	:number nr
  )
)


( defmethod display ( ( i individual ) )
  ( format t "~4A ~A [~A] ~%" ( individual-number i ) ( individual-row i ) ( individual-fitness i ) )
)

;; The Population Class

( setf *population-size* 100 )
( setf *selection-size* 10 )

( defclass population ()
  (
    ( individuals :accessor population-individuals :initarg :individuals )
	( generation :accessor population-generation :initarg :generation :initform 0 )
  )
)

( defmethod size ( ( p population ) )
  ( length ( population-individuals p ) )
)

( defmethod initial-population ( &aux individuals )
  ( setf individuals () )
  ( dotimes ( i *population-size* )
    ( push ( new-individual ( + i 1 ) ( note-row ) ) individuals )
  )
  ( make-instance 'population :individuals ( reverse individuals ) )
)

( defmethod average ( ( p population ) )
  ( /
    ( reduce #'+ ( mapcar #'individual-fitness ( population-individuals p ) ) ) 
	( size p )
  )
)

( defmethod select-individual ( ( p population ) &aux candidates mfi )
  ( setf candidates ( select-individuals p ) )
  ( setf mfi ( most-fit-individual candidates ) )
  mfi
)

( defmethod select-individuals ( ( p population ) &aux individuals candidates rn )
  ( setf individuals ( population-individuals p ) )
  ( setf candidates () )
  ( dotimes ( i *selection-size* )
    ( setf rn ( random *population-size* ) )
	( push ( nth rn individuals ) candidates )
  )
  candidates
)

( defmethod most-fit-individual ( ( lst list ) &aux max-value max-individual )
  ( reduce 
    ( lambda ( a b )
      ( if ( < ( individual-fitness a ) ( individual-fitness b ) ) a b )
    )
    lst
  )
)

( defmethod display  ( ( p population ) )
  ( format t "Population ~2A: ~%" ( population-generation p ) )
  ( dolist ( i ( population-individuals p ) )
    ( display i )
  )
)

;;;; Mutation, Copy, Crossover

;; Mutation

( defmethod mutate ( ( i individual ) &aux mutation )
  ( setf mutation ( mutation ( individual-row i ) ) )
  ( make-instance 'individual
    :number ( individual-number i )
	:row mutation
	:fitness ( fitness mutation )
  )
)

( defmethod maybe-mutate ( ( i individual ) )
  ( if ( <= ( + 1 ( random 100 ) ) *mutation-chance* )
    ( mutate i )
	i
  )
)

;; Copy

( defmethod perform-copies ( ( cp population ) ( np population ) )
  ( dotimes ( i ( nr-copies ) )
    ( perform-one-copy cp np )
  )
)

( defmethod nr-copies ()
  ( * ( / *copy-chance* 100 ) *population-size* )
)

( defmethod perform-one-copy ( ( cp population ) ( np population ) &aux m mm new-i )
  ( setf m ( select-individual cp ) )
  ( setf mm ( maybe-mutate m ) )
  ( setf ( individual-number mm ) ( + 1 ( size np ) ) )
  ( setf new-i ( new-individual ( + 1 ( size np ) ) ( individual-row mm ) ) )
  ( setf
    ( population-individuals np )
	( append ( population-individuals np ) ( list new-i ) )
  )
  NIL
)

( defmethod empty-population ( ( cp population ) &aux np )
  ( setf np ( make-instance 'population ) )
  ( setf ( population-individuals np ) () )
  ( setf ( population-generation np ) ( + 1 ( population-generation cp ) ) )
  np
)

;; Crossover

( defmethod perform-crossovers ( ( cp population ) ( np population ) )
  ( dotimes ( i ( nr-crossovers ) )
    ( perform-one-crossover cp np )
  )
)

( defmethod nr-crossovers ()
  ( * ( / *crossover-chance* 100 ) *population-size* )
)

( defmethod perform-one-crossover ( ( cp population ) ( np population ) &aux m mm mother father new-i )
  ( setf mother ( select-individual cp ) )
  ( setf father ( select-individual cp ) )
  ( setf m ( crossover mother father ) )
  ( setf mm ( maybe-mutate m ) )
  ( setf ( individual-number mm ) ( + 1 ( size np ) ) )
  ( setf new-i ( new-individual ( + 1 ( size np ) ) ( individual-row mm ) ) )
  ( setf ( population-individuals np ) ( append ( population-individuals np ) ( list new-i ) ) )
  NIL
)

( defmethod crossover ( ( mother individual ) ( father individual ) )
  ( new-individual 0 ( crossover ( individual-row mother ) ( individual-row father ) ) )
)

;;;; The GA

( setf most-fit NIL )

( defmethod next-generation ( ( cp population ) &aux np )
  ( setf np ( empty-population cp ) )
  ( perform-copies cp np )
  ( perform-crossovers cp np )
  np
)

( defmethod ga ( &aux p )
  ( setf p ( initial-population ) )
  ( terpri )
  ( summarize p )
  ( dotimes ( i *nr-generations* )
    ( setf p ( next-generation p ) )
	( check-average p )
  )
  ( summarize p )
  ( setf most-fit ( most-fit-individual ( population-individuals p ) ) )
)

( defmethod summarize ( ( p population ) &aux pmfi )
  ( display p )
  ( check-average p )
  ( terpri )
  ( setf pmfi ( most-fit-individual ( population-individuals p ) ) )
  ( format t "Most fit individual of this population:~%" )
  ( display pmfi )
  ( jf-notation ( individual-row pmfi ) )
)

( defmethod check-average ( ( p population ) )
  ( format t "average fitness of population ~A = ~A~%"
    ( population-generation p )
	( float ( average p ) )
  )
)
