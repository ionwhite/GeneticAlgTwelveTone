;;;; Further Composition

( defmethod retrograde ( ( row list ) &aux r )
  ( reverse row ) ;  retrograde
)

( defmethod retrograde-pitch-only ( ( row list ) )
  ( mapcar #'cons ( reverse ( mapcar #'car row ) ) ( mapcar #'cdr row ) )
)

( defmethod inversion ( ( row list ) &aux head temp )
  ( setf head ( car row ) )
  ( setf temp ( cdr row ) )
  ( dolist ( x temp ) 
    ( setf ( car x ) ( cdr ( assoc ( car x ) *pitch-inversions* ) ) )
  )
  ( setf temp ( cons head temp ) )
)

( defmethod retrograde-inversion ( ( row list ) &aux temp )
  ( inversion ( retrograde row ) )
)

( defmethod transform ( ( row list ) &aux r )
  ( setf r ( random 3 ) )
  ( cond 
    ( ( = r 0 )
	  ( retrograde row )
	)
	( ( = r 1 )
	  ( retrograde-pitch-only row )
	)
	( ( = r 2 )
	  ( inversion row )
	)
	( ( = r 3 )
	  ( retrograde-inversion row )
	)
  )
)

( defmethod compose ( ( row list ) &aux nr dur piece )
  ( setf nr row )
  ( setf dur ( duration nr ) )
  
  ( cond
    ( ( = dur 12 )
	  ( setf piece ( append nr ( transform row ) ( transform row ) ( transform row ) ) ) 
	)
	( ( = dur 24 )
	  ( setf piece ( append nr ( transform row ) ) )
	)
    ( T
	  ( setf piece ( append nr ( transform row ) ( transform row ) ) )
	)
  )
  piece
)

( defmethod random-piece ()
  ( compose ( note-row ) )
)

( defmethod main-demo ()
  ( ga )
  ( setf mp ( jf-notation ( compose ( individual-row most-fit ) ) ) )
  ( setf rp ( jf-notation ( random-piece ) ) )
  ( format t "Piece composed with GA note row: ~%~A~%" mp )
  ( terpri )
  ( format t "Piece composed with random note row: ~%~A~%" rp )
)

( defmethod play ( ( piece string ) ( port integer ) )
  ( format ( socket-connect port ) "~A:~A" ( length piece ) piece )
)
