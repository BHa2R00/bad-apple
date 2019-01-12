;;USE SGK----------------------------------------------------------------------------------------------------

(defvar *anchor_layer_number*)

(cffi:define-foreign-library 
  libsgk-rw
  (t (:or
	   "./rw.so"
	   )))

(cffi:use-foreign-library libsgk-rw)

(cffi:defcfun ("r642d" r642d) :double
			  (v :unsigned-long))

(cffi:defcfun ("d2r64" d2r64) :unsigned-long
			  (v :double))

(defun math-pow (a x)
  (if (= a 0)
	0
	(if (= x 0)
	  1
	  (exp (* x (log a))))))

(defun math-log (a x)
  (/ (log x) (log  a)))

(defun rd-u2 (str)
  (let ((u2 0))
	(setf (ldb (byte 8 8) u2) (read-byte str nil 'eof))
	(setf (ldb (byte 8 0) u2) (read-byte str nil 'eof))
	u2))

(defun rd-u2-1 (str)
  (let ((u2 0))
	(setf (ldb (byte 8 0) u2) (read-byte str nil 'eof))
	(setf (ldb (byte 8 8) u2) (read-byte str nil 'eof))
	u2))

(defun rd-u4 (str-in)
  (let ((u4 0))
	(setf (ldb (byte 16 16) u4) (rd-u2 str-in))
	(setf (ldb (byte 16 0) u4) (rd-u2 str-in))
	u4)
  )

(defun rd-u4-1 (str-in)
  (let ((u4 0))
	(setf (ldb (byte 16 0) u4) (rd-u2-1 str-in))
	(setf (ldb (byte 16 16) u4) (rd-u2-1 str-in))
	u4))

(defun rd-u8 (str-in)
  (let ((u8 0))
	(setf (ldb (byte 32 32) u8) (rd-u4 str-in))
	(setf (ldb (byte 32 0) u8) (rd-u4 str-in))
	u8)
  )

(defun rd-u16 (str-in)
  (let ((u16 0))
	(setf (ldb (byte 64 64) u16) (rd-u8 str-in))
	(setf (ldb (byte 64 0) u16) (rd-u8 str-in))
	u16)
  )

(defun rd-u2s-v (n str)
  (let ((lst (list)) (u2))
	(dotimes (m n)
	  (setf u2 (rd-u2 str))
	  (push u2 lst))
	(reverse lst)))

(defun sgk-f000 (str)
  (let ((lst (list)))
	(do ((u2 (rd-u2 str)
			 (rd-u2 str)))
	  ((eql (car lst) #x0000) (reverse (cdr lst)))
	  (if (> u2 0)
	   (push (rd-u2s-v (round (- (/ u2 2) 1)) str) lst)
	   (push #x0000 lst)))))

(defun sgk-f001 (l000)
  (dolist (elm l000)
	(cond
	  ((= (car elm) #x0102) (setf (car elm) 'RT_BGNLIB))
	  ((= (car elm) #x0206) (setf (car elm) 'RT_LIBNAME))
	  ((= (car elm) #x0305) (setf (car elm) 'RT_UNITS))
	  ((= (car elm) #x0400) (setf (car elm) 'RT_ENDLIB))
	  ((= (car elm) #x0502) (setf (car elm) 'RT_BGNSTR))
	  ((= (car elm) #x0606) (setf (car elm) 'RT_STRNAME))
	  ((= (car elm) #x0700) (setf (car elm) 'RT_ENDSTR))
	  ((= (car elm) #x0800) (setf (car elm) 'RT_BOUNDARY))
	  ((= (car elm) #x0900) (setf (car elm) 'RT_PATH))
	  ((= (car elm) #x0A00) (setf (car elm) 'RT_SREF))
	  ((= (car elm) #x0B00) (setf (car elm) 'RT_AREF))
	  ((= (car elm) #x0C00) (setf (car elm) 'RT_TEXT))
	  ((= (car elm) #x0D02) (setf (car elm) 'RT_LAYER))
	  ((= (car elm) #x0E02) (setf (car elm) 'RT_DATATYPE))
	  ((= (car elm) #x0F03) (setf (car elm) 'RT_WIDTH))
	  ((= (car elm) #x1003) (setf (car elm) 'RT_XY))
	  ((= (car elm) #x1100) (setf (car elm) 'RT_ENDEL))
	  ((= (car elm) #x1206) (setf (car elm) 'RT_SNAME))
	  ((= (car elm) #x1302) (setf (car elm) 'RT_COLROW))
	  ((= (car elm) #x1400) (setf (car elm) 'RT_TEXTNODE))
	  ((= (car elm) #x1500) (setf (car elm) 'RT_NODE))
	  ((= (car elm) #x1602) (setf (car elm) 'RT_TEXTTYPE))
	  ((= (car elm) #x1701) (setf (car elm) 'RT_PRESENTATION))
	  ((= (car elm) #x1800) (setf (car elm) 'RT_SPACING))
	  ((= (car elm) #x1906) (setf (car elm) 'RT_STRING))
	  ((= (car elm) #x1A01) (setf (car elm) 'RT_STRANS))
	  ((= (car elm) #x1B05) (setf (car elm) 'RT_MAG))
	  ((= (car elm) #x1C05) (setf (car elm) 'RT_ANGLE))
	  ((= (car elm) #x1D00) (setf (car elm) 'RT_UINTEGER))
	  ((= (car elm) #x1E00) (setf (car elm) 'RT_USTRING))
	  ((= (car elm) #x1F06) (setf (car elm) 'RT_REFLIB))
	  ((= (car elm) #x2006) (setf (car elm) 'RT_FONTS))
	  ((= (car elm) #x2102) (setf (car elm) 'RT_PATHTYPE))
	  ((= (car elm) #x2202) (setf (car elm) 'RT_GENERATIONS))
	  ((= (car elm) #x2306) (setf (car elm) 'RT_ATTRTABLE))
	  ((= (car elm) #x2406) (setf (car elm) 'RT_STYPTABLE))
	  ((= (car elm) #x2502) (setf (car elm) 'RT_STRTYPE))
	  ((= (car elm) #x2601) (setf (car elm) 'RT_ELFLAGS))
	  ((= (car elm) #x2703) (setf (car elm) 'RT_ELKEY))
	  ((= (car elm) #x2800) (setf (car elm) 'RT_LINKTYPE))
	  ((= (car elm) #x2900) (setf (car elm) 'RT_LINKKEYS))
	  ((= (car elm) #x2A02) (setf (car elm) 'RT_NODETYPE))
	  ((= (car elm) #x2B02) (setf (car elm) 'RT_PROPATTR))
	  ((= (car elm) #x2C06) (setf (car elm) 'RT_PROPVALUE))
	  ((= (car elm) #x2D00) (setf (car elm) 'RT_BOX))
	  ((= (car elm) #x2E02) (setf (car elm) 'RT_BOXTYPE))
	  ((= (car elm) #x2F03) (setf (car elm) 'RT_PLEX))
	  ((= (car elm) #x3003) (setf (car elm) 'RT_BGNEXTN))
	  ((= (car elm) #x3103) (setf (car elm) 'RT_ENDEXTN))
	  ((= (car elm) #x3202) (setf (car elm) 'RT_TAPENUM))
	  ((= (car elm) #x3302) (setf (car elm) 'RT_TAPECODE))
	  ((= (car elm) #x3401) (setf (car elm) 'RT_STRCLASS))
	  ((= (car elm) #x3503) (setf (car elm) 'RT_RESERVED))
	  ((= (car elm) #x3602) (setf (car elm) 'RT_FORMAT))
	  ((= (car elm) #x3706) (setf (car elm) 'RT_MASK))
	  ((= (car elm) #x3800) (setf (car elm) 'RT_ENDMASK))
	  ((= (car elm) #x3902) (setf (car elm) 'RT_LIBDIRSIZE))
	  ((= (car elm) #x3A06) (setf (car elm) 'RT_SRFNAME))
	  ((= (car elm) #x3B02) (setf (car elm) 'RT_LIBSECUR))
	  ((= (car elm) #x0003) (setf (car elm) 'PRES_MASK_H))
	  ((= (car elm) #x000C) (setf (car elm) 'PRES_MASK_V))
	  ((= (car elm) #x0030) (setf (car elm) 'PRES_MASK_FONT))
	  ((= (car elm) #x0002) (setf (car elm) 'STRANS_ABSMAG))
	  ((= (car elm) #x0004) (setf (car elm) 'STRANS_ABSANGLE))
	  ((= (car elm) #x8000) (setf (car elm) 'STRANS_REFLECTION))
	  ((= (car elm) #x0001) (setf (car elm) 'ELFLAG_TEMPLATE))
	  ((= (car elm) #x0002) (setf (car elm) 'ELFLAG_EXTERNAL))))
  l000)

(defun is-name? (asii)
  (let ((c (code-char asii)))
  (if (and (char>= c #\!) (char<= c #\~))
	t
	nil)))

(defun l2s (l)
  (let ((buf ""))
	(dolist (u2 l)
	  (if (is-name? (ldb (byte 8 8) u2))
		(setf buf (concatenate 'string buf (string (code-char (ldb (byte 8 8) u2)))))
		nil)
	  (if (is-name? (ldb (byte 8 0) u2))
		(setf buf (concatenate 'string buf (string (code-char (ldb (byte 8 0) u2)))))
		nil))
	buf))

(defun u2s2u4s (v)
  (let ((lst (list)) (u4 0))
	(dotimes (n (length v))
	  (if (oddp n)
		(progn
		  (setf (ldb (byte 16 16) u4) (aref v (1- n)))
		  (setf (ldb (byte 16 0) u4) (aref v n))
		  (push u4 lst)) nil))
	(apply 'vector (reverse lst))))

(defun u4s2u8s (v)
  (let ((lst (list)) (u8 0))
	(dotimes (n (length v))
	  (if (oddp n)
		(progn
		  (setf (ldb (byte 32 32) u8) (aref v (1- n)))
		  (setf (ldb (byte 32 0) u8) (aref v n))
		  (push u8 lst)) nil))
	(apply 'vector (reverse lst))))

(defun u8s2ds (v)
  (let ((lst (list)))
	(dotimes (n (length v))
	  (push (r642d (aref v n)) lst))
	(apply 'vector (reverse lst))))

(defun sgk-f002 (l001)
  (let ((lst (list (list 'RT_HEADER 5))))
	(dolist (elm l001)
	  (cond
		((eql (car elm) 'RT_BGNLIB)
		 (push (list 'RT_BGNLIB
					 (apply 'vector (cdr elm))) lst))
		((eql (car elm) 'RT_LIBNAME)
		 (push (list 'RT_LIBNAME
					 (string-right-trim ".DB" (l2s (cdr elm)))) lst))
		((eql (car elm) 'RT_STRNAME)
		 (push (list 'RT_STRNAME
					 (l2s (cdr elm))) lst))
		((eql (car elm) 'RT_SNAME)
		 (push (list 'RT_SNAME
					 (l2s (cdr elm))) lst))
		((eql (car elm) 'RT_STRING)
		 (push (list 'RT_STRING
					 (l2s (cdr elm))) lst))
		((eql (car elm) 'RT_ANGLE)
		 (push (list 'RT_ANGLE
					 (aref (u8s2ds (u4s2u8s (u2s2u4s (apply 'vector (cdr elm))))) 0)) lst))
		((eql (car elm) 'RT_MAG)
		 (push (list 'RT_MAG
					 (aref (u8s2ds (u4s2u8s (u2s2u4s (apply 'vector (cdr elm))))) 0)) lst))
		((eql (car elm) 'RT_COLROW)
		 (push (list 'RT_COLROW
					 (apply 'vector (cdr elm))) lst))
		((eql (car elm) 'RT_XY)
		 (push (list 'RT_XY
					 (u2s2u4s (apply 'vector (cdr elm)))) lst))
		((eql (car elm) 'RT_WIDTH)
		 (push (list 'RT_WIDTH
					 (aref (u2s2u4s (apply 'vector (cdr elm))) 0)) lst))
		((eql (car elm) 'RT_UNITS)
		 (push (list 'RT_UNITS
					 (u8s2ds (u4s2u8s (u2s2u4s (apply 'vector (cdr elm)))))) lst))
		((eql (car elm) 'RT_BGNSTR)
		 (push (list 'RT_BGNSTR
					 (apply 'vector (cdr elm))) lst))
		(t (push elm lst))))
	(reverse lst)))

(defun sgk-f003 (l002)
  (let ((lst (list)) (ena nil) (elm (list)))
	(dolist (elm0 l002)
	  (cond
		((eql (car elm0) 'RT_ENDEL)
		 (progn
		   (push (reverse (push 'RT_ENDEL elm)) lst)
		   (setf elm (list))
		   (setf ena nil))))
	  (if ena
		(push elm0 elm)
		nil)
	  (cond
		((or
		   (eql (car elm0) 'RT_BOUNDARY)
		   (eql (car elm0) 'RT_PATH)
		   (eql (car elm0) 'RT_SREF)
		   (eql (car elm0) 'RT_AREF)
		   (eql (car elm0) 'RT_TEXT)
		   (eql (car elm0) 'RT_NODE)
		   (eql (car elm0) 'RT_BOX)
		   )
		 (progn
		   (setf elm elm0)
		   (setf ena t))))
	  (if ena nil
		(if (eql (car elm0) 'RT_ENDEL)
		  nil (push elm0 lst))))
	(reverse lst)))

(defun sgk-f004 (l003)
  (let ((lst (list)) (ena nil) (elm (list)))
	(dolist (elm0 l003)
	  (cond
		((eql (car elm0) 'RT_ENDSTR)
		 (progn
		   (push (reverse (push 'RT_ENDSTR elm)) lst)
		   (setf elm (list))
		   (setf ena nil))))
	  (if ena
		(push elm0 elm)
		nil)
	  (cond
		((eql (car elm0) 'RT_BGNSTR)
		 (progn
		   (setf elm (reverse elm0))
		   (setf ena t))))
	  (if ena nil
		(if (eql (car elm0) 'RT_ENDSTR)
		  nil (push elm0 lst))))
	(reverse lst)))

(defun sgk-f005 (l004)
  (let ((lst (list)) (ena nil) (elm (list)))
	(dolist (elm0 l004)
	  (cond
		((eql (car elm0) 'RT_ENDLIB)
		 (progn
		   (push (reverse elm) lst)
		   (push 'RT_ENDLIB lst)
		   (setf elm (list))
		   (setf ena nil))))
	  (if ena
		(push elm0 elm)
		nil)
	  (cond
		((eql (car elm0) 'RT_UNITS)
		 (progn
		   (setf elm (reverse elm0))
		   (setf ena t))))
	  (if ena nil
		(if (eql (car elm0) 'RT_ENDLIB)
		  nil (push elm0 lst))))
	(reverse lst)))

(defun f-return-srctree (fi)
  (let ((srctree '()))
	(with-open-file (str-in fi :direction	:input
							:element-type	'(unsigned-byte 8))
	  (dotimes (n 3)
		(format t "~x~%" (rd-u2 str-in)))
	  (setq srctree (sgk-f005 (sgk-f004 (sgk-f003 (sgk-f002 (sgk-f001 (sgk-f000 str-in)))))))
	  )))

(defun flatten-tree (tree)
  (cond
	((null tree) nil)
	((atom tree) (list tree))
	(t (mapcan #'flatten-tree tree))))


(defun wt-u2 (str-out var)
  (write-byte (ldb (byte 8 8) var) str-out)
  (write-byte (ldb (byte 8 0) var) str-out))

(defun wt-u2-1 (str-out var)
  (write-byte (ldb (byte 8 0) var) str-out)
  (write-byte (ldb (byte 8 8) var) str-out))

(defun wt-u4 (str-out var)
  (wt-u2 str-out (ldb (byte 16 16) var))
  (wt-u2 str-out (ldb (byte 16 0) var)))

(defun wt-u4-1 (str-out var)
  (wt-u2-1 str-out (ldb (byte 16 0) var))
  (wt-u2-1 str-out (ldb (byte 16 16) var)))

(defun wt-u8 (str-out var)
  (wt-u4 str-out (ldb (byte 32 32) var))
  (wt-u4 str-out (ldb (byte 32 0) var)))

(defun wt-u8-1 (str-out var)
  (wt-u4-1 str-out (ldb (byte 32 0) var))
  (wt-u4-1 str-out (ldb (byte 32 32) var)))

(defun wt-u16 (str-out var)
  (wt-u8 str-out (ldb (byte 64 64) var))
  (wt-u8 str-out (ldb (byte 64 0) var)))

(defun wt-u16-1 (str-out var)
  (wt-u8-1 str-out (ldb (byte 64 0) var))
  (wt-u8-1 str-out (ldb (byte 64 64) var)))

(defun wt-dt-string5 (str-out stri)
  (let ((u2 0) (len (length stri)))
	(dotimes (n (floor (float (/ len 2))))
	  (setf (ldb (byte 8 8) u2) (char-code (aref stri (* 2 n))))
	  (setf (ldb (byte 8 0) u2) (char-code (aref stri (1+ (* 2 n)))))
	  (wt-u2 str-out u2)
	  )
	(if (oddp len)
	  (progn
		(setf (ldb (byte 8 8) u2) (char-code (aref stri (1- len))))
		(setf (ldb (byte 8 0) u2) #x00)
		(wt-u2 str-out u2))
	  nil)
	))

(defun wt-dt-string6 (str-out stri)
  (wt-dt-string7 str-out stri)
  (if (oddp (length stri))
	(write-byte #x2e str-out)
	nil))

(defun wt-dt-string7 (str-out stri)
  (loop for n from 0 to (1- (length stri)) do
		(write-byte (char-code (aref stri n)) str-out)))

(defun wt-dt-bitarray (str-out ary sel)
  (loop for n from 0 to (1- (array-total-size ary)) do
		(cond
		  ((= 2 sel) (wt-u2 str-out (aref ary n)))
		  ((= 4 sel) (wt-u4 str-out (aref ary n)))
		  ((= 8 sel) (wt-u8 str-out (aref ary n)))
		  )))

(defun wt-dt-real64 (str-out d)
  (wt-u8 str-out (d2r64 d)))

(defun wt-dt-real642 (str-out d)
  (wt-u8 str-out (d2r64 d)))

(defun sgk-f100 (fo tree)
  (let ((lst (flatten-tree tree)))
	(setq lst (make-array (length lst) :initial-contents lst))
	(with-open-file (str-out fo :direction	:output
							 :if-does-not-exist :create
							 :if-exists		:supersede
							 :element-type	'(unsigned-byte 8))
	  (loop for n from 0 to (1- (array-total-size lst)) do
			   (sgk-f101 str-out lst n)))))

(defun length1 (stri)
  (if (oddp (length stri))
	(1+ (length stri))
	(length stri)))

(defun sgk-f101 (str-out lst n)
  (cond
	((eql 'RT_HEADER (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out #x0002)
										  (wt-u2 str-out (aref lst (1+ n)))
										  (wt-u2 str-out #x001c)
										  ))
	((eql 'RT_BGNLIB (aref lst n)) (progn
										  (wt-u2 str-out #x0102)
										  (wt-dt-bitarray str-out (aref lst (1+ n)) 2)
										  ))
	((eql 'RT_LIBNAME (aref lst n)) (progn
										  (wt-u2 str-out (+ 6 (* 1 (length1 (aref lst (1+ n))))))
										  (wt-u2 str-out #x0206)
										  (wt-dt-string6 str-out (aref lst (1+ n)))
										  (wt-u2 str-out #x4442)
										  (wt-u2 str-out #x0014)
										  ))
	((eql 'RT_UNITS (aref lst n)) (progn
										  (wt-u2 str-out #x0305)
										  (wt-u8 str-out #x3E4189374BC6A7EC)
										  (wt-u8 str-out #x3944B82FA09B5A50)
										  ))
	((eql 'RT_ENDLIB (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out #x0400)
										  (dotimes (m 20)
											(wt-u2 str-out #x0000))
										  ))
	((eql 'RT_BGNSTR (aref lst n)) (progn
										  (wt-u2 str-out #x001c)
										  (wt-u2 str-out #x0502)
										  (wt-dt-bitarray str-out (aref lst (1+ n)) 2)
										  ))
	((eql 'RT_STRNAME (aref lst n)) (progn
										  (wt-u2 str-out (+ 4 (* 1 (length1 (aref lst (1+ n))))))
										  (wt-u2 str-out #x0606)
										  (wt-dt-string5 str-out (aref lst (1+ n)))
										  ))
	((eql 'RT_BOUNDARY (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out #x0800)
										  ))
	((eql 'RT_PATH (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out #x0900)
										  ))
	((eql 'RT_SREF (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out #x0A00)
										  ))
	((eql 'RT_AREF (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out #x0B00)
										  ))
	((eql 'RT_TEXT (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out #x0C00)
										  ))
	((eql 'RT_NODE (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out #x1500)
										  ))
	((eql 'RT_BOX (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out #x2D00)
										  ))
	((eql 'RT_ENDSTR (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out #x0700)
										  ))
	((eql 'RT_LAYER (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out #x0D02)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((eql 'RT_DATATYPE (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out #x0E02)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((eql 'RT_PATHTYPE (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out #x2102)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((eql 'RT_XY (aref lst n)) (progn
										  (wt-u2 str-out (+ 4 (* 4 (length (aref lst (1+ n))))))
										  (wt-u2 str-out #x1003)
										  (wt-dt-bitarray str-out (aref lst (1+ n)) 4)
										  ))
	((eql 'RT_ENDEL (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out #x1100)
										  ))
	((eql 'RT_SNAME (aref lst n)) (progn
										  (wt-u2 str-out (+ 4 (* 1 (length1 (aref lst (1+ n))))))
										  (wt-u2 str-out #x1206)
										  (wt-dt-string5 str-out (aref lst (1+ n)))
										  ))
	((eql 'RT_STRANS (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out #x1A01)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((eql 'RT_ANGLE (aref lst n)) (progn
										  (wt-u2 str-out #x000c)
										  (wt-u2 str-out #x1C05)
										  (wt-dt-real64 str-out (aref lst (1+ n)))
										  ))
	((eql 'RT_COLROW (aref lst n)) (progn
										  (wt-u2 str-out #x0008)
										  (wt-u2 str-out #x1302)
										  (wt-dt-bitarray str-out (aref lst (1+ n)) 2)
										  ))
	((eql 'RT_TEXTTYPE (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out #x1602)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((eql 'RT_PRESENTATION (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out #x1701)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((eql 'RT_MAG (aref lst n)) (progn
										  (wt-u2 str-out #x000c)
										  (wt-u2 str-out #x1B05)
										  (wt-dt-real642 str-out (aref lst (1+ n)))
										  ;(wt-u8 str-out (aref lst (1+ n)))
										  ))
	((eql 'RT_STRING (aref lst n)) (progn
										  (wt-u2 str-out (+ 4 (* 1 (length1 (aref lst (1+ n))))))
										  (wt-u2 str-out #x1906)
										  (wt-dt-string5 str-out (aref lst (1+ n)))
										  ))
	((eql 'RT_WIDTH (aref lst n)) (progn
										  (wt-u2 str-out #x0008)
										  (wt-u2 str-out #x0F03)
										  (wt-u4 str-out (aref lst (1+ n)))
										  ))
	)
  )

(defun sgk-rd-gds (fi) (f-return-srctree fi))

(defun sgk-wt-gds (fo tree) (sgk-f100 fo tree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *srcunits*)

(defun by-car-stri (lst stri)
  (if (cdr lst)
	(if (listp (car lst))
	  (if (not (listp (car lst)))
		(if (eql stri (car lst))
		  (car lst)
		  (by-car-stri (cdr lst) stri))
		(if (eql stri (car (car lst)))
		  (car lst)
		  (by-car-stri (cdr lst) stri)))
	  (if (eql stri (car lst))
		(car lst)
		(by-car-stri (cdr lst) stri)))
	(if (not (listp (car (cdr lst))))
	  (if (eql stri (car (cdr lst)))
		(cdr lst) nil) nil)))

(defun itoa (i)
  (let ((lst1 (list)) (s ""))
	(loop
	  (when (< i 10)
		(progn
		  (push (floor (float (mod i 10))) lst1)
		  (dolist (i2 lst1)
			(setf s (concatenate 'string s (string (code-char (+ i2 48))))))
		  (return s)))
	  (push (floor (float (mod i 10))) lst1)
	  (setf i (floor (float (/ i 10)))))))

(defmacro string+ (&rest body)
  `(concatenate 'string ,@body))

(defun max-of (lst)
  (let ((cmd '(max)))
	(dolist (n lst)
	  (push n cmd))
	(eval (reverse cmd))))

(defun min-of (lst)
  (let ((cmd '(min)))
	(dolist (n lst)
	  (push n cmd))
	(eval (reverse cmd))))

(defun sta-angle (angle)
  (if (>= angle 360.0d0)
	(mod angle 360.0d0)
	angle))

(defun v1-v2 (v1)
  (let ((v2 (list)))
	(dotimes (n (length v1))
	  (if (oddp n)
		(push (vector (aref v1 (1- n)) (aref v1 n)) v2)))
	(make-array (list (length v2)) :initial-contents (reverse v2))))

(defun v2-v1 (v2)
  (let ((v1 (list)))
	(dotimes (n (length v2))
	  (push (aref (aref v2 n) 0) v1)
	  (push (aref (aref v2 n) 1) v1))
	(make-array (list (length v1)) :initial-contents (reverse v1))))

(defun v1s-v2s (v1s)
  (let ((v2s (list)))
	(dolist (v1 v1s)
	  (push (v1-v2 v1) v2s))
	v2s))

(defun v2s-v1s (v2s)
  (let ((v1s (list)))
	(dolist (v2 v2s)
	  (push (v2-v1 v2) v1s))
	v1s))

(defun xmin (v2)
  (let ((xs (list)))
	(dotimes (n (length v2))
	  (push (aref (aref v2 n) 0) xs))
	(min-of xs)))

(defun ymin (v2)
  (let ((ys (list)))
	(dotimes (n (length v2))
	  (push (aref (aref v2 n) 1) ys))
	(min-of ys)))

(defun xmax (v2)
  (let ((xs (list)))
	(dotimes (n (length v2))
	  (push (aref (aref v2 n) 0) xs))
	(max-of xs)))

(defun ymax (v2)
  (let ((ys (list)))
	(dotimes (n (length v2))
	  (push (aref (aref v2 n) 1) ys))
	(max-of ys)))

(defun xsmin (v2s)
  (let ((mins (list)))
	(dolist (v2 v2s)
	  (push (xmin v2) mins))
	(min-of mins)))

(defun ysmin (v2s)
  (let ((mins (list)))
	(dolist (v2 v2s)
	  (push (ymin v2) mins))
	(min-of mins)))

(defun xsmax (v2s)
  (let ((maxs (list)))
	(dolist (v2 v2s)
	  (push (xmax v2) maxs))
	(max-of maxs)))

(defun ysmax (v2s)
  (let ((maxs (list)))
	(dolist (v2 v2s)
	  (push (ymax v2) maxs))
	(max-of maxs)))

(defun vector+ (v1 v2)
  (vector (+ (aref v1 0) (aref v2 0)) (+ (aref v1 1) (aref v2 1))))

(defun vector- (v1 v2)
  (vector (- (aref v1 0) (aref v2 0)) (- (aref v1 1) (aref v2 1))))

(defun vector*num (v1 num)
  (vector (* (aref v1 0) num) (* (aref v1 1) num)))

(defun vector/num (v1 num)
  (vector (div (aref v1 0) num) (* (aref v1 1) num)))

(defun vector-dot (v1 v2)
  (vector (* (aref v1 0) (aref v2 0)) (* (aref v1 1) (aref v2 1))))

(defun div (a b)
  (if (= 0 b)
	nil
	(/ a b)))

(defun div1 (a b)
  (floor (float (div a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro rt-lib (name units)
  `(list (list 'RT_HEADER 5)
		(list 'RT_BGNLIB #(117 1 11 16 17 41 117 1 11 16 21 29))
		(list 'RT_LIBNAME ,name)
		(concatenate 'list (list 'RT_UNITS 9.999999999999998d-4 9.999999999999999d-10) (reverse ,units))
		'RT_ENDLIB))

(defmacro rt-cell (name &rest elms)
  `(concatenate 'list
	 (list
	   (list 'RT_BGNSTR #(70 1 1 8 0 0 118 2 2 14 29 35))
	   (list 'RT_STRNAME ,name))
	 ,@elms
	 (list 'RT_ENDSTR)))

(defmacro rt-sref (vsref)
  `(let ((sname (aref ,vsref 0))
		 (strans (aref ,vsref 1))
		 (angle (aref ,vsref 2))
		 (xy (aref ,vsref 3))
		 )
	 (cond
	 ((eql strans #x4000)
	  (list 'RT_SREF (list 'RT_SNAME sname)
		   (list 'RT_STRANS #x8000)
		   (list 'RT_ANGLE (sta-angle (+ 180.0d0 angle)))
		   (list 'RT_XY xy)
		   'RT_ENDEL))
	 ((eql strans #x0000)
	  (list 'RT_SREF (list 'RT_SNAME sname)
		   (list 'RT_STRANS strans)
		   (list 'RT_ANGLE angle)
		   (list 'RT_XY xy)
		   'RT_ENDEL))
	 ((eql strans #x8000)
	  (list 'RT_SREF (list 'RT_SNAME sname)
		   (list 'RT_STRANS strans)
		   (list 'RT_ANGLE angle)
		   (list 'RT_XY xy)
		   'RT_ENDEL)))))

(defmacro rt-boundary (layer v2)
  `(list 'RT_BOUNDARY
		(list 'RT_LAYER ,layer)
		(list 'RT_DATATYPE 0)
		(list 'RT_XY (v2-v1 ,v2))
		'RT_ENDEL))

(defmacro rt-text (vtext)
  `(let (
		 (layer (aref ,vtext 0))
		 (xy (aref ,vtext 1))
		 (stri (aref ,vtext 2))
		 )
	 (list 'RT_TEXT
	   (list 'RT_LAYER layer)
	   (list 'RT_TEXTTYPE 0)
	   (list 'RT_PRESENTATION 5)
	   (list 'RT_STRANS 0)
	   (list 'RT_MAG 0.2d0)
	   (list 'RT_XY xy)
	   (list 'RT_STRING stri)
	   'RT_ENDEL)))

(defmacro rt-box (layer xmin ymin xmax ymax)
  `(rt-boundary ,layer (vector
						 (vector ,xmin ,ymin)
						 (vector ,xmax ,ymin)
						 (vector ,xmax ,ymax)
						 (vector ,xmin ,ymax)
						 (vector ,xmin ,ymin)
						 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lib (name units)
  (rt-lib name units))

(defun get-units (lib) (cddr (cadddr lib)))

(defun get-cell (units name)
  (dolist (str units)
	(when (string= name (get-cellname str))
	  (return str))))

(defun get-cellname (str)
  (cadr (by-car-stri str 'RT_STRNAME)))

(defun srefp (elm)
  (if (string= (car elm) 'RT_SREF) t nil))

(defun get-snames (str)
  (let ((lst (list)))
	 (dolist (elm str)
	   (if (listp elm)
		 (if (srefp elm)
		   (push (cadr (cadr elm)) lst)
		   nil) nil))
	 lst))

(defun get-cellnames (units)
  (let ((lst (list)))
	 (dolist (str units)
	   (push (get-cellname str) lst))
	 lst))

(defun in-units-p (units cell-name)
  (let ((names (get-cellnames units)) (rt nil))
	 (dolist (name names)
	   (if (string= name cell-name)
		 (setf rt t)
		 nil))
	 rt))

(defun have-sref-p (strname)
  (if (eql nil (get-snames (get-cell *srcunits* strname)))
	nil t))

(defun depend-2 (strname)
(if (have-sref-p strname)
  (let ((lst0 (get-snames (get-cell *srcunits* strname)))
		(lst1 (list)))
	(dolist (sname lst0)
	  (push sname lst1)
	  (push (depend-2 sname)
		lst1))
	(clearify (flatten-tree lst1)))
  (concatenate 'list (list strname) (get-snames (get-cell *srcunits* strname)))))

(defun withdraw (name)
  (let ((outunits) (names (concatenate 'list (list name) (depend-2 name))) (cnt 0))
	(format t "withdraw depends")
	(dolist (name1 names)
	  (push (if (in-units-p outunits name1) nil (get-cell *srcunits* name1)) outunits)
	  (incf cnt))
	(format t "~%")
	outunits))

(defun sref-sname (sref)
  (cadr (by-car-stri sref 'RT_SNAME)))

(defun sref-strans (sref)
  (cadr (by-car-stri sref 'RT_STRANS)))

(defun sref-angle (sref)
  (cadr (by-car-stri sref 'RT_ANGLE)))

(defun sref-xy (sref)
  (cadr (by-car-stri sref 'RT_XY)))

(defun cell-strname (cell)
  (cadr (by-car-stri cell 'RT_STRNAME)))

(defun vsref-real-xy (vsref pt)
	(let ((sref1 (rt-sref vsref)) (mir) (ang) (x (aref pt 0)) (y (aref pt 1)) (x0) (y0))
	  (setf mir (sref-strans sref1))
	  (setf ang (sref-angle sref1))
	  (setf x0 (aref (sref-xy sref1) 0))
	  (setf y0 (aref (sref-xy sref1) 1))
	  (cond
		((and (eql #x0000 mir) (eql 0.0d0 ang)) (vector (+ x0 x)			(+ y0 y)))
		((and (eql #x0000 mir) (eql 90.0d0 ang)) (vector (+ x0 (* -1 y))			(+ y0 x)))
		((and (eql #x0000 mir) (eql 180.0d0 ang)) (vector (+ x0 (* -1 x))	(+ y0 (* -1 y))))
		((and (eql #x0000 mir) (eql 270.0d0 ang)) (vector (+ x0 y)			(+ y0 (* -1 x))))
		((and (eql #x8000 mir) (eql 0.0d0 ang)) (vector (+ x0 x)			(+ y0 (* -1 y))))
		((and (eql #x8000 mir) (eql 90.0d0 ang)) (vector (+ x0 y) 			(+ y0 x)))
		((and (eql #x8000 mir) (eql 180.0d0 ang)) (vector (+ x0 (* -1 x))	(+ y0 y)))
		((and (eql #x8000 mir) (eql 270.0d0 ang)) (vector (+ x0 (* -1 y))	(+ y0 (* -1 x)))))))

(defun vsref-real-v2 (vsref v2)
  (let ((v2-1 (make-array (list (length v2)))))
	(dotimes (n (length v2))
	  (setf (aref v2-1 n) (vsref-real-xy vsref (aref v2 n))))
	v2-1))

(defun vsref-real-v2s (vsref v2s)
  (let ((v2s-1 (list)))
	(dolist (v2 v2s)
	  (push (vsref-real-v2 vsref v2) v2s-1))
	v2s-1))

(defun vsref-real-xys (vsref pts)
  (let ((xys (list)))
	(dolist (pt pts)
	  (push (vsref-real-xy vsref pt) xys))
	xys))

(defun boundaryp (elm)
  (if (string= (car elm) 'RT_BOUNDARY) t nil))

(defun get-boundarys (cell layer)
  (let ((lst1 (list)))
	(dolist (elm cell)
	  (if (listp elm)
		(if (boundaryp elm)
		  (if (= layer (cadr (cadr elm)))
			(push (cadr (cadddr elm)) lst1) nil)
		  nil) nil))
	(v1s-v2s lst1)))

(defun cell-xmin (cell)
  (xsmin (get-boundarys cell *anchor_layer_number*)))

(defun cell-ymin (cell)
  (ysmin (get-boundarys cell *anchor_layer_number*)))

(defun cell-xmax (cell)
  (xsmax (get-boundarys cell *anchor_layer_number*)))

(defun cell-ymax (cell)
  (ysmax (get-boundarys cell *anchor_layer_number*)))

(defun cell-width (cell)
  (let ((boundarys (get-boundarys cell *anchor_layer_number*)))
	(abs (- (xsmax boundarys) (xsmin boundarys)))))

(defun cell-high (cell)
  (let ((boundarys (get-boundarys cell *anchor_layer_number*)))
	(abs (- (ysmax boundarys) (ysmin boundarys)))))

(defun vsref-xmin (vsref)
  (xsmin (vsref-real-v2s vsref (get-boundarys (get-cell *srcunits* (aref vsref 0)) *anchor_layer_number*))))

(defun vsref-ymin (vsref)
  (ysmin (vsref-real-v2s vsref (get-boundarys (get-cell *srcunits* (aref vsref 0)) *anchor_layer_number*))))

(defun vsref-xmax (vsref)
  (xsmax (vsref-real-v2s vsref (get-boundarys (get-cell *srcunits* (aref vsref 0)) *anchor_layer_number*))))

(defun vsref-ymax (vsref)
  (ysmax (vsref-real-v2s vsref (get-boundarys (get-cell *srcunits* (aref vsref 0)) *anchor_layer_number*))))

(defun vsrefs-xmin (vsrefs)
  (let ((mins (list)))
	(dolist (vsref vsrefs)
	  (push (vsref-xmin vsref) mins))
	(min-of mins)))

(defun vsrefs-ymin (vsrefs)
  (let ((mins (list)))
	(dolist (vsref vsrefs)
	  (push (vsref-ymin vsref) mins))
	(min-of mins)))

(defun vsrefs-xmax (vsrefs)
  (let ((maxs (list)))
	(dolist (vsref vsrefs)
	  (push (vsref-xmax vsref) maxs))
	(max-of maxs)))

(defun vsrefs-ymax (vsrefs)
  (let ((maxs (list)))
	(dolist (vsref vsrefs)
	  (push (vsref-ymax vsref) maxs))
	(max-of maxs)))

(defun textp (elm)
  (if (string= (car elm) 'RT_TEXT) t nil))

(defun get-texts (cell layer stri)
  (let ((lst1 (list)))
	(dolist (elm cell)
	  (if (listp elm)
		(if (textp elm)
		  (if (and
				(= layer (cadr (by-car-stri elm 'RT_LAYER)))
				(string= stri (cadr (by-car-stri elm 'RT_STRING))))
			(push elm lst1) nil)
		  nil) nil))
	lst1))

(defun get-pinnames (cell)
  (let ((lst1 (list)))
	(dolist (elm cell)
	  (if (listp elm)
		(if (textp elm)
		  (push (cadr (by-car-stri elm 'RT_STRING)) lst1) nil) nil))
	(clearify lst1)))

(defun get-pins (cell stri)
  (let ((lst1 (list)))
	(dolist (elm cell)
	  (if (listp elm)
		(if (textp elm)
		  (if (string= stri (cadr (by-car-stri elm 'RT_STRING)))
			(push elm lst1) nil)
		  nil) nil))
	lst1))

(defun get-texts-xy (cell layer stri)
  (let ((xys (list)) (texts (get-texts cell layer stri)))
	(dolist (text texts)
	  (push (cadr (by-car-stri text 'RT_XY)) xys))
	xys))

(defun get-pins-xy (cell stri)
  (let ((xys (list)) (texts (get-pins cell stri)))
	(dolist (text texts)
	  (push (cadr (by-car-stri text 'RT_XY)) xys))
	xys))

(defun get-pins-layer (cell stri)
  (let ((xys (list)) (texts (get-pins cell stri)))
	(dolist (text texts)
	  (push (cadr (by-car-stri text 'RT_LAYER)) xys))
	xys))

(defun get-pinlists (cell stris)
  (let ((pinlists (list)))
	(dolist (stri stris)
	  (push (get-pins cell stri) pinlists))
	pinlists))

(defun get-pinlists-xys (cell stris)
  (let ((pinlists (list)))
	(dolist (stri stris)
	  (push (get-pins-xy cell stri) pinlists))
	pinlists))

(defun get-pinlists-layers (cell stris)
  (let ((pinlists (list)))
	(dolist (stri stris)
	  (push (get-pins-layer cell stri) pinlists))
	pinlists))

(defun rt-srefs (vsrefs)
  (let ((srefs (list)))
	(dolist (vsref vsrefs)
	  (push (rt-sref vsref) srefs))
	srefs))

(defun rt-texts (vtexts)
  (let ((texts (list)))
	(dolist (vtext vtexts)
	  (push (rt-text vtext) texts))
	texts))

(defun cell-1 (name vsrefs vtexts)
  (rt-cell
	name
	(concatenate 'list
	(list
	  (rt-box *anchor_layer_number*
			  (vsrefs-xmin vsrefs)
			  (vsrefs-ymin vsrefs)
			  (vsrefs-xmax vsrefs)
			  (vsrefs-ymax vsrefs)))
	(rt-srefs vsrefs)
	(rt-texts vtexts))))

(defun combine-list-layers-list-xys (list-layers list-xys)
  (let ((vector-layers (make-array (list (length list-layers)) :initial-contents list-layers))
		(vector-xys (make-array (list (length list-xys)) :initial-contents list-xys))
		(lst (list)))
	(if (= (length vector-layers) (length vector-xys))
	  (dotimes (n (length vector-xys))
		(push (concatenate 'list (list (aref vector-layers n)) (list (aref vector-xys n))) lst))
	  nil)
	lst))

(defun get-vsref-vtexts (vsref pinnames mapnames)
  (let ((combine-list (combine-list-layers-list-xys
						(get-pinlists-layers (get-cell *srcunits* (aref vsref 0)) pinnames)
						(get-pinlists-xys (get-cell *srcunits* (aref vsref 0)) pinnames)))
		(vtexts (list))
		(vector-mapnames (make-array (list (length mapnames)) :initial-contents mapnames))
		(n 0)
		(mapname)
		(vlayers)
		(vxys))
	(dolist (combine combine-list)
	  (setf mapname (aref vector-mapnames n))
	  (setf vlayers (make-array (list (length (car combine))) :initial-contents (car combine)))
	  (setf vxys (make-array (list (length (cadr combine))) :initial-contents (cadr combine)))
	  (dotimes (n (length vxys))
		(push (vector (aref vlayers n) (vsref-real-xy vsref (aref vxys n)) mapname) vtexts))
	  (incf n))
	vtexts))

(defun get-rt-paths (cell layer)
  (let ((paths (list)))
	(dolist (elm cell)
	  (if (listp elm)
		(if (string= (car elm) 'RT_PATH) 
		  (if (= (cadr (by-car-stri elm 'RT_LAYER)) layer)
			(push elm paths) nil) nil) nil))
	paths))

(defun get-paths (cell layer)
  (let ((rt-paths (get-rt-paths cell layer))
		(paths (list)))
	(dolist (rt-path rt-paths)
	  (push (vector (cadr (by-car-stri rt-path 'RT_WIDTH))
					(v1-v2 (cadr (by-car-stri rt-path 'RT_XY)))) paths))
	paths))

(defun get-metals (cell layer)
  (concatenate 'list (get-boundarys cell layer)
			   (get-paths cell layer)))

;;	instance:	(vector vsref (sub-pinlist "XPB0" "XPB1" ... ) (top-pinlist "shit0" "shit1" ... ))  !! NOTICE !!

(defun cell-2 (name instances)
  (let ((vsrefs (list))
		(vtexts (list)))
	(dolist (instance instances)
	  (push (aref instance 0) vsrefs)
	  (setf vtexts (concatenate 'list vtexts
								(get-vsref-vtexts (aref instance 0)
												  (aref instance 1)
												  (aref instance 2)))))
	(cell-1 name vsrefs vtexts)))

(defun relocate-1 (instance1s)
  (let ((instances (list))
		(vsrefs (list))
		(xymin)
		(n 0)
		(vvsrefs (list)))
	(dolist (instance1 instance1s)
	  (push (aref instance1 0) vsrefs))
	(setf vsrefs (reverse vsrefs))
	(setf xymin (vector (vsrefs-xmin vsrefs) (vsrefs-ymin vsrefs)))
	(dolist (vsref vsrefs)
	  (setf (aref vsref 3) (vector- (aref vsref 3) xymin))
	  (push vsref vvsrefs))
	(setf vvsrefs (make-array (list (length vvsrefs)) :initial-contents (reverse vvsrefs)))
	(dolist (instance1 instance1s)
	  (setf (aref instance1 0) (aref vvsrefs n))
	  (push instance1 instances)
	  (incf n))
	(reverse instances)))

(defun cell-3 (name instance1s)
  (cell-2 name (relocate-1 (relocate-2 instance1s))))

(defun relocate-2 (instance1s)
  (let ((instances (list))
		(vsrefs (list))
		(xymin)
		(n 0)
		(vvsrefs (list))
		(vsref.h)
		(vsref.w))
	(dolist (instance1 instance1s)
	  (push (aref instance1 0) vsrefs))
	(setf vsrefs (reverse vsrefs))
	(setf xymin (vector (vsrefs-xmin vsrefs) (vsrefs-ymin vsrefs)))
	(dolist (vsref vsrefs)
	  (setf vsref.h (cell-high (get-cell *srcunits* (aref vsref 0))))
	  (setf vsref.w (cell-width (get-cell *srcunits* (aref vsref 0))))
	  (case (aref vsref 1)
		(#x0000 (case (aref vsref 2)
				  (0.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector 0 0))))
				  (90.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector vsref.h 0))))
				  (180.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector vsref.w vsref.h))))
				  (270.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector 0 vsref.w))))
				  ))
		(#x4000 (case (aref vsref 2)
				  (0.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector vsref.w 0))))
				  (90.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector vsref.h vsref.w))))
				  (180.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector 0 vsref.h))))
				  (270.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector 0 0))))
				  ))
		(#x8000 (case (aref vsref 2)
				  (180.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector vsref.w 0))))
				  (270.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector vsref.h vsref.w))))
				  (0.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector 0 vsref.h))))
				  (90.0d0 (setf (aref vsref 3) (vector+ (aref vsref 3) (vector 0 0))))
				  ))
		)
	  (push vsref vvsrefs))
	(setf vvsrefs (make-array (list (length vvsrefs)) :initial-contents (reverse vvsrefs)))
	(dolist (instance1 instance1s)
	  (setf (aref instance1 0) (aref vvsrefs n))
	  (push instance1 instances)
	  (incf n))
	(reverse instances)))

(defun sname-width (sname)
  (cell-width (get-cell *srcunits* sname)))

(defun sname-high (sname)
  (cell-high (get-cell *srcunits* sname)))

(defun inst (vinst)
  (let ((sname (aref vinst 0))
		(strans (aref vinst 1))
		(angle (aref vinst 2))
		(xy (aref vinst 3))
		(inst-pin-list (aref vinst 4))
		(top-pin-list (aref vinst 5)))
	(vector
	  (vector sname strans angle xy)
	  inst-pin-list
	  top-pin-list)))

(defun vinst-width (vinst)
  (let ((cw (sname-width (aref vinst 0)))
		(ch (sname-high (aref vinst 0))))
	(case (aref vinst 2)
	  (0.0d0 cw)
	  (180.0d0 cw)
	  (90.0d0 ch)
	  (270.0d0 ch))))

(defun vinst-high (vinst)
  (let ((cw (sname-width (aref vinst 0)))
		(ch (sname-high (aref vinst 0))))
	(case (aref vinst 2)
	  (0.0d0 ch)
	  (180.0d0 ch)
	  (90.0d0 cw)
	  (270.0d0 cw))))

(defun inst-up-left (inst1 vinst)
  (let ((inst1.h (vinst-high (aref inst1 0)))
		(p0 (aref (aref inst1 0) 3)))
	(setf (aref vinst 3) (vector+ p0 (vector 0 inst1.h)))
	(inst vinst)))

(defun inst-up-right (inst1 vinst)
  (let ((inst2.w (vinst-width vinst))
		(inst1.w (vinst-width (aref inst1 0)))
		(inst1.h (vinst-high (aref inst1 0)))
		(p0 (aref (aref inst1 0) 3)))
	(setf (aref vinst 3) (vector+ p0 (vector (- inst1.w inst2.w) inst1.h)))
	(inst vinst)))

(defun inst-right-down (inst1 vinst)
  (let ((inst1.w (vinst-width (aref inst1 0)))
		(p0 (aref (aref inst1 0) 3)))
	(setf (aref vinst 3) (vector+ p0 (vector inst1.w 0)))
	(inst vinst)))

(defun inst-right-up (inst1 vinst)
  (let ((inst2.h (vinst-high vinst))
		(inst1.w (vinst-width (aref inst1 0)))
		(inst1.h (vinst-high (aref inst1 0)))
		(p0 (aref (aref inst1 0) 3)))
	(setf (aref vinst 3) (vector+ p0 (vector inst1.w (- inst1.h inst2.h))))
	(inst vinst)))

(defun inst-left-down (inst1 vinst)
  (let ((inst2.w (vinst-width vinst))
		(p0 (aref (aref inst1 0) 3)))
	(setf (aref vinst 3) (vector+ p0 (vector (* -1 inst2.w) 0)))
	(inst vinst)))

(defun inst-left-up (inst1 vinst)
  (let ((inst2.w (vinst-width vinst))
		(inst2.h (vinst-high vinst))
		(inst1.h (vinst-high (aref inst1 0)))
		(p0 (aref (aref inst1 0) 3)))
	(setf (aref vinst 3) (vector+ p0 (vector (* -1 inst2.w) (- inst1.h inst2.h))))
	(inst vinst)))

(defun inst-down-left (inst1 vinst)
  (let ((inst2.h (vinst-high vinst))
		(p0 (aref (aref inst1 0) 3)))
	(setf (aref vinst 3) (vector+ p0 (vector 0 (* -1 inst2.h))))
	(inst vinst)))

(defun inst-down-right (inst1 vinst)
  (let ((inst2.w (vinst-width vinst))
		(inst2.h (vinst-high vinst))
		(inst1.w (vinst-width (aref inst1 0)))
		(p0 (aref (aref inst1 0) 3)))
	(setf (aref vinst 3) (vector+ p0 (vector (- inst1.w inst2.w) (* -1 inst2.h))))
	(inst vinst)))

(defun p-on-stright (p p0 k)
  (let ((dp (vector- p p0)))
	(if (eql (div (aref dp 1) (aref dp 0)) nil)
	  (p-on-stright p (vector (+ (aref p0 0) 1) (+ (aref p0 1) k)) k)
	  (if (eql (div (aref dp 1) (aref dp 0)) k)
		t nil))))

(defun p-on-line (p p1 p2)
  (let ((k)
		(dp (vector- p2 p1)))
	(setf k (div (aref dp 1) (aref dp 0)))
	(if (p-on-stright p p1 k)
	  (if (and (and (>= (aref p 0) (min (aref p1 0) (aref p2 0))) (<= (aref p 0) (max (aref p1 0) (aref p2 0))))
			   (and (>= (aref p 1) (min (aref p1 1) (aref p2 1))) (<= (aref p 1) (max (aref p1 1) (aref p2 1)))))
		t nil)
	  nil)))

(defun overlay (vsref stri)
  (let ((text-xy (get-pins-xy (get-cell *srcunits* (aref vsref 0)) stri)))
	(if (= 1 (length text-xy))
	  (setf text-xy (car text-xy)) nil)
	(case (aref vsref 2)
	  (0.0d0 (setf (aref vsref 3) (vector- (aref vsref 3) text-xy)))
	  (90.0d0 (setf (aref vsref 3) (vector- (aref vsref 3) (reverse (vector-dot text-xy (vector 1 -1))))))
	  (270.0d0 (setf (aref vsref 3) (vector- (aref vsref 3) (reverse (vector-dot text-xy (vector 1 -1))))))
	  (180.0d0 (setf (aref vsref 3) (vector- (aref vsref 3) (vector-dot text-xy (vector -1 -1))))))
	(rt-sref vsref)))

(defun overlay-1 (sname strans angle cell stri stri0)
  (overlay
	(vector sname strans angle
			(car (get-pins-xy cell stri0)))
	stri))

(defun clearify-1 (lst a0)
  (let ((r nil))
	(dolist (a lst)
	  (if (string= a0 a)
		(setf r t) nil))
	r))

(defun clearify (lst)
  (let ((lst0 (list)))
	(dolist (a lst)
	  (if (clearify-1 lst0 a) nil
		(push a lst0)))
	(reverse lst0)))

