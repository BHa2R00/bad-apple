(defun parse-bmp (file)
  (with-open-file(si file
					 :direction	:input
					 :element-type	'(unsigned-byte 8))
	(let ((l1 '(header)))
	  (push (list 'bm (rd-u2-1 si)) l1)					;;1-2
	  (push (list 'file-size (rd-u4-1 si)) l1)			;;3-6
	  (push (list 'rsv1 (rd-u2-1 si)) l1)				;;7-8
	  (push (list 'rsv2 (rd-u2-1 si)) l1)				;;9-10
	  (push (list 'file-offset (rd-u4-1 si)) l1)		;;11-14
	  (push (list 'imf-len (rd-u4-1 si)) l1)			;;15-18
	  (push (list 'width (rd-u4-1 si)) l1)				;;19-22
	  (push (list 'height (rd-u4-1 si)) l1)				;;23-26
	  (push (list 'bmp-sel (rd-u2-1 si)) l1)			;;27-28
	  (push (list 'pix-cnt (rd-u2-1 si)) l1)			;;29-30
	  (push (list 'compress (rd-u4-1 si)) l1)			;;31-34
	  (push (list 'dat-len (rd-u4-1 si)) l1)			;;35-38
	  (push (list 'h-rsolv (rd-u4-1 si)) l1)			;;39-42
	  (push (list 'v-rsolv (rd-u4-1 si)) l1)			;;43-46
	  (push (list 'clr-set (rd-u4-1 si)) l1)			;;47-50
	  (push (list 'clr-sel (rd-u4-1 si)) l1)			;;51-54
	  (let*((width(cadr(assoc 'width (cdr l1))))
			(height(cadr(assoc 'height (cdr l1))))
			(pix-cnt(cadr(assoc 'pix-cnt (cdr l1))))
			(u2-cnt(/ pix-cnt 8)))
		(let((l2(list)))
		  (dotimes(n height)
			(let((l3(list)))
			  (dotimes(m width)
				(let((l4(list)))
				  (dotimes(m1 u2-cnt)
					(push(read-byte si nil 'eof)l4))
				  (push(reverse l4)l3)))
			  (push(reverse l3)l2)))
		  (let((v1(make-array(list height width):initial-contents (reverse l2))))
			(list
			  file
			  (reverse l1)
			  (list 'data v1))))))))

(defun export-bmp (l1)
  (let((file(car l1)))
	(with-open-file(so file
					   :direction			:output
					   :if-does-not-exist	:create
					   :if-exists			:supersede
					   :element-type		'(unsigned-byte 8))
	  (let*((header(cdr(assoc 'header (cdr l1))))
			(bm(cadr(assoc 'bm header)))
			(file-size(cadr(assoc 'file-size header)))
			(rsv1(cadr(assoc 'rsv1 header)))
			(rsv2(cadr(assoc 'rsv2 header)))
			(file-offset(cadr(assoc 'file-offset header)))
			(imf-len(cadr(assoc 'imf-len header)))
			(width(cadr(assoc 'width header)))
			(height(cadr(assoc 'height header)))
			(bmp-sel(cadr(assoc 'bmp-sel header)))
			(pix-cnt(cadr(assoc 'pix-cnt header)))
			(compress(cadr(assoc 'compress header)))
			(dat-len(cadr(assoc 'dat-len header)))
			(h-rsolv(cadr(assoc 'h-rsolv header)))
			(v-rsolv(cadr(assoc 'v-rsolv header)))
			(clr-set(cadr(assoc 'clr-set header)))
			(clr-sel(cadr(assoc 'clr-sel header)))
			(data(cadr(assoc 'data (cdr l1)))))
		(wt-u2-1 so bm)
		(wt-u4-1 so file-size)
		(wt-u2-1 so rsv1)
		(wt-u2-1 so rsv2)
		(wt-u4-1 so file-offset)
		(wt-u4-1 so imf-len)
		(wt-u4-1 so width)
		(wt-u4-1 so height)
		(wt-u2-1 so bmp-sel)
		(wt-u4-1 so pix-cnt)
		(wt-u2-1 so compress)
		(wt-u4-1 so dat-len)
		(wt-u4-1 so h-rsolv)
		(wt-u4-1 so v-rsolv)
		(wt-u4-1 so clr-set)
		(wt-u4-1 so clr-sel)
		(dotimes(n height)
		  (dotimes(m width)
			(dolist(bt (aref data n m))
			  (write-byte bt so))))))))

(defun bmp-cut-rect (name x0 y0 x1 y1 bmp)
  (let*((header(cdr(assoc 'header (cdr bmp))))
		(file-offset(cadr(assoc 'file-offset header)))
		(pix-cnt(cadr(assoc 'pix-cnt header)))
		(data(cadr(assoc 'data (cdr bmp))))
		(width(1+(abs(- x1 x0))))
		(height(1+(abs(- y1 y0))))
		(header1(list)))
	(dolist(elm header)
	  (push elm header1))
	(setf(cadr(assoc 'width header1))width)
	(setf(cadr(assoc 'height header1))height)
	(setf(cadr(assoc 'dat-len header1))(*(/ pix-cnt 8)(* width height)))
	(setf(cadr(assoc 'file-size header1))(+(*(/ pix-cnt 8)(* width height))file-offset))
	(let((l1(list)))
	  (do((y y0 (incf y)))
		((> y y1))
		(let(l2(list))
		  (do((x x0 (incf x)))
			((= x x1)(push(aref data y x)l2))
			(push(aref data y x)l2))
		  (push(reverse l2)l1)))
	  (let((v1(make-array(list height width):initial-contents (reverse l1))))
		(list
		  name
		  (concatenate 'list '(header) (reverse header1))
		  (concatenate 'list '(data) (list data)))))))

