(defun generate (lis)
	(list (list (+ (first lis) 1)(first (rest lis)))
            (list (- (first lis) 1) (first (rest lis)))
	        (list (first lis) (+ (first (rest lis)) 1))
            (list (first lis) (- (first (rest lis)) 1))))

(defun allow-extension (old-lis)			
 (let ((lis (first old-lis))) 
   (cond ((null lis) nil)
                 ((or (< (first lis) 0) (< (first (rest lis)) 0)
                    (> (first lis) 7) (> (first (rest lis)) 7))  
                           (allow-extension (rest old-lis)))
                  (t (cons lis (allow-extension (rest old-lis)))))))

(defun match (element pattern)
			(cond ((NULL pattern) NIL)
			((and (eq (first element) (first (first pattern))) (eq (second element) (second (first pattern))))  T)
			(t (match element (rest pattern)))))
			
(setq goal '((6 0) (2 3) (5 4)))

(setq holes '((1 1) (5 6)))

(setq visited '( ))

(setq possible-moves '( ))

(defun push_visited (pos)
		(push pos visited))

(defun searchB (player-moves)
		(cond ((eq (match (first visited) goal) t) (print visited))
		((and (eq (match (first player-moves) visited) NIL) (eq (match (first player-moves) holes) NIL)) (searchBreadth (first player-moves)))
		((and (eq (match (first (rest playe   player-moves))) visited) NIL) (eq (match (first (rest (rest player-moves))) holes) NIL)) (searchBreadth (first (rest (rest player-moves)))))))
		
		
			
(defun searchBreadth (player)
			(setq possible-moves (allow-extension (generate player)))
			(push_visited player)
			(searchB possible-moves))
			
;;; Best first search

(setq players-heuristic '( ))

(setq first-heuristic '( ))

(setq second-heuristic '( ))

(setq third-heuristic '( ))

(setq fourth-heuristic '( ))

(setq numofHeuristic '( ))
			
(setq heuristic '((0 0 5) (1 0 4) (2 0 3) (3 0 3) (4 0 2) (5 0 1) (6 0 0) (7 0 1) 
				  (0 1 4) (1 1 9) (2 1 2) (3 1 3) (4 1 4) (5 1 2) (6 1 1) (7 1 2)
				  (0 2 3) (1 2 2) (2 2 1) (3 2 2) (4 2 3) (5 2 9) (6 2 3) (7 2 3)
				  (0 3 2) (1 3 1) (2 3 0) (3 3 1) (4 3 2) (5 3 1) (6 3 2) (7 3 3)
				  (0 4 3) (1 4 2) (2 4 1) (3 4 2) (4 4 1) (5 4 0) (6 4 1) (7 4 2)
				  (0 5 4) (1 5 3) (2 5 2) (3 5 3) (4 5 2) (5 5 1) (6 5 2) (7 5 3)
				  (0 6 5) (1 6 4) (2 6 3) (3 6 4) (4 6 3) (5 6 9) (6 6 3) (7 6 4)
				  (0 7 6) (1 7 5) (2 7 4) (3 7 5) (4 7 4) (5 7 5) (6 7 4) (7 7 5))
				  
(defun match-first (element pattern)
			(cond ((NULL pattern) NIL)
			((and (eq (first element) (first (first pattern))) (eq (second element) (second (first pattern))))  (setq first-heuristic (third (first pattern))))
			(t (match element (rest pattern)))))
			
(defun match-second (element pattern)
			(cond ((NULL pattern) NIL)
				((and (eq (first element) (first (first pattern))) (eq (second element) (second (first pattern))))  (setq second-heuristic (third (first pattern))))
				(t (match element (rest pattern)))))
			
(defun match-third (element pattern)
			(cond ((NULL pattern) NIL)
			((and (eq (first element) (first (first pattern))) (eq (second element) (second (first pattern))))  (setq third-heuristic (third (first pattern))))
			(t (match element (rest pattern)))))
			
(defun match-fourth (element pattern)
			(cond ((NULL pattern) NIL)
			((and (eq (first element) (first (first pattern))) (eq (second element) (second (first pattern))))  (setq fourth-heuristic (third (first pattern))))
			(t (match element (rest pattern)))))
			
(defun determin-win (position)
	(cond ((eq (match position goal) t) (and (push_visited position) (print visited)))
			(t (searchBestFirst position))))
					
(defun searchBestFirst (player)
		(push_visited player)
		(setq possible-moves (allow-extension (generate player)))
		(if (eq (first possible-moves) (not nil)) (and (match-first (first possible-moves) heuristic) (setq numofHeuristic '1))) 
		(if (eq (second possible-moves) (not nil)) (and (match-second (second possible-moves) heuristic) (setq numofHeuristic '2))) 
		(if (eq (third possible-moves) (not nil)) (and (match-third (third possible-moves) heuristic) (setq numofHeuristic '3))) 
		(if (eq (fourth possible-moves) (not nil)) (and (match-fourth (fourth possible-moves) heuristic) (setq numofHeuristic '4)))
		(if (eq numofHeuristic '2)
		(cond ((< first-heuristic second-heuristic) (determin-win (first possible-moves)))
			  (< second-heuristic first-heuristic) (determin-win (second possible-moves))))
		(if (eq numofHeuristic '3)
		(cond ((and (< first-heuristic second-heuristic) (< first-heuristic third-heuristic)) (determin-win (first possible-moves)))
			  ((and (< second-heuristic first-heuristic) (< second-heuristic third-heuristic)) (determin-win (second possible-moves)))
			  ((and (< third-heuristic first-heuristic) (< third-heuristic second-heuristic)) (determin-win (third possible-moves)))))
		(if (eq numofHeuristic '4)
		(cond ((and (< first-heuristic second-heuristic) (< first-heuristic third-heuristic) (< first-heuristic fourth-heuristic)) (determin-win (first possible-moves)))
			  ((and (< second-heuristic first-heuristic) (< second-heuristic third-heuristic) (< second-heuristic fourth-heuristic)) (determin-win (second possible-moves)))
			  ((and (< third-heuristic first-heuristic) (< third-heuristic second-heuristic) (< third-heuristic fourth-heuristic)) (determin-win (third possible-moves)))
			  ((and (< fourth-heuristic first-heuristic) (< fourth-heuristic second-heuristic) (< fourth-heuristic third-heuristic)) (determin-win (third possible-moves)))))
