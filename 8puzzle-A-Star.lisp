(defun print-state (state)
  "Pretty prints the puzzle state"
  (format t "~%-------------~%")
  (dolist (row state)
    (format t "|")
    (dolist (cell row)
      (if (zerop cell)
          (format t "   |")
          (format t " ~A |" cell)))
    (format t "~%-------------~%")))

(defun make-state (numbers)
  "Creates a 2D list state from a flat list of numbers"
  (list 
    (subseq numbers 0 3)
    (subseq numbers 3 6)
    (subseq numbers 6 9)))

(defun manhattan-distance (state goal-state)
  "Calculates total Manhattan distance of all tiles from their goal positions"
  (let ((distance 0))
    (dotimes (i 3)
      (dotimes (j 3)
        (let ((value (nth j (nth i state))))
          (unless (zerop value)
            (let* ((pos (position value (apply #'append goal-state)))
                   (goal-i (floor pos 3))
                   (goal-j (mod pos 3)))
              (incf distance (+ (abs (- i goal-i))
                                (abs (- j goal-j)))))))))
    distance))

(defun find-blank (state)
  "Returns (row col) of blank (0) tile"
  (dotimes (i 3)
    (dotimes (j 3)
      (when (zerop (nth j (nth i state)))
        (return-from find-blank (list i j))))))

(defun move-blank (state direction)
  "Returns new state after moving blank tile in specified direction"
  (let* ((new-state (mapcar #'copy-list state))
         (blank-pos (find-blank state))
         (i (first blank-pos))
         (j (second blank-pos))
         (new-i i)
         (new-j j))
    (case direction
      (:up    (decf new-i))
      (:down  (incf new-i))
      (:left  (decf new-j))
      (:right (incf new-j)))
    (when (and (>= new-i 0) (< new-i 3)
               (>= new-j 0) (< new-j 3))
      (rotatef (nth j (nth i new-state))
               (nth new-j (nth new-i new-state)))
      new-state)))

(defun get-possible-moves (state)
  "Returns list of possible moves from current state"
  (let ((blank-pos (find-blank state))
        (moves nil))
    (when (> (first blank-pos) 0)          (push :up moves))
    (when (< (first blank-pos) 2)          (push :down moves))
    (when (> (second blank-pos) 0)         (push :left moves))
    (when (< (second blank-pos) 2)         (push :right moves))
    moves))

(defstruct node
  state
  parent
  move
  g      ; Cost from start
  h      ; Heuristic estimate to goal
  f)     ; Total estimated cost (g + h)

(defun solve-puzzle (initial-state goal-state)
  "Solves puzzle using A* search and shows steps with real execution time and nodes explored"
  (let ((open-list (list (make-node :state initial-state
                                   :parent nil
                                   :move nil
                                   :g 0
                                   :h (manhattan-distance initial-state goal-state)
                                   :f (manhattan-distance initial-state goal-state))))
        (closed-list nil)
        (nodes-explored 0)
        (start-time (get-internal-real-time)))
    
    (format t "~%Initial State:")
    (print-state initial-state)
    
    (loop while open-list do
          (let ((current (pop open-list)))
            (incf nodes-explored)
            ;; Check if goal reached
            (when (equal (node-state current) goal-state)
              (let* ((end-time (get-internal-real-time))
                     (elapsed-time (/ (- end-time start-time) (coerce internal-time-units-per-second 'float))))
                (format t "~%Goal reached! Showing solution path:~%")
                (show-solution current)
                (format t "~%Execution Time: ~,6F seconds~%" elapsed-time)
                (format t "Nodes Explored: ~A~%" nodes-explored)
                (return)))
            
            ;; Add current to closed list
            (push current closed-list)
            
            ;; Generate successors
            (dolist (move (get-possible-moves (node-state current)))
              (let* ((new-state (move-blank (node-state current) move))
                     (g (1+ (node-g current)))
                     (h (manhattan-distance new-state goal-state))
                     (f (+ g h))
                     (new-node (make-node :state new-state
                                        :parent current
                                        :move move
                                        :g g
                                        :h h
                                        :f f)))
                
                ;; Skip if state already in closed list
                (unless (find new-state closed-list 
                            :test #'equal :key #'node-state)
                  ;; Insert into open list, maintaining order by f value
                  (setf open-list (merge 'list 
                                       (list new-node)
                                       open-list
                                       #'<
                                       :key #'node-f)))))))
    (format t "~%No solution found!~%")))

(defun show-solution (goal-node)
  "Shows solution path from initial state to goal"
  (let ((path nil)
        (current goal-node))
    ;; Build path from goal to start
    (loop while current do
          (push current path)
          (setf current (node-parent current)))
    
    ;; Show each step
    (format t "~%Solution found in ~A moves:~%" (1- (length path)))
    (dolist (node path)
      (when (node-move node)
        (format t "~%Move: ~A" (node-move node)))
      (print-state (node-state node)))))

;; Example usage
(let ((initial-state (make-state '(1 2 0 3 4 5 6 7 8)))
      (goal-state    (make-state '(1 2 3 4 5 6 7 8 0))))
  (solve-puzzle initial-state goal-state))