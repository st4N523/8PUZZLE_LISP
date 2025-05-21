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
  depth)

(defun solve-puzzle-bfs (initial-state goal-state)
  "Solves puzzle using BFS and shows steps with execution time and nodes explored"
  (let ((queue (list (make-node :state initial-state
                               :parent nil
                               :move nil
                               :depth 0)))
        (visited (make-hash-table :test #'equal))
        (nodes-explored 0)
        (start-time (get-internal-real-time)))
    
    (format t "~%Initial State:")
    (print-state initial-state)
    
    (setf (gethash initial-state visited) t)
    
    (loop while queue do
          (let ((current (pop queue)))
            (incf nodes-explored)
            
            ;; Check if goal reached
            (when (equal (node-state current) goal-state)
              (let* ((end-time (get-internal-real-time))
                     (elapsed-time (/ (- end-time start-time) (coerce internal-time-units-per-second 'float))))
                (format t "~%Goal reached! Showing solution path:~%")
                (show-solution current)
                (format t "~%Execution Time: ~,6F seconds~%" elapsed-time)
                (format t "Total Nodes Explored: ~A~%" nodes-explored)
                (return)))
            
            ;; Generate and process all possible moves
            (dolist (move (get-possible-moves (node-state current)))
              (let ((new-state (move-blank (node-state current) move)))
                (unless (gethash new-state visited)
                  (setf (gethash new-state visited) t)
                  (setf queue 
                        (append queue 
                              (list (make-node :state new-state
                                             :parent current
                                             :move move
                                             :depth (1+ (node-depth current))))))))))))
    (format t "~%No solution found!~%"))

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
(let ((initial-state (make-state '(1 2 3 4 0 5 6 7 8)))
      (goal-state    (make-state '(1 2 3 4 5 6 7 8 0))))
  (solve-puzzle-bfs initial-state goal-state))