;; --- macros to define fsm spec. ----------------------------------------------

(ps:defmacro+ps error-with-msgs (&rest msgs)
  "Concatenate messages and call `error`."
  `(error (cat ,@msgs)))

(ps:defmacro+ps defstate-single (sexp)
  "Define a FSM state."
  (let ((name (car sexp))
        (hash (make-hash-table))
        (type 'nil))
    (labels ((push-type (x)
               (if (gethash type hash)
                   (nconc (gethash type hash) `(,x))
                   (push x (gethash type hash)))))
      (dolist (x (cdr sexp))
        (if (keywordp x)
            (setf type x)
            (push-type x)))
      `(list (list :name ',name)
             (list :entry-fn (lambda () (progn ,@(gethash :on-entry hash))))
             (list :transits '(,@(gethash :transit-to hash)))))))

(ps:defmacro+ps defstates (&rest sexp)
  "Define FSM states."
  `(list ,@(mapcar
            (lambda (x) `(defstate-single ,x))
            sexp)))

(ps:defmacro+ps fsm-get-state (index fsm-spec)
  "From the FSM spec, parse the name of the state at INDEX."
  `(progn
     (when (eql 0 (length ,fsm-spec))
       (error "No states in fsm-spec."))

     (when (>= ,index (length ,fsm-spec))
       (error (format nil "Not enough states in fsm-spec for index ~D."
                      ,index)))

     (let* ((state (elt ,fsm-spec ,index)))
       (unless (assoc-p :name state)
         (error (format nil "No name for state ~D." ,index)))
       state)))

(ps:defmacro+ps defsm (fsm-spec)
  "Define a FSM (Finite State Machine). As soon as the fsm gets defined, the
state becomes the fsm's current state and its entry function is executed."
  (alexandria:with-gensyms (cur-g state-g)
    `(let* ((,state-g (fsm-get-state 0 ,fsm-spec))
            (,cur-g (elt (assoc-p :name ,state-g) 1)))
       (prog1
           (list
            (list :fsm-spec
                  (lambda () ,fsm-spec))
            (list :current-name
                  (lambda () ,cur-g))
            (list :set-state
                  (lambda (name) (setf ,cur-g name))))
         (funcall (elt (assoc-p :entry-fn ,state-g) 1))))))

;;; FSM functionality
;;; =================

;; --- assoc-p to work with a variation of association lists -------------------

(defun *assoc-p (item xs index-to-compare)
  (dolist (el xs)
    (when (eql item (elt el index-to-compare))
      (return-from *assoc-p el))))

(defun assoc-p (item xs)
  "assoc-p is like ASSOC for Parenscript. It works with a variation of alists.

In an alist, each element is a cons cell. For example:
    ((a . 1) (b . 2))

Since Parenscript does not support a `cons` cell, we resort to using a list
of two items instead of a cons cell. For example:
    ((a 1) (b 2))

Suffix `-P` is used (arbitrarily) to differentiate it from ASSOC."
  (*assoc-p item xs 0))

(defun rassoc-p (item xs)
  "See ASSOC-P."
  (*assoc-p item xs 1))

(defun get-item (item-key alist-l)
  "Get item from an alist-l. See assoc-p"
  (awhen (assoc-p item-key alist-l)
    (elt anaphora:it 1)))


;; --- fsm functions -----------------------------------------------------------

(defun fsm-funcall (fsm fname &rest rest)
  "For the given fsm structure, call an fsm function:

(fsm-funcall *fsm* :fsm-spec)

The `fsm` data structure contains items such as :current-name or :fsm-spec some
of which are function definitions. See the defsm macro."
  (let ((fn (get-item fname fsm)))
    (if (functionp fn)
        (apply fn rest)
        (error-with-msgs "No fsm function: " fname))))

(defun get-fsm-spec (fsm)
  (fsm-funcall fsm :fsm-spec))

(defun get-state (fsm name)
  (dolist (state-spec (get-fsm-spec fsm))
    (when (eql name (get-item :name state-spec))
      (return-from get-state state-spec))))

(defun get-state-by-index (fsm i)
  (let ((fsm-spec (get-fsm-spec fsm)))
    (when (< i (length fsm-spec))
      (elt fsm-spec i))))

(defun get-current-state-name (fsm)
  (fsm-funcall fsm :current-name))

(defun get-current-state (fsm)
  (get-state fsm (get-current-state-name fsm)))

(defun peek-next-state (fsm input)
  "Peek at the next state if INPUT were applied. Apart from returning the
next state name, also return the next state's entry function which runs all
:ON-ENTRY forms defined for FSM."
  (let* ((state (get-current-state fsm))
         (next-state-name (awhen (get-item :transits state)
                            (get-item input it)))
         (next-state (when next-state-name (get-state fsm next-state-name))))
    (macrolet ((err (&rest msgs) ; helper to reduce repetition
                 `(error-with-msgs "Attempted to apply input `" input
                                   "` in state `" (get-current-state-name fsm)
                                   "`" ,@msgs)))
      (unless next-state-name
        (err " but no such input defined."))
      (if next-state
          (values next-state-name (get-item :entry-fn next-state))
          (err " but next state `" next-state-name "` is undefined.")))))

(defun apply-input (fsm input)
  "Apply input to a FSM. Applying an INPUT which the current state does not
support results in an error."
  (multiple-value-bind (name entry-fn)
      (peek-next-state fsm input)
    (if name
        (progn
          (when (functionp entry-fn)
            (funcall entry-fn))
          (fsm-funcall fsm :set-state name))
        (error-with-msgs
         "apply-input: State " (get-current-state-name fsm)
         " does not accept input " input))))


;; --- utils (platform specific for Common Lisp) -------------------------------

(defun cat (&rest rest)
  (apply #'concatenate
         'cl:string
         (mapcar (lambda (x)
                   (cond ((stringp x)
                          x)
                         ((or (numberp x)
                              (symbolp x)
                              (null x)
                              (eq (type-of x) 'boolean))
                          (write-to-string x))
                         (t (error "cannot cat argument type"))))
                 rest)))


;; --- example fsm -------------------------------------------------------------

#+nil
(defparameter *fsm*
  (defsm
      (defstates
          (to-be-done
           :transit-to (:begin-work in-progress))

          (in-progress
           :on-entry (notify-team) :transit-to (:request-review in-review)
           (:abort-work to-be-done))

        (in-review
         :transit-to (:accept complete) (:reject in-progress))

        (complete
         :on-entry (notify-others-about-task-complete)
         (move-item-from-todo-list-to-archive)))))

