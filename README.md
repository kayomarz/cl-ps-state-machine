*This is work in progress*

A Deterministic Finite State Machine for Common Lisp and
[Parenscript](https://parenscript.common-lisp.dev/).

*Parenscript brings Lisp macros to front-end web development.*

## API

+ Macro `defsm`
+ Function `apply-input`

## An Example

In a **todo list**, let us assume each task can have the following states:

1. to-be-done
2. in-progress
3. in-review
3. complete

A state machine accepts an **input** in order to change its state.

We can apply the following inputs only if that input is valid for the current
state of the task.

1. begin-work
2. abort-work
3. request-review
4. accept
5. reject

This library provides `defsm` and `defstates` to define the above FSM.

```common-lisp

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
```

This library provides `apply-input` to change the input of an fsm.

```common-lisp

(apply-input *fsm* :begin-work)
(apply-input *fsm* :request-review)
(apply-input *fsm* :accept)

```

If an input cannot be applied in a particular state, an error condition is
signalled.


## Roadmap

1. Remove the need for `defstates` because `defsm` should suffice.
2. `defsm` should accept :on-exit forms.
3. `defsm` should maybe accept :on-transition forms.
4. Evaluate use cases and usefulness of nested states if they are useful.
