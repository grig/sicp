;Exercise 2.76. As a large system with generic operations evolves, new types of data objects or new
;operations may be needed. For each of the three strategies -- generic operations with explicit dispatch, datadirected
;style, and message-passing-style -- describe the changes that must be made to a system in order to
;add new types or new operations. 

;; generic op/explicit dispatch
;; new operation: implement single function dispatching over data type.
;; new data type: modify each operation to accomodate for new type

;; data-directed
;; new op: implement a generic function, implement a datatype-specific function for each data type
;; new data type: implement a datatype-specific functions for each supported operation and register them

;; message-passing
;; new op: modify each data type adding an operation implementation
;; new data type: implement this data type and all supported operations.

;Which organization would be most appropriate for a system in which
;new types must often be added? 

;; message-passing would probably shine here because all we need is a single datatype definition.

;Which would be most appropriate for a system in which new operations
;must often be added?

;; explicit dispatch or data-directed dispatch: we don't really want to go through all data type definitions and add a new clause for handling a new operation.
