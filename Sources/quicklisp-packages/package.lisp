(defpackage om-ckn
  (:use #:cl)
  (:export #:iterate #:iter #:display-iterate-clauses
	   #:defsynonym #:dsetq #:declare-variables
	   #:defmacro-clause #:defmacro-driver #:defclause-sequence
	   #:initially #:after-each #:finally #:finally-protected
	   #:else #:if-first-time #:first-iteration-p #:first-time-p
	   #:finish #:leave #:next-iteration #:next #:terminate
	   #:repeat #:for #:as #:generate #:generating #:in
	   #:sum #:summing #:multiply #:multiplying
	   #:maximize #:minimize #:maximizing #:minimizing #:counting
	   #:always #:never #:thereis #:finding #:collect #:collecting
	   #:with #:while #:until #:adjoining #:nconcing #:appending
	   #:nunioning #:unioning #:reducing #:accumulate #:accumulating))

(in-package om-ckn)

