(require 'cl-openstack-client-test)
(let ((results (5am:run 5am::*suite*)))
  (5am:explain! results)
  (exit :code (if (eq (5am:results-status results ) t) 0 1)))
