.. _tests:

=======
 Tests
=======

Tests are cool now. To run them just use:

   .. code-block:: bash

       make deps
       make testgui

Tests also run automatically on `Travis-CI
<https://travis-ci.org/pashinin/workgroups2/builds>`_ using the GUI
version of Emacs. So you can tests any frames as on your
desktop.

Tests themselves are in ``tests/workgroups2-tests.el``


Serialization tests
===================

If you see an error like this:

   .. code-block:: cl

        wg-add-buffer-to-buf-list(nil)
        wg-pickel-marker-serializer(#<marker in no buffer>)
        #[(obj id) "..." [id obj result wg-pickel-object-serializer] 3](#<marker in no buffer> 18)
        maphash(#[(obj id) "..." [id obj result wg-pickel-object-serializer] 3] #s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8 data (((#<buffer todo-orgx.org> #<marker at 1 in todo-orgx.org> #<marker at 158366 in todo-orgx.org>) (#<buffer refile-orgx.org> #<marker at 39 in refile-orgx.org> #<marker at 39 in refile-orgx.org>) (nil #<marker in no buffer> #<marker in no buffer>)) 0 (#<buffer todo-orgx.org> #<marker at 1 in todo-orgx.org> #<marker at 158366 in todo-orgx.org>) 1 #<buffer todo-orgx.org> 2 (#<marker at 1 in todo-orgx.org> #<marker at 158366 in todo-orgx.org>) 3 #<marker at 1 in todo-orgx.org> 4 (#<marker at 158366 in todo-orgx.org>) 5 #<marker at 158366 in todo-orgx.org> 6 nil 7 ((#<buffer refile-orgx.org> #<marker at 39 in refile-orgx.org> #<marker at 39 in refile-orgx.org>) (nil #<marker in no buffer> #<marker in no buffer>)) 8 (#<buffer refile-orgx.org> #<marker at 39 in refile-orgx.org> #<marker at 39 in refile-orgx.org>) 9 #<buffer refile-orgx.org> 10 (#<marker at 39 in refile-orgx.org> #<marker at 39 in refile-orgx.org>) 11 #<marker at 39 in refile-orgx.org> 12 (#<marker at 39 in refile-orgx.org>) 13 #<marker at 39 in refile-orgx.org> 14 ((nil #<marker in no buffer> #<marker in no buffer>)) 15 (nil #<marker in no buffer> #<marker in no buffer>) 16 (#<marker in no buffer> #<marker in no buffer>) 17 #<marker in no buffer> 18 (#<marker in no buffer>) 19 #<marker in no buffer> 20 ...)))
        wg-pickel-serialize-objects(#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8 data (((#<buffer todo-orgx.org> #<marker at 1 in todo-orgx.org> #<marker at 158366 in todo-orgx.org>) (#<buffer refile-orgx.org> #<marker at 39 in refile-orgx.org> #<marker at 39 in refile-orgx.org>) (nil #<marker in no buffer> #<marker in no buffer>)) 0 (#<buffer todo-orgx.org> #<marker at 1 in todo-orgx.org> #<marker at 158366 in todo-orgx.org>) 1 #<buffer todo-orgx.org> 2 (#<marker at 1 in todo-orgx.org> #<marker at 158366 in todo-orgx.org>) 3 #<marker at 1 in todo-orgx.org> 4 (#<marker at 158366 in todo-orgx.org>) 5 #<marker at 158366 in todo-orgx.org> 6 nil 7 ((#<buffer refile-orgx.org> #<marker at 39 in refile-orgx.org> #<marker at 39 in refile-orgx.org>) (nil #<marker in no buffer> #<marker in no buffer>)) 8 (#<buffer refile-orgx.org> #<marker at 39 in refile-orgx.org> #<marker at 39 in refile-orgx.org>) 9 #<buffer refile-orgx.org> 10 (#<marker at 39 in refile-orgx.org> #<marker at 39 in refile-orgx.org>) 11 #<marker at 39 in refile-orgx.org> 12 (#<marker at 39 in refile-orgx.org>) 13 #<marker at 39 in refile-orgx.org> 14 ((nil #<marker in no buffer> #<marker in no buffer>)) 15 (nil #<marker in no buffer> #<marker in no buffer>) 16 (#<marker in no buffer> #<marker in no buffer>) 17 #<marker in no buffer> 18 (#<marker in no buffer>) 19 #<marker in no buffer> 20 ...)))
        wg-pickel(((#<buffer todo-orgx.org> #<marker at 1 in todo-orgx.org> #<marker at 158366 in todo-orgx.org>) (#<buffer refile-orgx.org> #<marker at 39 in refile-orgx.org> #<marker at 39 in refile-orgx.org>) (nil #<marker in no buffer> #<marker in no buffer>)))
        ...

then we have a problem in ``wg-pickel`` function. More precisely object
``#<marker in no buffer>`` cannot be serialized. And that was a bug.

To create a test in ``workgroups2-tests.el`` for such situation find this:

   .. code-block:: cl

        (defmacro test-pickel (value)
          "Test `wg-pickel' `wg-unpickel' on VALUE."
          `(eq (wg-unpickel (wg-pickel ,value)) ,value))

        (ert-deftest 110-wg-pickel ()
          (test-pickel 123)
          (test-pickel "str")
          (test-pickel 'symbol)
          (test-pickel (current-buffer))  ; #<buffer tests.el>
          (test-pickel (point-marker))    ; #<marker at 3427 in tests.el>
          (test-pickel (make-marker))     ; #<marker in no buffer>
          (test-pickel (list 'describe-variable 'help-xref-stack-item (get-buffer "*Help*")))
          )

And pass an object that cannot be serialized and should be checked. Then
you need to fix something in ``wg-pickel``, see :ref:`serialize`.
