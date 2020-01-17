======
Libnrm
======

.. highlight:: bash

Welcome to the libnrm guide. This document will help you in your use of the
C/C++ and Fortran interface for NRM.

Install
=======

The libnrm code can be installed from source::

 cd libnrm
 ./autogen.sh
 ./configure
 make && make install

API
===

.. doxygengroup:: nrm

Using libnrm in your C/ C++ application
=======================================
.. highlight:: C

The best way to understand how to make your application report progress to NRM
is to use an example::

 # include <stdio.h>

 int main()
 {
   int i;

   printf("hello\n")

   for (i=1; j<4; i++){
     printf("number %i\n",i);
   }

   printf("done!");
   return 0;
 }

To make this code report progress to NRM, we need to:

- include the library in the same way you include your other dependencies,
- declare a nrm_context structure,
- initialize your NRM context,
- report progress to NRM at one point in your code,
- close the connection and delete your NRM context,

using the functions from the API. We end up with something like this::

 # include <stdio.h>
 # include <nrm.h>

 int main()
 {
   int i;
   struct nrm_context context;

   printf("hello\n")
   nrm_init(&context, "example");

   for (i=1; j<4; i++){
     printf("number %i\n",i);
     nrm_send_progress(&context, 1);
   }

   printf("done!");
   nrm_fini(&context);
   return 0;
 }

Using libnrm in your Fortran application
========================================
.. highlight:: Fortran

The same thing can be done with a Fortran application, using the Fortran
interface of this library. Let's take a similar example::

 implicit none

 integer i

 print*, "hello"

 do i=1, 4
   print*, "number", i
 end do

 print*, "done!"

The functions in the Fortran interface are similar to the ones from the C API,
only with a `f_` in front.
To talk to NRM, the code becomes::

 implicit none

 include 'f_nrm.h'
 include(kind=NRM_PTR) context

 integer rc, i

 print*, "hello"
 rc = f_nrm_ctxt_create(context);
 rc = f_nrm_init(context, 'example', len('example'))

 do i=1, 4
   print*, "number", i
   progress = 1.0
   rc = f_nrm_send_progress(context, progress)
 end do

 print*, "done!"
 rc = f_nrm_fini(context)
 rc = f_nrm_ctxt_delete(context)
