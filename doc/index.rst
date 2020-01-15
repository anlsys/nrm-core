Welcome to NRM's documentation!
===============================

If you know about NRM and are just looking to get it to run on your
application, please visit the :doc:`quickstart <quickstart>` guide.

This documentation is technical. For a high-level overview of NRM, please
refer to the Argo website_.

The Node Resource Manager(NRM) is a node-local userspace client-server daemon
for managing your scientific applications. It runs the various tasks that
compose an application in resource-constrained slices, monitors
performance, power use and application progress, and arbitrates resources at
the node level, among which CPU Cores, NUMA Nodes, and Power budgets.

There are two user software components shipped with NRM itself: the `nrm`
command-line client and the `nrmd` daemon. Additionally, NRM ships with the
`libnrm` application instrumentation library, to be used for progress
monitoring. The following diagram describes this architecture:

 .. image:: nrm.svg

Note that the container runtime used by NRM to allocate slices is a
system-installed dependency, regardless of whether Argo NodeOS or Singularity is used.

The :doc:`quickstart <quickstart>` guide describes the use of `nrm` and `nrmd`.
Please refer to the :doc:`libnrm <libnrm>` guide for application
instrumentation. See the `haddock`_ documentation for the shared library API,
and the `notebooks`_ for python upstream client use.

.. toctree::
   :maxdepth: 2
   :caption: Contents:

   quickstart
   libnrm
   nrm.so
   notebooks

Indices and tables
==================

* :ref:`search`

.. _website: https://www.mcs.anl.gov/research/projects/argo/overview/nrm/
.. _haddock: http://hnrm.readthedocs.io/en/latest/_static/haddocks
.. _notebooks: http://hnrm.readthedocs.io/en/latest/_static/notebooks
