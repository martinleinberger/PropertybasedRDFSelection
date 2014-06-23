PropertybasedRDFSelection
=========================

A F# type provider for working with RDF data. The basic idea is to use a set based approach. One lists properties, e.g. ``hasName.hasOwner`` and gets a type based on the intersection of the two sets of instances that have the property ``hasOwner`` and ``hasName``.
