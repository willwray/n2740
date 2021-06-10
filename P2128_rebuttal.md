n2740 Arguments against `a[i...]` syntax for multidimensional indexing
======================================================================

P2128 rebuttal
--------------

```text
Will Wray  
June 10th, 2021

Document: n2740  
Audience: SG22: WG14 and WG21 LEWG  
Category: Critique 

Target Audience: Array-related, Matrix / Numeric Developers
```

Notation
--------

```text
We denote an extended subscript expression as a[i...] or a[i,j,k]

  • a[i...] 'variadic' indexing expression
             with comma-separated list of 'indices' i...
 ```

Contents
--------

* [Introduction](#introduction)
* [P2128R5 Summary](#p2128R5-summary)
* [The Multidimensional Plan](#the-multidimensional-plan)
* [Summary Recommendations](#summary-recommendations)
* [Case Summary](#case-summary)
* [Terminology](#terminology)
* [Lvalues, References and Proxies](#lvalues-references-and-proxies)
* [Proxies have pitfalls](#proxies-have-pitfalls)
* [Storage, Layout and Indexing](#storage-layout-and-indexing)
* [`a[i]` is Subobject Indexing](#a[i]-is-subobject-indexing)
* [`a[i]` Proxy returns in std](#a[i]-proxy-returns-in-std)
* [`a[i...]` as Subobjects Indexing](#a[i...]-as-subobjects-indexing)
* [`a[b..e)` is Subarray Indexing](#a[b..e\)-as-subarray-indexing)
* [`a(i...)` is Functional Indexing](#a(i...)-is-functional-indexing)
* [`[x...]` in Lambdas and Structured Bindings](#[x...-in-lambdas-and-structured-bindings)
* [Why not nested array?](#why-not-nested-array?)
* [Why are nested arrays unflattenable?](#why-are-nested-arrays-unflattenable?)
* [Why are flat arrays not multi-indexable?](#why-are-flat-arrays-not-multi-indexable?)
* [Flexible nested access](#flexible-nested-access)
* [Why not nested single-index array access?](#why-not-nested-single-index-array-access?)
* [Why not `a[i...]` for multidimensional indexing?](#why-not-a[i...]-for-multidimensional-indexing?)
* [Why not non-member subscript operator?](#why-not-non-member-subscript-operator?)
* [Alternatives for multidimensional indexing](#alternatives-for-multidimensional-indexing?)
* [The span-ish Inquisition](#the-span-ish-inquisition)
* [Why should we care?](#why-should-we-care?)
* [Summing up](#summing-up)

Appendices
----------

* [Span Mechanics](#span-mechanics)

Introduction
------------

```text
This paper presents arguments against the imminent merge of C++ proposal
P2128: "Multidimensional subscript operator", and its usage in standard
library types. Costs, risks and issues should be assessed, and core
language options for the syntax considered, before a decision to merge.

P2128 is the second part of a three-part plan being passed through WG21
and intended to hit the C++23 release cycle (with the first part already
merged in C++20). We argue against the rationale of the plan and suggest
that it be put on hold, pending further recommended actions.

The case is presented via SG22, the joint C and C++ Liaison Study Group,
because the plan sets a direction for language usage that affects the
future development of array indexing facilities in both languages.

The primary question is:

  • What should be the meaning of a[i,j,k] in C and C++ language?

We suggest a useful 'multi-indexing' interpretation of a[i,j,k] over the
assumed 'multidimensional' indexing interpretation of P2128. We analyse
the semantics of the subscript operator and explore possible extensions.
Array-handling extensions would benefit most from a new 'language span',
as gradually introduced in sections below and summarised in an appendix.

The secondary question is:

  • Why should we care? (This is just a C++ operator overload!)

This is addressed in the final sections. The Case Summary is an outline.
We hope our analysis can advance the 'state of array'. Please comment if
you see errors or omissions.
```

P2128R5 Summary
---------------

```text
P2128 proposes to extend the C++ subscript operator[] overload to behave
exactly like the function call operator() overload.

P2128 does not propose any core language meaning for a[i...] syntax
(and the operator cannot be overloaded to act on a builtin array).

The subscript operator overload cannot be a non-member function; P2128
doesn't propose non-member operator[], nor a static operator[] as P1169
does for the function call operator (it does not oppose future proposals
and recommends consistency).

Currently the subscript operator accepts only a single argument (exactly
one argument, like its language counterpart). The proposed change allows
overloads to accept any number of arguments, including no arguments or
variadic arguments, up to the implementation limit.

(Revisions R0, R1 restricted to "1 or more arguments" then R2 and onward
 remove the restriction to accept "0 or more arguments". The reasons for
 its initial restriction and subsequent relaxation aren't documented.)

Below, we denote the operator class, its return and parameter types as
class S, return R and parameters A,B,C in order to avoid any particular
interpretation (as usual for C++ there are no type restrictions):
```

```C++
    S s;       // Class type object s : S with subscript operator[](...)
    s[a];      // R S::operator[](A)      Usual single argument overload

    s[a,b,c];  // R S::operator[](A,B,C)  P2128 three argument overload
    s[];       // R S::operator[]()       P2128 zero argument overload

    T m[2][2]; // Language builtin array type; nested C array
    m[0,1];    // FAILs to compile (in C++20 following P1161, see below)
               // No core language variadic subscript operator
```

```text
This summarizes the proposal itself; the remainder of P2128 expands on
its motivation and critique of alternate syntax choices (EWG requested
more motivation and four new pages were added in R5 under Motivation).
```

The Multidimensional Plan
-------------------------

 >"If P2128 Multidimensional subscript operator is also adopted into C++23,  
 we will likely propose that `basic_mdspan::operator()`  
 be replaced with `basic_mdspan::operator[]` for C++23."

— quoted from P0009R12

```text
P2128 is nearing completion as the second part of this three-part plan:

  1. P1161: Deprecate comma op a[i,j,k] (merged for C++20)
> 2. P2128: Variadic overloads a[i...]  (LEWG review finished -> EWG)
  3. P0009: mdspan, mdarray... a[i...]  (targets C++23, now on R12)

  FIN: a[i...] is 'multidimensional' indexing in std, and likely beyond.

Before P2128 could proceed the a[i...] syntax had first to be reclaimed:
• P1161 Deprecate uses of the comma operator in subscripting expressions
This was swiftly merged for C++20 (and now flagged for Liaison review).

Its ground prepared, P2128 now stakes semantic claim for a[i...] syntax.

The third part of the plan is to populate 'multidimensional' a[i...] in
standard library types std::mdspan and std::mdarray targeted for C++23.

The legacy would be normalisation of a[i...] syntax as multidimensional
indexing and its homogenization with a(i...) syntax, because existing
multidimensional array implementations will not switch to the new syntax
but may add it as an option in order to conform to the new std norm.
```

Summary Recommendations
-----------------------

```text
• WG14 should review P1161 and consider a compatible change (scheduled).
• WG21 should consider to lay off P2128 and its use in std lib, for now.
• SG22 should coordinate efforts on compatible array multi-indexing.
```

Case Summary
------------

```text
P2128 proposes a seemingly C++-specific change; extension of an operator
overload. However, it extends the allowed 'arity' of the operator beyond
what's accepted by the core language and assigns an assumed semantics to
usage of the extended operator, so sets a norm that restricts the design
space for future extension of the builtin operator, in C++ and in C.

The title itself assumes that expression a[i...] is a 'Multidimensional'
subscript operator rather than a neutral 'variadic' subscript operator.
There is no exploration of alternative semantics, only of alternative
syntax choices for the assumed semantics of multidimensional indexing.

Now, if a[i...] is interpreted as multidimensional indexing and if slice
expressions are allowed as i terms in the comma-separated i... then the
notation is deficient because a single index i is rank-reducing whereas
a slice expression as 'index' (b:e or b..e say) is rank-preserving.

We promote an alternative interpretation of a[i...] as 'multi-indexing'
with 'selection' semantics that then irons out this rank inconsistency.

If a[i...] is selection of elements a[i]... within a _single_ dimension
then it is always a rank-reducing operation, but of varying multiplicity
(the comma-separated lists below are imagined tuples of element lvalues
 and not C comma-operator expressions):

    a[i,j,k]  => a[i],a[j],a[k]  General case; 'collective lvalue' tuple

    a[1 .. 4] == a[1,2,3]        Special case; an index-sequence 'slice'
              => a[1],a[2],a[3]  yields a 'span of lvalues' tuple

Collective lvalues can be given useful collective semantics, SIMD-style,
providing valarray facilities as a builtin feature. Boolean indices can
do masked selection, a reduction primitive otherwise hard to mimic.

'Slice' indexing is the special case for a run of consecutive indices.
We denote the equivalent slice operation that preserves rank a[b..e):

    a[b..e]    'span of lvalue' tuple, contiguous lvalues, rank-reduced
    a[b..e)    'span', a subarray-reference of same rank as the array

Here, 'span' is a new C API and ABI type with great unifying potential;
a true reference builtin version of C++ std::span proxy reference.

Since slice indexing of one-dimensional spans of elements is of greater
general importance than multidimensional indexing (which is important,
but to a relative few), unidimensional usage should take precedence.

Chained subscript operators a[i][j][k] aren't a general purpose solution
for abstract indexing purposes. C admits dynamic extents, which improves
flexibility. Allowing index permutations would bring transposed views to
builtin arrays. Extending these features to 'value-parameterize' structs
would cover most multidimensional use cases, with a simple path to JIT.

In modern C++, a comma-separated list within square brackets [i...] is a
sub-syntax of lambda expressions and structured bindings, where the i...
are ids of objects packed into a closure or unpacked from aggregate-like
packs that are flat; not a hierarchical or multidimensional meaning.

Use of a[i...] as 'multidimensional' indexing syntax does not integrate
back into the type system as a 'true' multidimensional array type by the
'declaration follows use' rule; expressions a[], a[i], a[i,j]... don't
correspond to a new array type progression; T[], T[N], T[M,N]... because
T[] and T[N] are existing C array types, and both are of rank 1.

The function call operator a(i...) is long established in C++ as the way
to do multidimensional indexing with a comma-separated list of indices;
it is not ideal, but it is not broken. Comma-separation doesn't strongly
convey 'dimension separation' and its resulting rank-reductions.

As existing practice a(i...) should be the preferred syntax for std lib.
The C committee circa 1994 considered a(i...) as an extension for better
compatibility with this C++ style of indexing (DPCE TR 1994 Appendix 11)
(Appendix 12 deemed slice indexing "desirable and straight-forward").

The call operator implies a general functional relationship from indices
to an indexed entity, so an abstract proxy-reference return is expected.
On the other hand, the subscript operator hints at linear indexing that
addresses an actual subobject or subarray, so the expected type of a[i]
is a direct 'lvalue' reference to the indexed object, and not a proxy.

P2128 weakens this expectation of the subscript operator by making it as
general and abstract as the call operator so that proxy-reference return
must be expected. Proxies have pitfalls (they should not, but they do)
so this is a safety issue and current expectations should be maintained.

The proposed operator[] is identical by design to operator(), and so is
its proposed usage in classes like mdspan. It's another, redundant, way
to do multidimensional indexing with comma-separated list of indices.
This choice will cause churn, uncertainty and challenges in teaching.

The main motivation given for multidimensional a[i...] is as a novice-
friendly syntax that is familiar from languages such as Python's NumPy.
Emulating a[i...] syntax as the way to index multidimensional types in
C++ is a poor fit for the language, disregards C, and even goes against
the Zen of Python, if applied to C and C++.

P2128 is syntax sugar for the proposed std::mdspan API; it doesn't bring
any new functionality. A similar sugaring proposal by the mdspan authors
• P0332 Relaxed Incomplete Multidimensional Array Type Declaration
was rejected as too risky and costly for a 'window dressing' use case.

It is unprecedented for an operator overload to be extended beyond unary
or binary cases that are valid for the builtin operator. A full variadic
signature is most powerful of all; also most tempting and open to abuse.

The a[i...] syntax is valuable turf. It should be reserved for the core
language to consider first. Only once core assigns a meaning, or passes
on the chance, should it be open for user-defined overload and standard
library assignation. It is simply too valuable to grab before sufficient
due diligence is done.
```

Terminology
-----------

```text
From now on, we attach 'indexing' interpretations to expression a[i...]
with 'a' an 'array type' object and i... a list of subscript 'indices':

    A a;     'Array-like' object, a : A, of builtin or class type.

    a[i...]   Builtin indexing operator[] -> language lvalue(s)

  op[](...)   Overload indexing operator[] -> 'reference-like'
                             A::operator[](I...)

  'A':    builtin array-like type (C array or future builtin),
       or C++ array-like class type, with 'array protocol':
  
        • Subscript operator[] overload(s) and possible size(), data(),
          element_type typedef if contiguous, range begin and end, etc.

  i...: The supplied arguments; a comma-separated list of expressions:

    • Convertible-to-integral is required for the builtin operator.

  I...: List of Index parameter types, of any valid function arg type,
        can be deduced from supplied args, by value or by reference:
  
    • Arithmetic types are expected for numerical matrix use cases
      (and usually all of the same integral type).
    • Discrete 'categorical' values for abstract data indexing.
    • Slice or strided wrapper types for 'collective' indexing.

The value of an indexing expression is either 'yielded' from the builtin
operator or is the return value of an operator function overload.
```

Lvalues, References and Proxies
-------------------------------

```text
The builtin subscript operator 'yields' an lvalue and, generalizing, the
extended variadic operator may yield multi-lvalues:

  • builtin operator[]  a[i] -> yields an lvalue
                        a[i...] -> yields lvalue... (tuple of lvalues)

A subscript operator overload is necessarily a function, so there's a
choice of return type in defining the function; by-reference or by-proxy
(or by-value for const access to small arrays):

  • operator overload op[](...) -> returns language reference (or span)
                                           or proxy-reference type

Language spans represent arbitrary contiguous ranges as true references.
They are safer replacements for proxy-references in those cases.

Lvalues
‾‾‾‾‾‾‾
       Yield from builtin operator; a[i...] -> lvalue...

    • A language lvalue yield from the single-index a[i], or
    • a new 'collective lvalue' yield from extended a[i...], or
    • a new 'span of lvalues' yield from slice expr a[b..e].

The builtin single-subscript operator a[i] yields a language lvalue,
then we consider below that extended a[i...] yields extended lvalues;
either a general collective lvalue or a special-case span of lvalues
when i... is a sequence of consecutive integers.

The language has more latitude in handling builtin operator yields than
it has for function returns; collective semantics for collective lvalues
is one possibility explored below.

References
‾‾‾‾‾‾‾‾‾‾
       Return from overload operator[](...) -> language reference

  1. C++ reference to element;  E&, E const&, E&&, etc..., or
  2. Language span; a new subarray-reference type, E[) or E[N).

A C++ reference type is the closest equivalent return type to an lvalue.
Like an lvalue it only binds to a subobject (without reinterpret cast).
This is a both a strength and a major restriction on handling subarrays;
a bound reference is as good as the object itself but a reference cannot
bind to a subarray of arbitrary span (when the span is not a subobject).

What if a function could return reference-to-subarray of arbitrary span?

Language span is the reference version of Dennis Ritchie's 'fat pointer'
that carries both a pointer to the start of its range and the length.
Available to both C and C++ it would provide flexibility and safety for
array and buffer handling APIs, cross-platform and cross-language.

  'Language span' is the missing link; a new API and ABI type that
   allows to yield and pass arbitrary contiguous ranges of elements.

std::span paved the way as 'the best span' that can be implemented as a
C++ proxy-reference class type; a language span can be better all round,
as has been demonstrated in other languages that incorporate span types.

Proxies
‾‾‾‾‾‾‾
       Return from overload operator[](...) -> proxy class type

When there's no subobject to be referenced, or contiguous span to view,
then it's necessary to fall back to a proxy-reference return.

Proxy-reference return types open the door to the full power of abstract
indexing return-type possibilities.
```

Proxies have pitfalls
---------------------

```text
Taking a proxy by value and then using its operator=() is idiomatic use.
Unfortunately, the same code for a reference return mutates a temporary
(e.g. let T = bool and then A = T[] or A = std::vector<T> below):
```

```C
    A a = {1,0};    // Generic array type A, so must handle generically
    auto a0 = a[0]; // Temporary copy of element 0, or copy of proxy?
    a0 = 0;         // Mutate temporary copy, or mutate proxied element?
```

```text
The problem is not necessarily to do with generic 'auto' type deduction.
This non-generic code explicitly binds a reference to an element type T,
or to a temporary returned by the converting operator T() of proxy<T>
(so fails in exactly the opposite way to the previous case above):
```

```C
    T&& a0 = move(a)[0];
    a0 = 0;         // Mutate element, or mutate a proxy-made temporary?
```

```text
In both cases, the generic solution is to use decltype(auto) to deduce
the type of the local variable a0. However, decltype(auto) isn't allowed
for function argument passing, which is the more likely scenario here; a
forwarding reference is the generic way to pass arguments - a compromise
solution with its own trade-offs and pitfalls.
```

Storage, Layout and Indexing
----------------------------

```text
Multidimensional array interfaces often distinguish Storage and Indexing
(which are not always distinguished in unidimensional array interfaces).

Here we distinguish a third intermediate level; an optional subdivision
of Storage into logical Layout:

• 'Storage' according to the 'C object model', with mapping to memory.
• 'Layout' is a logical division of Storage as a hierarchy of subranges.
• 'Indexing' is a notional interpretation of the logical Layout.

These are our terms, to help treat nested C array alongside flat-storage
arrays in the following discussions. The Storage / Layout distinction is
imposed by a strict object model whose relaxation may blur the boundary
if Layout can flatten nested Storage, or flat storage used as-if nested.

• Subobject Storage:
  Physical layout of subobjects in a compound aggregate type hierarchy.
 
• Subdivision Layout (optional):
  Logical subdivision of the extents of leaf Storage arrays.

• Indexing API:
  Semantical data shape as advertised by the type's indexing API.

Note that our Layout is not the physical 'standard' layout of Storage;
it is an optional 'logical' partition within a storage array subobject.
The map to memory is still monotonic linear, but with finer subdivision.
Indexing API can break monotonicity and linearity of offset computation;
it is fully abstract (and mostly not material to this case).
```

`a[i]` is Subobject Indexing
--------------------------

```text
For array 'a', the expression a[i] directly addresses object storage for
element i as a subobject of the array:

  • a[i] is 'subobject indexing'

This is always true in C and has provided strong guidance so far for C++
array-like types; std:: array, vector, string, span, unique_ptr<T[]>, as
well as array-like iterators. In particular, it includes the nested case
when the element is another subscriptable type, including a nested array
(i.e. as a subarray subobject).

It is also true for node-based containers like std::map if the nodes are
objects, even if they're scattered in memory and indexed by key, but our
focus here is on array-like types with contiguous layout at some level.

'Subobject' is an inflexible restriction as it can only refer to Storage
subobjects and not to logical Layout, without reinterpret cast. A future
language span type would allow to address arbitrary subspans of elements
as if they were subarray subobjects, so extending a[i] to address Layout
more freely, as explored in later sections on flexible nested access.

For fully abstract Indexing, proxy-reference types become necessary and
the use of a[i] syntax may be a poor fit, semantically and technically.
```

`a[i]` Proxy returns in std
-------------------------

```text
There are two counter-examples in std for "a[i] is subobject indexing";
1. bit-string types that present an illusion of subobject indexing, and 
2. types that extend the subscript to do slicing, index lists and masks.

1. vector of bool
   ‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Infamously, vector<bool> unexpectedly returns vector<bool>::reference,
a proxy-reference for a single bit of storage, breaking the contract for
vector<T> which should return T& according to its iterator requirements
(and the reference type also has an implicit bool conversion operator).

The original sgi STL had a separate bit_vector type and std::bitset is a
fixed-length array of bits. In both cases a[i] returns a proxy-reference
similar to vector but, as distinct types, their requirements are relaxed
to accommodate the proxy return.

2. valarray
   ‾‾‾‾‾‾‾‾
For mutable access, the valarray<T> types provide four advanced indexing
modes, selected by overload resolution on index type, that return proxy-
reference types (and a usual single-integer subscript that returns T&):

                                va : valarray<T>  (non-const)
             index type                           return type

          i : size_t               va[ i ]    ->      T&

    a_slice : slice             va[ a_slice ] ->  slice_array<T>
    g_slice : gslice            va[ g_slice ] -> gslice_array<T>
    va_bool : valarray<bool>    va[ va_bool ] ->   mask_array<T>
    va_inds : valarray<size_t>  va[ va_inds ] -> indirect_array<T>

(For const va, the overloads return by-value copies, T or valarray<T>.)
```

`a[i...]` as Subobjects Indexing
--------------------------------

```text
Here, we suggest:

  • a[i,j,k] is 'Subobjects Indexing' (note the plural)
             or 'Elements Selection'

(and defer analysis as multidimensional indexing to a later section).

Here, a[i...] is defined to select elements by-index. As a special-case,
with all arguments bool valued expressions, and of the same number as
elements in the array, then it is defined as masked selection by-bool:

  • a[i,j,k] is 'Selection by Index' -> a[i],a[j],a[k] 'lvalues tuple'

  • a[false,true,true] is 'Masking'  -> a[1],a[2]

This interpretation provides a consistent language-level basis for array
functionality similar to valarray, with greater optimization potential.
Pack expansion in C++ has overlap, with a different set of trade offs.
Here, selection is developed as an array-focused language extension.

There are many C extension languages. We surveyed a few forks targeted
at numerical and data-parallel computation; Cstar, Ch, Cn, Cilk, dpc++.
So far we find none that use a[i...] syntax, while many have extensions
for slice indexing.

Slice indexing was proposed in X3J11/94-025 DPCE Array Slicing Proposal,
using the colon-separated syntax familiar from several other languages.

(Slicing notation in other languages varies; stride is sometimes the mid
 index, end is sometimes an inclusive index, negative indexing back from
 the end may be allowed, the separator is sometimes double-dot ..)

      a[b:e] and a[b:e:s] = 'Slice Indexing' and 'Strided Indexing'

      where b,e,s are (b)egin and (e)nd indices with (s)tride

here, [b:e] corresponds to the half-open range of indices [b..e)
(we ignore stride for now to focus on contiguous ranges only):

      [b..e) -> b, b+1, b+2 ... e-2, e-1                      b,c,d

For reasons that will become clear, we prefer that if slice indexing is
added, it is denoted with the double-dot '..' syntax and not colon ':'

      a[b..e]  // preferred syntax for slice indexing (and not a[b:e])

Array slice expression a[b..e] should be equivalent to its expansion:

      a[b..e] -> a[b,   b+1,   b+2  ...   e-2,   e-1]        a[b,c,d]

So the slice expression a[b..e] is an abbreviation of the expanded form
a[b,c,d], itself a special case of 'element selection' a[i,j,k] in which
the indices [b..e) => b,c,d are a range of consecutive integers, and the
array slice is then a contiguous sequence of elements; a[b], a[c], a[d].

      a[b..e] -> a[b,   b+1,   b+2  ...   e-2,   e-1]        a[b,c,d]

              -> a[b],a[b+1],a[b+2] ... a[e-2],a[e-1]     a[b],a[c],a[d]

This last form is a notional 'naked language tuple' or 'selected-element
tie' of array element lvalues (and not the C comma-operator expression).
This can be granted collective semantics as an operand of the built-in
operators, or their overloads, e.g.:
```

```C++
      a[i,j,k] *= 2   ->   a[i]*=2, a[j]*=2, a[k]*=2

      z[..] = scalar * x[..] + y[..]  // "A-X Plus Y" vector operation
                                      // where a[..] is a "full slice"
```

```text
These collective operations are performed elementwise. Self assignments
and compound assignments are not applied sequentially as the comma-list
rewrites suggest; lvalue to rvalue conversions are implied on RHS values
in order to perform the overall combination or permutation requested:
```

```C++
    a[b..e] = a[i,j,k] => a[b]=T(a[i]), a[c]=T(a[j]), a[d]=T(a[k])
                               ^             ^             ^
                               ^ Lvalue to Rvalue conversions

    a[1,2] = a[2,1];   // swap two elements

    a[..] = a[1,2,0];  // It's a rotate !

    a[2,0,1] = a[..];  // The same rotate, but with LHS target permuted
    a[1,2,0] = a[..];  // The inverse rotate

    int rot[] = {1,2,0};
    a[..] = a[rot[..]];// The same rotate, selection by index-array[..]

    T nonzeros[] = {   // Reduction step to remove zero values
       a[ a[..] != 0 ] // Selection by bool as aggregate initializers
      };
```

```text
When the compiler has full static information then permutations can be
expected to optimize well for latency, throughput or temporary storage.

The interpretation here of a[i,j,k] as 'Element Selection' is powerful
as a builtin facility but limited to language-level unless extended to
integrate 'collective lvalue' tuples more closely into the type system.
```

`a[b..e)` as Subarray Indexing
------------------------------

```text
The previous sections extended from single-index a[i] to multi-indexed
a[i...] then contiguous slice-indexed a[b..e], all square-bracketed and
all interpreted as rank-reducing operations, rank(a[i]) == rank(a) - 1:

       a[i] is 'Subobject Indexing'        \
    a[i...] is 'Subobjects Indexing'       |- rank-reducing
    a[b..e] is 'Subobjects Slice Indexing' /

Now, slice indexing is more compelling as a rank-preserving operation.
We denote this with half-open bracket [) syntax:

    a[b..e) is 'Subarray Indexing'            rank-preserving

The indexing expression a[b..e) yields a span, rank(a[b..e)) = rank(a),
of static or dynamic size, type T[N) or T[), which acts as an array type
(but better behaved than C array, no decay etc.; see Appendix A).

Slicing a span, as 'Subarray' indexing, is the happy medium between
inflexible 'Subobject' indexing and fully flexible 'Proxy' indexing:

  • 'Subobject' is too inflexible; it can only refer to Storage objects.
  • 'Subarray' is a happy medium as a reference to logical Layout.
  • 'Proxy' is often too flexible as a fully abstract Indexing return.
```

`a(i...)` is Functional Indexing
------------------------------

```text
The function call operator a(i...) is the established convention in C++
for indexing multidimensional array types with general Indexing schemes,
i.e. beyond the hierarchical, rectangular, dense, row-major Layout case
for which chained single-index subscript operators should suffice:

   a(i...) is 'full-rank' indexing if sizeof...(i) == rank(a), else
   a(j...) is 'partial' indexing when sizeof...(j) <  rank(a), and
   a()     is 'empty' indexing, an idempotent operation a() == a()()...

Partial indexing takes fewer than rank(a) indices to return some sort of
subindexed subarray, not expected to be a subobject so a proxy-reference
return is expected as signalled by the functional indexing syntax (...).
Partial indexing is expected to compose, as for any partial application:

   a(i...)(j...) == a(i..., j...) is composition of partial indexing

Full-rank indexing is expected to return reference-to-element, an lvalue
(as elements are expected to be subobjects) (or it could return a rank-0
array reference type). Use of [] syntax only in this special case and ()
otherwise would make for a confusing API.

The recommendation is to use functional indexing in general for possible
proxy-returning types and avoid special-casing full-rank.

The bounds-checked indexing convention is also 'functional' in that it
uses a named member function, a.at(i...), or free function, at(a,i...).
The reference versus proxy-return is more easily flagged there by naming
the proxy-return version differently.
```

`[x...]` in Lambdas and Structured Bindings
-----------------------------------------

```text
This section examines what [x...] means when it appears as a sub-syntax
in two newer C++ constructs, lambda expressions and structured bindings,
and how that might inform its meaning in a[i...] indexing expression.

Structured binding (since C++17 with C++20 updates):
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    auto [i,j,k] = array_of_int{1,2,3}; // typedef int array_of_int[];

Here, the i,j,k are identifiers, introduced by this structured binding
declaration. The ids bind to each element of the RHS expression, in this
case an array rvalue whose lifetime is extended to that of its bindings,
so i,j,k are names for the three elements of the otherwise unnamed array
which could then be accessed via its first named element as (&i)[index].

Lambda expression (since C++11 with updates in 14, 17, 20):
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    auto ijk = [i,j,k]{}; // likely layout is struct { int i,j,k; };

This declares a lambda expression that captures i,j,k by value, i.e. it
copy-captures three ints into its closure object (layout is unspecified;
implementations usually lay out copy captures as the equivalent struct).

Binding to lambda
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
The square brackets containing structured binding ids mean 'unpacking'.
The square brackets containing by-copy captures mean (re)'packing'.

There's a duality here. Above we saw that a lambda can capture bindings
(since C++20) and a gcc 'feature' lets us do the opposite:

    auto [u,v,w] = I{[U=1,V=2,W=3]{}}; // template<class O>struct I:O{};

Three ints are packed into a closure then unpacked as binding ids
(this 'feature' provides a handy language tuple, say for multi-returns,
 via a record-breakingly short thirty-character tuple implementation).

Array superpowers
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
These two language features share an array-related superpower ability; 
both structured bindings and lambda copy-captures can copy arrays;

    int a[] = {1,2,3};
    auto [x,y,z] = a;  // magic array copy
    auto l = [a]{};    // magic array copy

There is apparently a close affinity between array 'element selection'
and the [x...] sub-syntax as already populated in these C++ constructs.
```

Why not nested array?
---------------------

> **A multidimensional array is not an array-of-array-of-array-of...**  
 ...  
While such types have some similarity to multidimensional arrays, they  
do not provide adequate multidimensional array functionality ...  
Two critical functionality differences are  
(1) multiple dynamic extents and  
(2) polymorphic mapping of multidimensional indices to element objects.

— From the mdspan proposal P0009R12

```text
Nested arrays, and nested access syntax, are a good fit for data that is
naturally laid out in nested hierarchies. Strengths and shortcomings are
covered in this section. Extensions to improve flexibility are suggested
in the following sections, in response to the points quoted above:

 (2) Polymorphic mapping of multidimensional indices to element objects:

     This reflects how nested array Indexing access is tied to Storage.
     This is too rigid. We suggest to relax nested access, allowing to:
      • View nested arrays as if flat
      • View flat arrays as if nested, via span binding (and no casts).

 (1) Multiple dynamic extents:
 
     Dynamic extents are supported by C99 array extensions
     (VM variably-modified types and VLA variable-length arrays).
     We suggest to allow dynamic 'value-parameterized' span bindings 
     for safer handles to variable-sized data, with nested access.

 (0) A multidimensional array is not an array-of-array-of-array-of...

     True, nested arrays are multidimensional only by convention, but it
     is a truism to say that they are not multidimensional; type traits
     for array type A report nested rank<A> and Ith nested extent<A,I>;
     this is a standardized convention.

Nested arrays have pros or cons compared to array types that store their
elements in one flat array, depending on access pattern and needs:

 Pro: Nested arrays are built-in, simple, direct and elegant.
 Pro: The nested structure provides extra information for the compiler.
 Con: Nested storage forces recursive access for whole-array operations.
 Con: Too rigid; cannot be reshaped (without a reinterpret cast and UB).
 Con: Elements are no longer contiguous across subobject boundaries.

E.g. std::swap supports nested arrays (DR following 2008 LWG issue 809).
Its generated code can be suboptimal compared to non-recursive code that
treats the elements as if they were laid out contiguously (UB).

    int a[2][2];
    int b[2][2];
    std::swap(a,b);         // recursive implementation, non-UB access
    my_swap(a,b);           // memcpy, good speed but UB; non-constexpr

On the other hand, for algorithms that naturally access nested elements
recursively, nested storage array can be faster than flat storage array
if a compiler can exploit no-alias access between nested array objects.

This lack of flexibility and element-contiguity contributes to a decline
in use and support of nested arrays in C++ array types and algorithms.
E.g. C++20 ranges copy algorithm works for 1D arrays but not for nested:

    std::ranges::copy(a,b);      // compile error: no match for call

(C++ proposal P1997 "Relax restrictions on array" could fix this case.)
A compile-error is better than an erroneous result at runtime, as below:

    using A = std::array<int[1],1>;    // std array of nested C array
    A x={1}; A y = x; assert(x != y);  // Copy works, comparison fails !

Here, array is accepted as a std::array element type then initialization
and copy work fine as builtin aggregate operations, but comparisons fail
(C++20 defaulted comparison operators would work correctly in this case)
(but may be an ABI break?). Similar applies to pair and tuple.
```

Why are nested arrays unflattenable?
------------------------------------

```text
 • Nest -> Flat:
     Allow opt-in element-contiguous access to a nested array
    (so opting out of any no-alias benefits of subobject access and
     disabling padding that may otherwise be added for any purpose).

It is undefined behaviour to treat a nested array as if a flat array of
elements in memory, even though contiguous layout is almost ubiquitous.
Only in rare cases is any padding added to array layout (when compiling
with particular flags to detect out of bounds access, for example).

There should be some way to specify flat, element-contiguous, layout,
e.g. by annotation on nested array declarations (presence of padding
can be detected via sizeof comparison; we want to disable any padding).

Then, a flattening access operation can merge the current array extent
with its next nested extent; let's spell the operation a[><]:

    T nest[M][N];     // Annotation needed for T element-contiguity
    T* p = nest[><];  // Scottish flag operator (and decay, the noo)
    nest[><][i] = *p; //      nest[><] : T[M*N] flattened array

A possible annotation is the 'layoutas' keyword specifier described in
C++ proposal P1912 "Types with array-like object representations":

    T layoutas(T[M*N]) nest[M][N];  // Explicit flat-layout annotation

guaranteeing that nest is pointer-interconvertible and layout-compatible
with flat array layout (though nested arrays aren't explicitly covered).

Assuming that a well-defined way can be found to flatten nested arrays,
then the mdspan design decision to not support nested arrays is suaged
because client code can flatten arrays first, before taking a span view,
or mdspan can provide the conversion as a convenience.
```

Why are flat arrays not multi-indexable?
----------------------------------------

```text
 Flat -> Nest:
    Allow flat array access as if nested, without reinterpret cast
   (so bringing any no-alias benefits of known hierarchical structure).

In other words, a compiler-indexed 'nested view' of a flat buffer.
This can be done via span bindings. For example, binding 1D, 2D and 3D
spans to a flat array 'buffer' (see Appendix A for span notation); the
span, nest and w variables below are array references of span type, and
the span bindings are allowed if within the initializing array's extent:

    T flat[M*N];         // No annotation necessary; T[] contiguous 1D
    T span[M*N) = flat;  // span : T[M*N)  "span-of-MxN-T"          1D

    T nest[M)[N] = flat; // nest : T[M)[N] "span-of-M-array-of-N-T" 2D
    T w[X)[Y][Z] = flat; // w : T[X)[Y][Z] "span-of-X-array-of-Y-
                                                     -array-of-Z-T" 3D

Partial indexing expression nest[i] is then well formed; it conjures up
a subarray lvalue where no actual T[N] subobject exists. It is seen as a
reference, but not a reference to an extant object:

    nest[i];             // nest[i] : T[N) "span-of-N-T" lvalue     1D
    nest[i][j];          // flat[i*N + j] addresses the same element T

We assigned the type T[N) to the lvalue rather than C++ lvalue reference
T(&)[N] which has to bind to a T[N] object lvalue and behaves like T[N].
As spans don't decay, nested access via a span is fortified compared to
direct access via the array id.

Nested access via nest[i] opens a new, smaller, window into the larger
flat buffer - an overlapping access path that could assist or could
compromise aliasing analysis depending on how the potentially aliasing
paths are used (and what a compiler can prove).

Compared to merged C++ proposal P0593: "Implicit creation of objects for
low-level object manipulation", no objects are created here, only a span
reference; "Explicit creation of lvalues for nested array access"?
```

Flexible nested access
----------------------

```text
This section speculates on possibilities for more flexible array access,
and on ways to extend the flexibility to structs, classes and functions.

The multidimensional span bindings of last section give nested access to
flat Storage, so raise the level of abstraction from physical to logical
Layout, but static extents give static layout, and the layout is linear.
We explore:

  • Dynamic extents that then enable dynamic Layout.

  • Index permutations for strided access, e.g. transposed Indexing.

  • Higher level use of extent value parameters in user-defined structs,
    classes, and functions; both static and dynamic parameterization.

Dynamic extents
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
Arrays, if declared as usual with extents specified by integral constant
expressions, have static types. C99 arrays also accept declarations with
non-constant extents giving 'VM' variably modified types:

    int v = atoi(argv[0]);
    T a2v[2][v];          // VLA with one static, one dynamic extent
    T (*p2v)[][v] = &a2v; // VM pointer type initialized to point to VLA
    (*p2v)[i][j];         // builtin indexing with the runtime extent v

Language spans are better variable-size data handles than VM pointers;
safer with no decay, and flexible in binding to more compatible types.
Here, a span deduces the 'staticity' of extents as well as their values:

    T s2v[)[] = a2v;      // s2v : T[2)[v] static span of 2 dynamic T[v]
    s2v[i][j];            // builtin indexing with the runtime extent v

Dynamic spans are 'fat' references; unlike static span, or analogous C++
reference-to-array, they manage extra storage for their dynamic extents,
to be applied when used in nested access indexing calculations.

Index permutations
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
This briefest sketch looks at how strided access can be achieved through
changing the order of application of indices.

There are N! permutations of N indices in ND. For 2D array an index swap
is the only non-identity permutation; we denote it here with postfix ~

    T mn[M][N];             // row-major M row x N column
    mn[i][j] == mn~[j][i];  // mn~ gives column-major access as NxM

    T m[N][N];              // Square matrix NxN
    m[..][..] = m~[..][..]; // In-place transpose

Here, ~ is a type-compounding operation, mn : T[M][N], mn~ : T~[M][N]
then partial indexing must memo the indexed extent N to use as a stride
in subsequent indexing or iteration, mn~[j] : [N]T[M) - the used extent
is shown as stacking up to the left of T; span-of-M N-strided-T.

The first supplied index j is a unit-stride offset, then the next index
i is N-strided, so the resulting offset is i*N + j, as expected.

Value-parameterized types
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
More flexible than a 'Flexible Array Member' FAM struct...

The extended array notation so far applies to builtin array types only.
The notation might extend to 'value parameterized' structs, spans, etc.:

    typedef
    struct[int M][int N]  // M and N are value parameters, they can be
    {                     // static or dynamic (unlike template NTTPs)
      T m2[2*M];
      T mn[M][N];
      T nn[N*N];
    }
    X2D;                  // 'X2D' id isn't a type, until parameterized

Here, M and N are like C++ non-type template parameters except that they
can be static or runtime-determined values. The 'X2D' id doesn't name a
type itself, yet, but a template-like struct definition that must first
be parameterized with two values to complete it as a type:

    X2D[3][v] x3v;        // Instantiate type with static 3, dynamic v
    X2D s3v[)[] = x3v;    // Deduce span with static 3, dynamic v params
    s3v.mn[i][j];         // builtin indexing with the runtime extent v

The span variable manages the fat-reference parameters as runtime values
or as compile-time constants. Dynamic value-parameterized types would be
more easily JIT compiled than type-parameterized templates.

In C++, value-parameterized class types can then have extended operators
to provide safer construction, access and destruction. In particular the
nested access syntax could be overloaded to generate spans and not proxy
references so that both declaration and use look like array syntax:

    X2D x3v[3][v];        // C++ value-parameterized class with op[]
    x3v[i][j];            // op-overload indexing via spans, not proxies

Compared to C++ proposals like N4025 "Exploring classes of runtime size"
this is a more limited but more straightforward scheme.
```

Why not nested single-index array access?
-----------------------------------------

c.f. **P2128 section
"Chain of single-argument array access operators; `a[x][y][z]`"**

>(1) `a[x][y]` implies that `a[x]` is a valid expression  
 (2) `a[x][y]` notation doesn't tell users about the copy behavior of
     `a[x]`  
 (3) `a[x][y]` strongly suggests an array of arrays  
 (4) `a[i...]` variadic signature is more friendly to pack expansion  

```text
Our responses are general agreement to these points from P2128's section
on 'nested access', with different conclusions:

 1) YES: a[x][y] does imply that a[x] is a valid expression; GOOD.

 2) YES: the notation DOES tell users about the copy behavior of a[x].

   It proclaims 'subobject' or 'subarray' semantics which then implies
   copy semantics for a regular element type (array itself isn't regular
   copyable; P1997 fixes that).

 3) YES: a[x][y] strongly suggests an array of arrays; GOOD.

   This is a strength of [] syntax; it denotes hierarchical subarrays.
   It makes less sense for arrays that don't have a hierarchical view.

 4) YES: a[i...] variadic signature is more friendly to pack expansion.

   As is a(i...) signature, already well-established and available...
   Pack expansion is less needed with our indexing guidelines, and is
   anyway either easily dealt with or avoided. The majority of use cases
   of a multidimensional type are either 2D or low fixed dimension.

Nested access syntax is appropriate for dense, rectangular, hierarchical
array Storage or Layout. We agree that, beyond this, a[x][y][z] syntax
isn't a general-purpose solution for abstract multidimensional indexing,
at present, though the ideas suggested above could significantly extend
the flexibility of nested access syntax.

Once proxy return types become necessary then it is difficult to design
well-defined nesting single-subscript operators that also perform well.
In these cases, partial indexing with a variadic subscript operator has
exactly the same issues and implementation difficulties.

Conclusion: don't use subscript operators, single-index or variadic, for
abstract multidimensional types. Proxies and subscripts don't mix well;
a poor fit in semantical and mechanical terms. Use functional indexing
syntax for general purpose array indexing interfaces, a(i...).
```

Why not `a[i...]` for multidimensional indexing?
----------------------------------------------

```test
By C's "declaration follows use" rule, the declaration of array-of-T 'a'
follows its use in the indexing expression a[i] -> T to give T a[N];

If, in future, a[i...] syntax indexes a new builtin array type then,
according the rule, the type of the new builtin array should be T[D...]:

    a[i...] expressions imply T[D...] types

    T[D...] : T[] rank-0, T[N] rank-1, T[M,N] rank-2, T[L,M,N] rank3...

This fails for the rank-0 and rank-1 cases; T[] has a conficting meaning
as a rank-1 array of unknown or incomplete bound and T[N] corresponds to
C array, so fails to distinguish itself as a new and different type (of
a new family of array types we'd want to behave differently to C array).

Comma-separated lists i... in C and C++ most usually denote flat lists
of entities, and not ranked levels in a hierarchy. The bracketed [i] in
a[i] is a rank-reducing operation. The bracketed [i,j] in a[i,j] reduces
rank twice; the syntax does not strongly convey the extra rank reduction
performed by the comma. Then slice expression indices don't reduce rank.
The rank of a 'multidimensional' a[i...] expression is hard to devine.

The a[i...] syntax usage proposed by P2128 is inspired by the a(i...)
syntax that it seeks to replace. The function call syntax is not ideal
and P2128 replicates its shortcomings without addressing them; it is a
shallow change that blocks opportunities for deeper, more useful change.

Any use of a[i...] syntax should maintain the stong semantic meaning of
a[i] as yielding 'subarray lvalue'. Reference-like proxy returns weaken
this expectation; functional a(i...) syntax is a more honest expression
of such abstract indexing use cases.

(P0009 std::mdspan proposal promotes a specific use case of operator[]
 for full-rank multidimensional indexing returning an element reference.
 Because mdspan chooses not to implement partial indexing, consequences
 of returning proxy-reference subarray types have not been sufficiently
 explored and so there are likely to be unintended consequences.)

This said, a variadic subscript operator would be useful for libraries
that integrate or interoperate with array types or interfaces in other
languages that do use a[i...] syntax for multidimensional indexing, to
emulate the foreign language syntax as closely as possible. The costs
and consequences of a[i...] must be weighed against its potential value.
```

Why not non-member subscript operator?
--------------------------------------

```text
This question is raised in P2128 section "Non-member operator[]?" where
it is thought out of scope.

Both the P2128 proposed variadic operator[] and the current single-index
operator[] are member functions, which causes implementation problems.
Two overloads are generally needed, for const and non const access, and,
to match value category, other reference-qualified overloads are needed.

A non-member subscript would be more powerful and more useful than the
member function. It allows to extend existing interfaces. It could be
overloaded for C array.

We are torn. On the whole it seems better not to mess with subscripts.
```

Alternatives for multidimensional indexing
------------------------------------------

```text
If square-bracket syntax is to be used for multidimensional indexing,
then an extra level of 'dimension delimiter' punctuation adds utility,
semantic clarity and differentiation. 

Comma-separated lists can then appear in each dimension-delimited slot.
Semicolon termination an obvious contender:

    a[i;j;k;]         // E.g. semicolons as 'dimension terminators'

    a[i; j,l; k..m;]  // Semicolon-terminated list of
                     // three comma-separated lists of indices or slices

The punctuation can denote a sequence of multidimensional array types:

    T[Nd;...;] : T[;] rank-0, T[N1;] rank-1, T[N1;N2;] rank-2 ...

These are direct variadic compounds of T and Nd... => N1,N2... (N0 == 1)
(and not increasingly compounded types like nested C array).

Variadic signature is necessary to replace the function call operator or
the proposed variadic subscript operator. 'Doubly variadic' list of list
signatures is a new and more disruptive change; the overload signatures
for such an operator[;] might be:

    auto A::operator[;]()
    auto A::operator[;](auto i0, auto...i)
    auto A::operator[;](auto i0, auto...i; auto j0, auto...j)
    ...

The rank-1 case can be conscripted for a[i...] comma-separated list use.

An alternative notation revives BCPL indexing syntax a!i bred with its
ternary operator c?a:b to spawn a variadic trailing colon ':' that acts
as its dimension-separator, mutating BCPL infix binary operator ! into a
postfix operator !: that can then be unary, binary, ternary, quaternary
...
    T!Nd:... : T! rank-0, T!N rank-1, T!M:N rank-2, T!L:M:N rank-3 ...

This notation avoids brackets of any shape. Distinct syntax can be given
distinct semantics, e.g. order of evaluation of arguments.

Of course, nested array syntax, a[i][j][k] already has a strong existing
dimension separator '][' - why not expend effort to extend this existing
syntax to have more flexible, C-compatible semantics.
```

The span-ish Inquisition
------------------------

```text
What is mdspan and why does it ask for variadic extension of operator[]?
A brief diversion into span design seems not entirely inappropriate.

std::span<T> 'dynamic span' ("the best span"), and
std::span<T,N> 'static span' (C++20):

  • Satisfy the 'array protocol'.
  • Don't provide deep array assignment and comparison operations.
  • Static span is not as generally useful in APIs as dynamic span.

Dynamic std::span<T>, for concrete type T, is "the best span" for use as
a non-templated API type to accept any type that converts to span<T>, as
compatible std types do and user-defined types can choose to.

Dynamic span is the best model of a 'vocabulary' type - a common lingua-
Franca conversion target for communicating a range of T, regardless of
origin, and with very low cost both at compile time and at runtime.

For non-concrete T, type deduction disables the crucial conversions, so
callers would then have to do explicit conversion to span, which negates
both the simpicity and the convenience of concrete span APIs.

Static std::span<T,N> offers explicit 'constant folding' optimization at
the loss of generality. Again, one can't deduce T or N without disabling
the crucial conversions, so its specificity limits its applicability.

C++20 Range concepts are the new generic solution for accepting ranges.
std::span is a contiguous, sized range, with a subscript operator and an
element_type typedef. We say that it satisfies the 'array protocol' (our
term; there's no std concept). There's no std trait for static size.

We note that a P2128 author wrote a scathing critique of std::span:

      Something very, very wrong with span:
      "... I would go so far as to say that span ... breaks C++."

This view seems at odds with the plan to standardise a multidimensional
generalisation of std::span; will mdspan break C++ multidimensionally?

Vamos a ver...

std::mdspan<T,...> (P0009 C++23 target):

  • Doesn't extend the 'array protocol' to multidimensions.
  • Too many template signature types so less useful than span in APIs.

Inspired by std::span, mdspan presumably aims to be used primarily as a
concrete dynamic span 'vocabulary' type in APIs. However, moving up to
multiple dimensions brings a combinatorial explosion of type signatures
(dynamic span<T> is already a family of types, for each concrete T).
At some point either type deduction or type erasure is called for, both
of which negate the point of a span type.

The mdspan proposal's Description doesn't describe its 'span' aspect, it
talks about mapping multidimensional indices, Layout mapping and what it
is not (it is not an array-of-array-of-array-of...). The mdspan design
encapsulates these indexing schemes rather than exposing them.

So, the main 'single responsibility' for an effort to standardize multi-
dimensional type support might be better focused on the index mapping;
formalizing the nascent array protocol, adding support for static extent
checking and then extending the protocol to communicate indexing schemes
in multiple dimensions.

mdspan:

  • Doesn't provide deep array assignment and comparison operations.
  • Does not provide partial indexing, only full-rank indexing.
  • Repeated dynamic extents, for square matrices say, are all stored.

The issue of whether or not assignment and comparison operations should
be included remains controversial. We believe that they should be; mspan
is impoverished by not offering these whole-array operations (users can
implement missing comparison operators as non-member functions, but only
by opening namespace std which is not strictly allowed) (assignment has
to be a member function so can't be implemented outside the class).
```

Why should we care?
-------------------

```text
The subscript operator is a paradoxical, pre-C-historical relic from B.
Its definition in terms of pointer arithmetic forces decay on arrays, so
implicating it in C's Biggest Mistake; conflating pointers with arrays,
until the dereference redeems it. The subscript is a powerful survivor,
its strong sematics carried it on to C++ with minimal change.

The builtin subscript operator conveys strong 'subobject' semantics that
proxy-returning overloads can't match.

So, the subscript operator is also implicated in that C++ embarrassment;
vector-of-bool's mis-specialization. Extension to multidimensional usage
will lead to more such incidents of unexpected poorly-specified proxies;
the multidimensional subscript conflates lvalues with proxies, weakening
semantic expectations.

P2128 spends over four pages on Expression rewriting schemes that might
make all of a(i,j,k), a[i][j][k], a[i,j,k] semantically equivalent.
Syntax is our notation, not to be put in a blender and homogenized.

Dennis Ritchie was surely familiar with the work of Ken Iverson, creator
of APL (DMR's first programming assignment was to machine-code some APL
operators for the UNIVAC I at Havard, while Iverson was still around) -
"Notation as a Tool of Thought".

He was also intimate with Fortran and other with rich array heritages.
DMR spent some months experimenting with array semantics before hitting
on C's object model with its direct map to memory, maybe the main reason
for C's success and continuing endurance. But C array is a rough cut gem
with sharp edges that do cut.

At some point, C array should be fixed or superseded by a safer builtin.
The a[i...] syntax should be reserved until that time.
```

Summing up
----------

```text
The question of which syntax to use, subscript[] or functional(), wasn't
seen as that important; at least one scientific author of P2128 prefers
(from a 2020 email thread):

   "that either x(i,j,k) or x[i,j,k] work, and I don't care which"

The "bikeshedding" tendancy, for reviews to pick up on details of syntax
rather than overall design issues, has got hold of the mdspan proposal.

P2128 says:

   "While the timeline is aggressive, we think it is important that
    this feature be available for the benefit of mdspan and mdarray"

The 'multidimensional plan' has led to a train of thought; deprecate ->
extend -> reuse, and a fear of missing the train makes critical review
pressing. The value of a variadic subscript operator must be weighed
against its costs.

P2128 should not be rushed through. More review is needed.
```

Span Mechanics
--------------

```text
This is a preliminary sketch of span as a C and C++ language extension.
It covers only static span type though dynamic spans are more important
in APIs and applications.

    typedef T A[N];   // A = T[N]   array of N T (static extent N)
    typedef T S[N);   // S = T[N)   span of N T (static extent N)

We use array-like notation with mismatched parentheses [) to hint at the
array-like nature and language rules. Span, as an array reference type,
acts as a C array in most ways, apart from one crucial fix:

    • Span does not decay like a C array

Span does, however, convert directly to an array which may then decay,
for compatibility with existing code.

For instance, a generic C++ reference initialization 'auto&& x = span'
sees an array and deduces a C++ array reference type such as x : T(&)[N]
and then x can decay if not carefully handled. Potentially, these cases
could be strengthened to deduce span

The removal of span decay raises the 'auto' question:

    How to interpret 'auto x = span' when the array equivalent is
    a 'shallow' copy 'auto p = array' (i.e. decay-copy of pointer)?

If 'auto x = span' is interpreted as a deep array copy then span doesn't
'act as a C array'. And the result is a C array which can further decay.
Also, array copy is a potential performance gotcha which 'auto x = span'
makes all too easy.

The safe, conservative, choice is to disallow and reject 'auto x = span'
as either a shallow copy or a deep copy, and instead promote alternative
ways to copy arrays that bind directly to a span variable.

Array copy semantics, along the lines of C++ proposal P1997, are assumed
as a convenient foundation, though not necessary. Span complements array
copy semantics by providing a safe handle.

Usual array declaration and decay
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    T a[N];           // a : T[N]   array variable, static extent N
    T* p = a;         // p : T*     array decay to pointer
    auto p = a;       // p : T*     array decay to pointer

Array copy semantics (P1997)
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    T c[] = a;        // c : T[N]   array copy, P1997 proposed syntax
  ? auto c[] = a;     // c : T[N]   array copy, ? deduced element type ?

The 'auto c[] = a' syntax  above may not be viable for auto deduction of
the element type, as there may be parsing issues (unconfirmed). However,
the equivalent syntax could work for span binding.

Span binding to array
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    T s[) = a;        // s : T[N)   span binding, reference to array a
    auto s[) = a;     // s : T[N)   span binding, deduced element type

Span does not decay
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    T* p = s;         // compile FAIL, span does not decay to pointer
    T* p = a[);       // compile FAIL, span does not decay to pointer
    auto* p = s;      // compile FAIL, span does not decay to pointer

    auto p = a;       // p : T*     array decay to pointer, as at top
    auto c = s;       // compile FAIL, span does not decay to pointer
    auto c = a[);     // compile FAIL, span does not decay to pointer

Span -> array copy (P1997)
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    T c[] = s;        // c : T[N]   array copy, from span; same as array

    auto c = s;       // compile FAIL, span does not decay to pointer
                      // i.e. not a shallow or a deep copy, disallowed

Span bind direct to copy (with lifetime extension)
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
    T [b) = a;        // b : T[N)   span binding to a copy of array a
    auto [b) = a;     // b : T[N)   span binding to a copy of array a
    auto [e...] = a;  // C++17 structured binding; copy array, bind ids
                      // to array elements, lifetime-extend

    T b[) = (T[N]){}; // b : T[N)   span binding to array literal
    T b[) = (T[]){a}; // b : T[N)   span binding to copy of array
```
