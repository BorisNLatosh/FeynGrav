(* ::Package:: *)

(*
    CETensor.wl

    This file defines perturbative coefficients for vierbein factors
    multiplied by the volume factor Sqrt[-g].  These coefficients are
    required for the perturbative expansion of the Dirac action in the
    vierbein formalism.

    The file works with two composite geometric factors:

        Sqrt[-g] (e^mu)_m,

    and

        Sqrt[-g] (e^mu)_m (e^nu)_n.

    The public interface consists of two functions:

        CETensorPlain
        CETensor

    Both functions are overloaded by the number of external-index
    arguments.

    ----------------------------------------------------------------------
    1. One-vierbein factor
    ----------------------------------------------------------------------

    The call

        CETensorPlain[{mu, m}, indexArray]

    returns the ordered, non-symmetrised perturbative coefficient

        ( Sqrt[-g] (e^mu)_m )^{rho1 sigma1 ... rhon sigman},

    where

        indexArray = {rho1, sigma1, ..., rhon, sigman}.

    This coefficient is defined by multiplying the perturbative series

        Sqrt[-g] =
            Sum[
                kappa^p C_(p)^{...} h ... h,
                {p, 0, Infinity}
            ]

    and

        (e^mu)_m =
            Sum[
                kappa^q ((e^mu)_m)^{...} h ... h,
                {q, 0, Infinity}
            ].

    Therefore, for n perturbations, the ordered plain coefficient is a
    convolution of the volume-factor coefficient and the inverse-vierbein
    coefficient:

        ( Sqrt[-g] (e^mu)_m )_(n)
            =
        Sum[
            C_(p) * ((e^mu)_m)_(n-p),
            {p, 0, n}
        ].

    The inverse-vierbein coefficient is not treated as an independent
    primitive mixed Kronecker object.  It is reduced to the plain I-tensor
    and the background metric.  Equivalently, it may be viewed as

        (e^mu)_m = g^{mu beta} (e_beta)_m.

    In compact form,

        ((e^mu)_m)^{rho1 sigma1 ... rhon sigman}
            =
        Binomial[-1/2, n]
        eta_{m tau}
        I_(1+n)^{mu tau rho1 sigma1 ... rhon sigman}.

    ----------------------------------------------------------------------
    2. Two-vierbein factor
    ----------------------------------------------------------------------

    The call

        CETensorPlain[{mu, m}, {nu, n}, indexArray]

    returns the ordered, non-symmetrised perturbative coefficient

        ( Sqrt[-g] (e^mu)_m (e^nu)_n )
            ^{rho1 sigma1 ... rhok sigmak},

    where

        indexArray = {rho1, sigma1, ..., rhok, sigmak}.

    This coefficient is defined by multiplying the three perturbative
    series

        Sqrt[-g],
        (e^mu)_m,
        (e^nu)_n.

    Therefore, for k perturbations, the ordered plain coefficient is the
    triple convolution

        ( Sqrt[-g] (e^mu)_m (e^nu)_n )_(k)
            =
        Sum[
            C_(p) * ((e^mu)_m)_(q) * ((e^nu)_n)_(k-p-q),
            {p, 0, k}, {q, 0, k-p}
        ].

    ----------------------------------------------------------------------
    3. Plain and symmetrised tensors
    ----------------------------------------------------------------------

    CETensorPlain always returns ordered coefficients.  It assumes that
    the perturbation indices in indexArray are already ordered and does
    not symmetrise them.

    CETensor returns the corresponding coefficient symmetrised with
    respect to all perturbation index pairs.  The symmetrisation is
    performed only over the indices contained in indexArray.  External
    vierbein indices such as {mu, m} and {nu, n} are kept fixed and are
    not included in the symmetrisation.

    The implemented tensors are therefore plain/symmetrised perturbative
    coefficient generators, not full Feynman rules.  Momentum-space
    symmetrisation over external graviton legs is handled later at the
    rule-generation level.

    ----------------------------------------------------------------------
    4. Implementation conventions
    ----------------------------------------------------------------------

    The implementation uses only FeynCalc Lorentz metrics and the tensor
    generators already present in the Rules directory:

        MTD              for the background Minkowski metric,
        ITensorPlain     for plain I-tensors,
        CTensorPlain     for plain C-tensors,
        indexArraySymmetrization for pair symmetrisation.

    No mixed coordinate--local-Lorentz Kronecker delta is introduced as
    a primitive tensor object.  This avoids ambiguity in FeynCalc, where
    Lorentz-index contractions are represented through metric tensors.

    All exported functions are memoised, because the same perturbative
    coefficients are repeatedly used during the generation of Feynman
    rules.
*)


SetDirectory[DirectoryName[$InputFileName]];


BeginPackage[
    "CETensor`",
    {
        "FeynCalc`",
        "ITensor`",
        "CTensorGeneral`",
        "indexArraySymmetrization`"
    }
];


CETensorPlain::usage =
"CETensorPlain[{\[Mu], m}, {\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}] returns the ordered perturbative \
coefficient of Sqrt[-g] (e^\[Mu])_m.

CETensorPlain[{\[Mu], m}, {nu, n}, {\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}] returns the ordered \
perturbative coefficient of Sqrt[-g] (e^\[Mu])_m (e^\[Nu])_n.

The coefficients are plain ordered coefficients.  No symmetrisation over \
perturbation index pairs is performed.  The external vierbein index pairs \
{mu, m} and {nu, n} are kept fixed.";

CETensor::usage =
"CETensor[{\[Mu], m}, {\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}] returns the perturbative coefficient of \
Sqrt[-g] (e^\[Mu])_m symmetrised over all perturbation index pairs.

CETensor[{\[Mu], m}, {\[Nu], n}, {\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}] returns the perturbative coefficient \
of Sqrt[-g] (e^\[Mu])_m (e^\[Nu])_n symmetrised over all perturbation index pairs.

The list indexArray must have the form {\!\(\*SubscriptBox[\(\[Rho]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Rho]\), \(n\)]\),\!\(\*SubscriptBox[\(\[Sigma]\), \(n\)]\)}. \
The symmetrisation is performed only over the perturbation index pairs \
contained in indexArray, including flips rhoi <-> sigmai inside each pair \
and permutations of the pairs.  The external vierbein index pairs {\[Mu], m} \
and {\[Nu], n} are not included in the symmetrisation.";


Begin["Private`"];


(* Auxiliary index-array tools *)


(*
    CETensorIndexBlock[indexArray, firstPair, numberOfPairs]

    This auxiliary function extracts a consecutive block of perturbation
    index pairs from indexArray.

    Motivation:
    CETensorPlain is built from products of perturbative series.  For
    example, in Sqrt[-g] (e^mu)_m, some ordered perturbation index pairs
    belong to the Sqrt[-g] coefficient, while the remaining pairs belong
    to the inverse-vierbein coefficient.  For
    Sqrt[-g] (e^mu)_m (e^nu)_n, the same ordered index array must be split
    between three factors.  This helper performs that splitting in a
    readable and uniform way.

    The input indexArray is assumed to have the form

        {rho1, sigma1, rho2, sigma2, ..., rhok, sigmak}.

    Since one perturbation index pair occupies two list entries, the pair
    number firstPair is converted to the list position 2 firstPair + 1,
    and numberOfPairs is converted to the corresponding endpoint
    2 (firstPair + numberOfPairs).  Pair counting starts from zero.

    If numberOfPairs is zero, the function returns the empty list {}.
    This represents the zeroth-order perturbative coefficient, which
    carries no perturbation indices.
*)


Clear[CETensorIndexBlock];

CETensorIndexBlock[indexArray_List, firstPair_Integer, numberOfPairs_Integer] :=
    If[
        numberOfPairs == 0,
        {},
        Take[
            indexArray,
            {
                2*firstPair + 1,
                2*(firstPair + numberOfPairs)
            }
        ]
    ];


(* Inverse vierbein coefficient *)


Clear[EInverseTensorPlain];

(*
    EInverseTensorPlain[{mu, m}, indexArrayInternal]

    Ordered coefficient of the inverse vierbein

        ((e^mu)_m)^{rho1 sigma1 ... rhon sigman}
            =
        Binomial[-1/2, n]
        eta_{m tau}
        I_(1+n)^{mu tau rho1 sigma1 ... rhon sigman}.

    This is a private auxiliary function.  It is not exported.
*)

EInverseTensorPlain[indexArrayExternal : {_, _}, indexArrayInternal_List] :=
    EInverseTensorPlain[indexArrayExternal, indexArrayInternal] =
        Module[{mu, m, tau, numberOfPairs},

            {mu, m} = indexArrayExternal;
            numberOfPairs = Length[indexArrayInternal]/2;

            tau = Unique["tau"];

            Expand[
                Contract[
                    Binomial[-1/2, numberOfPairs] *
                    MTD[m, tau] *
                    ITensorPlain[
                        Join[
                            {mu, tau},
                            indexArrayInternal
                        ]
                    ]
                ]
            ]
        ];


(* CETensorPlain *)


Clear[CETensorPlain];

(*
    CETensorPlain[{mu, m}, indexArrayInternal]

    Ordered coefficient of

        Sqrt[-g] (e^mu)_m.

    The perturbation indices are distributed between Sqrt[-g] and
    (e^mu)_m in all ordered ways.
*)

CETensorPlain[indexArrayExternal : {_, _}, indexArrayInternal_List] :=
    CETensorPlain[indexArrayExternal, indexArrayInternal] =
        Module[{numberOfPairs},

            numberOfPairs = Length[indexArrayInternal]/2;

            Expand[
                Sum[
                    CTensorPlainGeneral[
                        {},
                        CETensorIndexBlock[indexArrayInternal, 0, p]
                    ] *
                    EInverseTensorPlain[
                        indexArrayExternal,
                        CETensorIndexBlock[indexArrayInternal, p, numberOfPairs - p]
                    ],
                    {p, 0, numberOfPairs}
                ]
            ]
        ];


(*
    CETensorPlain[{mu, m}, {nu, n}, indexArrayInternal]

    Ordered coefficient of

        Sqrt[-g] (e^mu)_m (e^nu)_n.

    The perturbation indices are distributed between Sqrt[-g],
    (e^mu)_m, and (e^nu)_n in all ordered ways.
*)

CETensorPlain[
    indexArrayExternal1 : {_, _},
    indexArrayExternal2 : {_, _},
    indexArrayInternal_List
] :=
    CETensorPlain[indexArrayExternal1, indexArrayExternal2, indexArrayInternal] =
        Module[{numberOfPairs},

            numberOfPairs = Length[indexArrayInternal]/2;

            Expand[
                Sum[
                    CTensorPlainGeneral[
                        {},
                        CETensorIndexBlock[indexArrayInternal, 0, p]
                    ] *
                    EInverseTensorPlain[
                        indexArrayExternal1,
                        CETensorIndexBlock[indexArrayInternal, p, q]
                    ] *
                    EInverseTensorPlain[
                        indexArrayExternal2,
                        CETensorIndexBlock[
                            indexArrayInternal,
                            p + q,
                            numberOfPairs - p - q
                        ]
                    ],
                    {p, 0, numberOfPairs},
                    {q, 0, numberOfPairs - p}
                ]
            ]
        ];


(* CETensor *)


Clear[CETensor];

(*
    CETensor[{mu, m}, indexArrayInternal]

    Symmetrised coefficient of

        Sqrt[-g] (e^mu)_m.

    The symmetrisation acts only on the perturbation index pairs.
*)

CETensor[indexArrayExternal : {_, _}, indexArrayInternal_List] :=
    CETensor[indexArrayExternal, indexArrayInternal] =
        Module[{numberOfPairs},

            numberOfPairs = Length[indexArrayInternal]/2;

            If[
                indexArrayInternal === {},

                CETensorPlain[indexArrayExternal, indexArrayInternal],

                Expand[
                    1/Power[2, numberOfPairs] *
                    1/Factorial[numberOfPairs] *
                    Total[
                        Map[
                            CETensorPlain[indexArrayExternal, #] &,
                            indexArraySymmetrization[indexArrayInternal]
                        ]
                    ]
                ]
            ]
        ];


(*
    CETensor[{mu, m}, {nu, n}, indexArrayInternal]

    Symmetrised coefficient of

        Sqrt[-g] (e^mu)_m (e^nu)_n.

    The symmetrisation acts only on the perturbation index pairs.
    The external vierbein index pairs are kept fixed.
*)

CETensor[
    indexArrayExternal1 : {_, _},
    indexArrayExternal2 : {_, _},
    indexArrayInternal_List
] :=
    CETensor[indexArrayExternal1, indexArrayExternal2, indexArrayInternal] =
        Module[{numberOfPairs},

            numberOfPairs = Length[indexArrayInternal]/2;

            If[
                indexArrayInternal === {},

                CETensorPlain[
                    indexArrayExternal1,
                    indexArrayExternal2,
                    indexArrayInternal
                ],

                Expand[
                    1/Power[2, numberOfPairs] *
                    1/Factorial[numberOfPairs] *
                    Total[
                        Map[
                            CETensorPlain[indexArrayExternal1, indexArrayExternal2, #] &,
                            indexArraySymmetrization[indexArrayInternal]
                        ]
                    ]
                ]
            ]
        ];


End[];


EndPackage[];
