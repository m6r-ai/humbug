"""Main AIFPL (AI Functional Programming Language) class with enhanced error messages."""

import hashlib
import math
from pathlib import Path
import os
from typing import Union, Dict, List, Iterator
from contextlib import contextmanager

from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_ast import AIFPLASTNode
from aifpl.aifpl_value import AIFPLFunction, AIFPLFloat, AIFPLValue
from aifpl.aifpl_vm import AIFPLVM, AIFPLTraceWatcher
from aifpl.aifpl_error import AIFPLModuleNotFoundError, AIFPLModuleError, AIFPLCircularImportError


class AIFPL:
    """
    AIFPL (AI Functional Programming Language) calculator with LISP-like syntax and enhanced error messages.

    This version provides comprehensive error reporting with:
    - Clear explanations of what went wrong
    - Context showing the problematic input
    - Suggestions for how to fix the problem
    - Examples of correct usage
    - Position information where helpful

    Designed specifically to help LLMs understand and self-correct errors.

    Execution Model:
    - Uses bytecode ir_builder and VM for all evaluation
    - Tail-call optimized for recursive functions
    - High performance through bytecode compilation and optimized VM
    """

    # AIFPL implementations of higher-order functions
    _PRELUDE_SOURCE = {
        'boolean=?': """(lambda (. args)
                          (if (integer<? (list-length args) 2)
                            (error "Function 'boolean=?' requires at least 2 arguments")
                            (letrec ((loop (lambda (lst prev)
                                             (if (list-null? lst) #t
                                                 (if (boolean=? prev (list-first lst))
                                                     (loop (list-rest lst) (list-first lst))
                                                     #f)))))
                              (loop (list-rest args) (list-first args)))))""",
        'boolean!=?': """(lambda (. args)
                            (if (integer<? (list-length args) 2)
                              (error "Function 'boolean!=?' requires at least 2 arguments")
                              (letrec ((outer (lambda (lst)
                                                (if (list-null? lst) #t
                                                    (letrec ((inner (lambda (rest-lst)
                                                                      (if (list-null? rest-lst)
                                                                          (outer (list-rest lst))
                                                                          (if (boolean!=? (list-first lst) (list-first rest-lst))
                                                                              (inner (list-rest rest-lst))
                                                                              #f)))))
                                                      (inner (list-rest lst)))))))
                                (outer args))))""",
        'integer=?': """(lambda (. args)
                          (if (integer<? (list-length args) 2)
                            (error "Function 'integer=?' requires at least 2 arguments")
                            (letrec ((loop (lambda (lst prev)
                                             (if (list-null? lst) #t
                                                 (if (integer=? prev (list-first lst))
                                                     (loop (list-rest lst) (list-first lst))
                                                     #f)))))
                              (loop (list-rest args) (list-first args)))))""",
        'integer!=?': """(lambda (. args)
                            (if (integer<? (list-length args) 2)
                              (error "Function 'integer!=?' requires at least 2 arguments")
                              (letrec ((outer (lambda (lst)
                                                (if (list-null? lst) #t
                                                    (letrec ((inner (lambda (rest-lst)
                                                                      (if (list-null? rest-lst)
                                                                          (outer (list-rest lst))
                                                                          (if (integer!=? (list-first lst) (list-first rest-lst))
                                                                              (inner (list-rest rest-lst))
                                                                              #f)))))
                                                      (inner (list-rest lst)))))))
                                (outer args))))""",
        'integer<?': """(lambda (. args)
                          (if (integer<? (list-length args) 2)
                            (error "Function 'integer<?' requires at least 2 arguments")
                            (letrec ((loop (lambda (lst prev)
                                             (if (list-null? lst) #t
                                                 (if (integer<? prev (list-first lst))
                                                     (loop (list-rest lst) (list-first lst))
                                                     #f)))))
                              (loop (list-rest args) (list-first args)))))""",
        'integer>?': """(lambda (. args)
                          (if (integer<? (list-length args) 2)
                            (error "Function 'integer>?' requires at least 2 arguments")
                            (letrec ((loop (lambda (lst prev)
                                             (if (list-null? lst) #t
                                                 (if (integer>? prev (list-first lst))
                                                     (loop (list-rest lst) (list-first lst))
                                                     #f)))))
                              (loop (list-rest args) (list-first args)))))""",
        'integer<=?': """(lambda (. args)
                           (if (integer<? (list-length args) 2)
                             (error "Function 'integer<=?' requires at least 2 arguments")
                             (letrec ((loop (lambda (lst prev)
                                              (if (list-null? lst) #t
                                                  (if (integer<=? prev (list-first lst))
                                                      (loop (list-rest lst) (list-first lst))
                                                      #f)))))
                               (loop (list-rest args) (list-first args)))))""",
        'integer>=?': """(lambda (. args)
                           (if (integer<? (list-length args) 2)
                             (error "Function 'integer>=?' requires at least 2 arguments")
                             (letrec ((loop (lambda (lst prev)
                                              (if (list-null? lst) #t
                                                  (if (integer>=? prev (list-first lst))
                                                      (loop (list-rest lst) (list-first lst))
                                                      #f)))))
                               (loop (list-rest args) (list-first args)))))""",
        'integer+': """(lambda (. args)
                         (if (list-null? args) 0
                           (letrec ((loop (lambda (lst acc)
                                            (if (list-null? lst) acc
                                                (loop (list-rest lst) (integer+ acc (list-first lst)))))))
                             (loop (list-rest args) (list-first args)))))""",
        'integer-': """(lambda (. args)
                         (if (integer<? (list-length args) 2)
                           (error "Function 'integer-' requires at least 2 arguments")
                           (letrec ((loop (lambda (lst acc)
                                            (if (list-null? lst) acc
                                                (loop (list-rest lst) (integer- acc (list-first lst)))))))
                             (loop (list-rest args) (list-first args)))))""",
        'integer*': """(lambda (. args)
                         (if (list-null? args) 1
                           (letrec ((loop (lambda (lst acc)
                                            (if (list-null? lst) acc
                                                (loop (list-rest lst) (integer* acc (list-first lst)))))))
                             (loop (list-rest args) (list-first args)))))""",
        'integer/': """(lambda (. args)
                         (if (integer<? (list-length args) 2)
                           (error "Function 'integer/' requires at least 2 arguments")
                           (letrec ((loop (lambda (lst acc)
                                            (if (list-null? lst) acc
                                                (loop (list-rest lst) (integer/ acc (list-first lst)))))))
                             (loop (list-rest args) (list-first args)))))""",
        'integer-bit-or': """(lambda (. args)
                       (if (integer<? (list-length args) 2)
                         (error "Function 'integer-bit-or' requires at least 2 arguments")
                         (letrec ((loop (lambda (lst acc)
                                          (if (list-null? lst) acc
                                              (loop (list-rest lst) (integer-bit-or acc (list-first lst)))))))
                           (loop (list-rest args) (list-first args)))))""",
        'integer-bit-and': """(lambda (. args)
                       (if (integer<? (list-length args) 2)
                         (error "Function 'integer-bit-and' requires at least 2 arguments")
                         (letrec ((loop (lambda (lst acc)
                                          (if (list-null? lst) acc
                                              (loop (list-rest lst) (integer-bit-and acc (list-first lst)))))))
                           (loop (list-rest args) (list-first args)))))""",
        'integer-bit-xor': """(lambda (. args)
                       (if (integer<? (list-length args) 2)
                         (error "Function 'integer-bit-xor' requires at least 2 arguments")
                         (letrec ((loop (lambda (lst acc)
                                          (if (list-null? lst) acc
                                              (loop (list-rest lst) (integer-bit-xor acc (list-first lst)))))))
                           (loop (list-rest args) (list-first args)))))""",
        'integer-min': """(lambda (. args)
                             (if (list-null? args)
                               (error "Function 'integer-min' requires at least 1 argument")
                               (letrec ((loop (lambda (lst acc)
                                                (if (list-null? lst) acc
                                                    (loop (list-rest lst) (integer-min acc (list-first lst)))))))
                                 (loop (list-rest args) (list-first args)))))""",
        'integer-max': """(lambda (. args)
                             (if (list-null? args)
                               (error "Function 'integer-max' requires at least 1 argument")
                               (letrec ((loop (lambda (lst acc)
                                                (if (list-null? lst) acc
                                                    (loop (list-rest lst) (integer-max acc (list-first lst)))))))
                                 (loop (list-rest args) (list-first args)))))""",
        'integer->complex': """(lambda (real . rest)
                                 (integer->complex real (if (list-null? rest) 0 (list-first rest))))""",
        'integer->string': """(lambda (n . rest)
                                 (if (list-null? rest)
                                   (integer->string n 10)
                                   (integer->string n (list-first rest))))""",
        'float=?': """(lambda (. args)
                        (if (integer<? (list-length args) 2)
                          (error "Function 'float=?' requires at least 2 arguments")
                          (letrec ((loop (lambda (lst prev)
                                           (if (list-null? lst) #t
                                               (if (float=? prev (list-first lst))
                                                   (loop (list-rest lst) (list-first lst))
                                                   #f)))))
                            (loop (list-rest args) (list-first args)))))""",
        'float!=?': """(lambda (. args)
                            (if (integer<? (list-length args) 2)
                              (error "Function 'float!=?' requires at least 2 arguments")
                              (letrec ((outer (lambda (lst)
                                                (if (list-null? lst) #t
                                                    (letrec ((inner (lambda (rest-lst)
                                                                      (if (list-null? rest-lst)
                                                                          (outer (list-rest lst))
                                                                          (if (float!=? (list-first lst) (list-first rest-lst))
                                                                              (inner (list-rest rest-lst))
                                                                              #f)))))
                                                      (inner (list-rest lst)))))))
                                (outer args))))""",
        'float<?': """(lambda (. args)
                        (if (integer<? (list-length args) 2)
                          (error "Function 'float<?' requires at least 2 arguments")
                          (letrec ((loop (lambda (lst prev)
                                           (if (list-null? lst) #t
                                               (if (float<? prev (list-first lst))
                                                   (loop (list-rest lst) (list-first lst))
                                                   #f)))))
                            (loop (list-rest args) (list-first args)))))""",
        'float>?': """(lambda (. args)
                        (if (integer<? (list-length args) 2)
                          (error "Function 'float>?' requires at least 2 arguments")
                          (letrec ((loop (lambda (lst prev)
                                           (if (list-null? lst) #t
                                               (if (float>? prev (list-first lst))
                                                   (loop (list-rest lst) (list-first lst))
                                                   #f)))))
                            (loop (list-rest args) (list-first args)))))""",
        'float<=?': """(lambda (. args)
                         (if (integer<? (list-length args) 2)
                           (error "Function 'float<=?' requires at least 2 arguments")
                           (letrec ((loop (lambda (lst prev)
                                            (if (list-null? lst) #t
                                                (if (float<=? prev (list-first lst))
                                                    (loop (list-rest lst) (list-first lst))
                                                    #f)))))
                             (loop (list-rest args) (list-first args)))))""",
        'float>=?': """(lambda (. args)
                         (if (integer<? (list-length args) 2)
                           (error "Function 'float>=?' requires at least 2 arguments")
                           (letrec ((loop (lambda (lst prev)
                                            (if (list-null? lst) #t
                                                (if (float>=? prev (list-first lst))
                                                    (loop (list-rest lst) (list-first lst))
                                                    #f)))))
                             (loop (list-rest args) (list-first args)))))""",
        'float+': """(lambda (. args)
                       (if (list-null? args) 0.0
                         (letrec ((loop (lambda (lst acc)
                                          (if (list-null? lst) acc
                                              (loop (list-rest lst) (float+ acc (list-first lst)))))))
                           (loop (list-rest args) (list-first args)))))""",
        'float-': """(lambda (. args)
                       (if (integer<? (list-length args) 2)
                         (error "Function 'float-' requires at least 2 arguments")
                         (letrec ((loop (lambda (lst acc)
                                          (if (list-null? lst) acc
                                              (loop (list-rest lst) (float- acc (list-first lst)))))))
                           (loop (list-rest args) (list-first args)))))""",
        'float*': """(lambda (. args)
                       (if (list-null? args) 1.0
                         (letrec ((loop (lambda (lst acc)
                                          (if (list-null? lst) acc
                                              (loop (list-rest lst) (float* acc (list-first lst)))))))
                           (loop (list-rest args) (list-first args)))))""",
        'float/': """(lambda (. args)
                       (if (integer<? (list-length args) 2)
                         (error "Function 'float/' requires at least 2 arguments")
                         (letrec ((loop (lambda (lst acc)
                                          (if (list-null? lst) acc
                                              (loop (list-rest lst) (float/ acc (list-first lst)))))))
                           (loop (list-rest args) (list-first args)))))""",
        'float-expt': """(lambda (. args)
                           (if (integer<? (list-length args) 2)
                             (error "Function 'float-expt' requires at least 2 arguments")
                             (letrec ((loop (lambda (lst acc)
                                              (if (list-null? lst) acc
                                                  (loop (list-rest lst) (float-expt acc (list-first lst)))))))
                               (loop (list-rest args) (list-first args)))))""",
        'float-min': """(lambda (. args)
                           (if (list-null? args)
                             (error "Function 'float-min' requires at least 1 argument")
                             (letrec ((loop (lambda (lst acc)
                                              (if (list-null? lst) acc
                                                  (loop (list-rest lst) (float-min acc (list-first lst)))))))
                               (loop (list-rest args) (list-first args)))))""",
        'float-max': """(lambda (. args)
                           (if (list-null? args)
                             (error "Function 'float-max' requires at least 1 argument")
                             (letrec ((loop (lambda (lst acc)
                                              (if (list-null? lst) acc
                                                  (loop (list-rest lst) (float-max acc (list-first lst)))))))
                               (loop (list-rest args) (list-first args)))))""",
        'float->complex': """(lambda (real . rest)
                               (float->complex real (if (list-null? rest) 0 (list-first rest))))""",
        'complex=?': """(lambda (. args)
                          (if (integer<? (list-length args) 2)
                            (error "Function 'complex=?' requires at least 2 arguments")
                            (letrec ((loop (lambda (lst prev)
                                             (if (list-null? lst) #t
                                                 (if (complex=? prev (list-first lst))
                                                     (loop (list-rest lst) (list-first lst))
                                                     #f)))))
                              (loop (list-rest args) (list-first args)))))""",
        'complex!=?': """(lambda (. args)
                            (if (integer<? (list-length args) 2)
                              (error "Function 'complex!=?' requires at least 2 arguments")
                              (letrec ((outer (lambda (lst)
                                                (if (list-null? lst) #t
                                                    (letrec ((inner (lambda (rest-lst)
                                                                      (if (list-null? rest-lst)
                                                                          (outer (list-rest lst))
                                                                          (if (complex!=? (list-first lst) (list-first rest-lst))
                                                                              (inner (list-rest rest-lst))
                                                                              #f)))))
                                                      (inner (list-rest lst)))))))
                                (outer args))))""",
        'complex+': """(lambda (. args)
                         (if (list-null? args)
                           (error "Function 'complex+' requires at least 1 argument, got 0")
                           (letrec ((loop (lambda (lst acc)
                                            (if (list-null? lst) acc
                                                (loop (list-rest lst) (complex+ acc (list-first lst)))))))
                             (loop (list-rest args) (list-first args)))))""",
        'complex-': """(lambda (. args)
                         (if (integer<? (list-length args) 2)
                           (error "Function 'complex-' requires at least 2 arguments")
                           (letrec ((loop (lambda (lst acc)
                                            (if (list-null? lst) acc
                                                (loop (list-rest lst) (complex- acc (list-first lst)))))))
                             (loop (list-rest args) (list-first args)))))""",
        'complex*': """(lambda (. args)
                         (if (list-null? args)
                           (error "Function 'complex*' requires at least 1 argument, got 0")
                           (letrec ((loop (lambda (lst acc)
                                            (if (list-null? lst) acc
                                                (loop (list-rest lst) (complex* acc (list-first lst)))))))
                             (loop (list-rest args) (list-first args)))))""",
        'complex/': """(lambda (. args)
                         (if (integer<? (list-length args) 2)
                           (error "Function 'complex/' requires at least 2 arguments")
                           (letrec ((loop (lambda (lst acc)
                                            (if (list-null? lst) acc
                                                (loop (list-rest lst) (complex/ acc (list-first lst)))))))
                             (loop (list-rest args) (list-first args)))))""",
        'complex-expt': """(lambda (. args)
                             (if (integer<? (list-length args) 2)
                               (error "Function 'complex-expt' requires at least 2 arguments")
                               (letrec ((loop (lambda (lst acc)
                                                (if (list-null? lst) acc
                                                    (loop (list-rest lst) (complex-expt acc (list-first lst)))))))
                                 (loop (list-rest args) (list-first args)))))""",
        'string=?': """(lambda (. args)
                         (if (integer<? (list-length args) 2)
                           (error "Function 'string=?' requires at least 2 arguments")
                           (letrec ((loop (lambda (lst prev)
                                            (if (list-null? lst) #t
                                                (if (string=? prev (list-first lst))
                                                    (loop (list-rest lst) (list-first lst))
                                                    #f)))))
                             (loop (list-rest args) (list-first args)))))""",
        'string!=?': """(lambda (. args)
                            (if (integer<? (list-length args) 2)
                              (error "Function 'string!=?' requires at least 2 arguments")
                              (letrec ((outer (lambda (lst)
                                                (if (list-null? lst) #t
                                                    (letrec ((inner (lambda (rest-lst)
                                                                      (if (list-null? rest-lst)
                                                                          (outer (list-rest lst))
                                                                          (if (string!=? (list-first lst) (list-first rest-lst))
                                                                              (inner (list-rest rest-lst))
                                                                              #f)))))
                                                      (inner (list-rest lst)))))))
                                (outer args))))""",
        'string<?': """(lambda (. args)
                         (if (integer<? (list-length args) 2)
                           (error "Function 'string<?' requires at least 2 arguments")
                           (letrec ((loop (lambda (lst prev)
                                            (if (list-null? lst) #t
                                                (if (string<? prev (list-first lst))
                                                    (loop (list-rest lst) (list-first lst))
                                                    #f)))))
                             (loop (list-rest args) (list-first args)))))""",
        'string>?': """(lambda (. args)
                         (if (integer<? (list-length args) 2)
                           (error "Function 'string>?' requires at least 2 arguments")
                           (letrec ((loop (lambda (lst prev)
                                            (if (list-null? lst) #t
                                                (if (string>? prev (list-first lst))
                                                    (loop (list-rest lst) (list-first lst))
                                                    #f)))))
                             (loop (list-rest args) (list-first args)))))""",
        'string<=?': """(lambda (. args)
                          (if (integer<? (list-length args) 2)
                            (error "Function 'string<=?' requires at least 2 arguments")
                            (letrec ((loop (lambda (lst prev)
                                             (if (list-null? lst) #t
                                                 (if (string<=? prev (list-first lst))
                                                     (loop (list-rest lst) (list-first lst))
                                                     #f)))))
                              (loop (list-rest args) (list-first args)))))""",
        'string>=?': """(lambda (. args)
                          (if (integer<? (list-length args) 2)
                            (error "Function 'string>=?' requires at least 2 arguments")
                            (letrec ((loop (lambda (lst prev)
                                             (if (list-null? lst) #t
                                                 (if (string>=? prev (list-first lst))
                                                     (loop (list-rest lst) (list-first lst))
                                                     #f)))))
                              (loop (list-rest args) (list-first args)))))""",
        'string-concat': """(lambda (. args)
                              (if (list-null? args) ""
                                (letrec ((loop (lambda (lst acc)
                                                 (if (list-null? lst) acc
                                                     (loop (list-rest lst) (string-concat acc (list-first lst)))))))
                                  (loop (list-rest args) (list-first args)))))""",
        'string-slice': """(lambda (str start . rest)
                             (if (list-null? rest)
                                 (string-slice str start (string-length str))
                                 (string-slice str start (list-first rest))))""",
        'string->list': """(lambda (str . rest)
                             (string->list str (if (list-null? rest) "" (list-first rest))))""",
        'list': """(lambda (. args) args)""",
        'list=?': """(lambda (. args)
                       (if (integer<? (list-length args) 2)
                         (error "Function 'list=?' requires at least 2 arguments")
                         (letrec ((loop (lambda (lst prev)
                                          (if (list-null? lst) #t
                                              (if (list=? prev (list-first lst))
                                                  (loop (list-rest lst) (list-first lst))
                                                  #f)))))
                           (loop (list-rest args) (list-first args)))))""",
        'list!=?': """(lambda (. args)
                            (if (integer<? (list-length args) 2)
                              (error "Function 'list!=?' requires at least 2 arguments")
                              (letrec ((outer (lambda (lst)
                                                (if (list-null? lst) #t
                                                    (letrec ((inner (lambda (rest-lst)
                                                                      (if (list-null? rest-lst)
                                                                          (outer (list-rest lst))
                                                                          (if (list!=? (list-first lst) (list-first rest-lst))
                                                                              (inner (list-rest rest-lst))
                                                                              #f)))))
                                                      (inner (list-rest lst)))))))
                                (outer args))))""",
        'list-concat': """(lambda (. args)
                            (if (list-null? args) (list)
                              (letrec ((loop (lambda (lst acc)
                                               (if (list-null? lst) acc
                                                   (loop (list-rest lst) (list-concat acc (list-first lst)))))))
                                (loop (list-rest args) (list-first args)))))""",
        'list-slice': """(lambda (lst start . rest)
                             (if (list-null? rest)
                                 (list-slice lst start (list-length lst))
                                 (list-slice lst start (list-first rest))))""",
        'list->string': """(lambda (lst . rest)
                             (list->string lst (if (list-null? rest) "" (list-first rest))))""",
        'alist': """(lambda (. args)
                      (letrec ((loop (lambda (pairs acc)
                                       (if (list-null? pairs) acc
                                           (if (boolean-not (list? (list-first pairs)))
                                               (error "alist: each argument must be a 2-element list")
                                               (if (!= (list-length (list-first pairs)) 2)
                                                   (error "alist: each argument must be a 2-element list")
                                                   (loop (list-rest pairs)
                                                         (alist-set acc
                                                                    (list-first (list-first pairs))
                                                                    (list-first (list-rest (list-first pairs)))))))))))
                        (loop args (alist))))""",
        'alist=?': """(lambda (. args)
                        (if (integer<? (list-length args) 2)
                          (error "Function 'alist=?' requires at least 2 arguments")
                          (letrec ((loop (lambda (lst prev)
                                           (if (list-null? lst) #t
                                               (if (alist=? prev (list-first lst))
                                                   (loop (list-rest lst) (list-first lst))
                                                   #f)))))
                            (loop (list-rest args) (list-first args)))))""",
        'alist!=?': """(lambda (. args)
                            (if (integer<? (list-length args) 2)
                              (error "Function 'alist!=?' requires at least 2 arguments")
                              (letrec ((outer (lambda (lst)
                                                (if (list-null? lst) #t
                                                    (letrec ((inner (lambda (rest-lst)
                                                                      (if (list-null? rest-lst)
                                                                          (outer (list-rest lst))
                                                                          (if (alist!=? (list-first lst) (list-first rest-lst))
                                                                              (inner (list-rest rest-lst))
                                                                              #f)))))
                                                      (inner (list-rest lst)))))))
                                (outer args))))""",
        'alist-get': """(lambda (a-list key . rest)
                          (alist-get a-list key (if (list-null? rest) #f (list-first rest))))""",
        'range': """(lambda (start end . rest)
                      (range start end (if (list-null? rest) 1 (list-first rest))))""",
        'map': """(lambda (f lst)
                    (letrec ((helper (lambda (f lst acc)
                                       (if (list-null? lst) (list-reverse acc)
                                           (helper f (list-rest lst) (list-prepend acc (f (list-first lst))))))))
                    (helper f lst (list))))""",
        'filter': """(lambda (pred lst)
                    (letrec ((helper (lambda (pred lst acc)
                                       (if (list-null? lst) (list-reverse acc)
                                           (if (pred (list-first lst))
                                               (helper pred (list-rest lst) (list-prepend acc (list-first lst)))
                                               (helper pred (list-rest lst) acc))))))
                        (helper pred lst (list))))""",
        'fold': """(lambda (f init lst)
                    (letrec ((helper (lambda (f acc lst)
                                       (if (list-null? lst) acc
                                           (helper f (f acc (list-first lst)) (list-rest lst))))))
                    (helper f init lst)))""",
        'find': """(lambda (pred lst)
                    (letrec ((find (lambda (pred lst) (if (list-null? lst) #f (if (pred (list-first lst)) (list-first lst) (find pred (list-rest lst)))))))
                    (find pred lst)))""",
        'any?': """(lambda (pred lst)
                    (letrec ((any? (lambda (pred lst) (if (list-null? lst) #f (if (pred (list-first lst)) #t (any? pred (list-rest lst)))))))
                    (any? pred lst)))""",
        'all?': """(lambda (pred lst)
                    (letrec ((all? (lambda (pred lst) (if (list-null? lst) #t (if (pred (list-first lst)) (all? pred (list-rest lst)) #f)))))
                    (all? pred lst)))""",
    }

    # Mathematical constants
    CONSTANTS: Dict[str, AIFPLValue] = {
        'pi': AIFPLFloat(math.pi),
        'e': AIFPLFloat(math.e),
    }

    # Class-level cache for prelude functions
    _prelude_cache = None

    @classmethod
    def _load_prelude(
        cls,
        compiler: AIFPLCompiler,
        vm: AIFPLVM
    ) -> Dict[str, AIFPLFunction]:
        """Load prelude as bytecode AIFPLFunction objects (cached)."""
        if cls._prelude_cache is not None:
            return cls._prelude_cache

        bytecode_prelude: dict[str, AIFPLFunction] = {}
        for name, source_code in cls._PRELUDE_SOURCE.items():
            bytecode = compiler.compile(source_code, name=f"<prelude:{name}>")
            func = vm.execute(bytecode, cls.CONSTANTS, {})
            if isinstance(func, AIFPLFunction):
                bytecode_prelude[name] = func

        cls._prelude_cache = bytecode_prelude
        return bytecode_prelude

    def __init__(self, module_path: List[str] | None = None):
        """
        Initialize AIFPL calculator.

        Args:
            module_path: List of directories to search for modules (default: ["."])
        """
        self._module_path = module_path or ["."]

        # Module system state
        self.module_cache: Dict[str, AIFPLASTNode] = {}  # module_name -> alist
        self.module_hashes: Dict[str, str] = {}  # module_name -> sha256 hex digest
        self.loading_stack: List[str] = []  # Track currently-loading modules for circular detection

        # Compiler and VM
        self.compiler = AIFPLCompiler(module_loader=self)
        self.vm = AIFPLVM()

        # Load prelude once at initialization
        self._prelude = self._load_prelude(self.compiler, self.vm)

    def _evaluate_raw(self, expression: str) -> 'AIFPLValue':
        """
        Evaluate an AIFPL expression without error handling.

        Args:
            expression: AIFPL expression string to evaluate

        Returns:
            The result of evaluating the expression as AIFPLValue
        """
        # Compile (lexing, parsing, semantic analysis, IR building, code generation)
        code = self.compiler.compile(expression)

        # Execute
        result = self.vm.execute(code, self.CONSTANTS, self._prelude)
        return result

    def evaluate(self, expression: str) -> Union[int, float, complex, str, bool, list, AIFPLFunction]:
        """
        Evaluate an AIFPL expression with comprehensive enhanced error reporting.

        Args:
            expression: AIFPL expression string to evaluate

        Returns:
            The result of evaluating the expression converted to Python types

        Raises:
            AIFPLTokenError: If tokenization fails (with detailed context and suggestions)
            AIFPLParseError: If parsing fails (with detailed context and suggestions)
            AIFPLEvalError: If evaluation fails (with detailed context and suggestions)
        """
        result = self._evaluate_raw(expression)
        return result.to_python()

    def evaluate_and_format(self, expression: str) -> str:
        """
        Evaluate an AIFPL expression and return formatted result with comprehensive enhanced error reporting.

        Args:
            expression: AIFPL expression string to evaluate

        Returns:
            String representation of the result using LISP conventions

        Raises:
            AIFPLTokenError: If tokenization fails (with detailed context and suggestions)
            AIFPLParseError: If parsing fails (with detailed context and suggestions)
            AIFPLEvalError: If evaluation fails (with detailed context and suggestions)
        """
        result = self._evaluate_raw(expression)
        return result.describe()

    # Module System Implementation (ModuleLoader interface)

    @contextmanager
    def begin_loading(self, module_name: str) -> Iterator[None]:
        """
        Begin loading a module with circular import detection.

        This context manager tracks the module in the loading stack and
        automatically cleans up when exiting (even on exception).

        Args:
            module_name: Name of module being loaded

        Yields:
            None

        Raises:
            AIFPLCircularImportError: If this module is already being loaded
        """
        # Check for circular dependency BEFORE adding to stack
        if module_name in self.loading_stack:
            cycle = self.loading_stack + [module_name]
            raise AIFPLCircularImportError(import_chain=cycle)

        # Add to loading stack
        self.loading_stack.append(module_name)
        try:
            yield

        finally:
            # Always remove from stack, even if loading fails
            self.loading_stack.pop()

    def _compute_file_hash(self, file_path: str) -> str:
        """
        Compute SHA256 hash of file content.

        Uses chunked reading for memory efficiency with large files.

        Args:
            file_path: Path to file to hash

        Returns:
            SHA256 hash as hex string
        """
        hasher = hashlib.sha256()
        with open(file_path, 'rb') as f:
            while chunk := f.read(8192):
                hasher.update(chunk)

        return hasher.hexdigest()

    def resolve_module(self, module_name: str) -> str:
        """
        Find module file in search path.

        Security: Module names must not use absolute or relative path navigation.
        Only simple names (e.g., "calendar") or subdirectory paths (e.g., "lib/validation")
        are allowed. This prevents escaping the configured module directories.

        Args:
            module_name: Name like "calendar" or "lib/validation"

        Returns:
            Full path to module file

        Raises:
            AIFPLModuleNotFoundError: If module not found in search path
            AIFPLModuleError: If module name contains invalid path components
        """
        # Reject absolute paths
        if module_name.startswith('/') or (os.sep != '/' and module_name.startswith(os.sep)):
            raise AIFPLModuleError(
                message=f"Absolute module paths are not allowed: '{module_name}'",
                context="Module names must be relative to the module search path",
                suggestion="Use a simple module name like 'calendar' or 'lib/validation'"
            )

        # Reject relative path navigation (. or ..)
        if module_name.startswith('./') or module_name.startswith('../') or '/./' in module_name or '/../' in module_name:
            raise AIFPLModuleError(
                message=f"Relative path navigation is not allowed in module names: '{module_name}'",
                context="Module names must not contain './' or '../' path components",
                suggestion="Use a simple module name like 'calendar' or 'lib/validation'"
            )

        # Search for module in configured paths
        for directory in self._module_path:
            module_path = Path(directory) / f"{module_name}.aifpl"
            if module_path.exists():
                return str(module_path)

        raise AIFPLModuleNotFoundError(
            module_name=module_name,
            search_paths=self._module_path
        )

    def load_module(self, module_name: str) -> AIFPLASTNode:
        """
        Load and compile a module to a fully resolved AST.

        This implements the ModuleLoader interface. It compiles the module through
        the full front-end pipeline (lex, parse, semantic analysis, module resolution).
        The result is cached for subsequent imports. Cache is automatically invalidated
        when the module file content changes (detected via SHA256 hash).

        Note: Callers should use begin_loading() before calling this method to enable
        circular import detection. The module resolver handles this automatically.

        Args:
            module_name: Name of module to load

        Returns:
            Fully resolved AST of the module (all imports already resolved)

        Raises:
            AIFPLModuleNotFoundError: If module file not found
            AIFPLCircularImportError: If circular dependency detected (via begin_loading)
            AIFPLError: If module compilation fails
        """
        # Resolve to file path
        try:
            module_path = self.resolve_module(module_name)

        except AIFPLModuleNotFoundError:
            # File doesn't exist - clean up any stale cache entries
            self.module_cache.pop(module_name, None)
            self.module_hashes.pop(module_name, None)
            raise

        # Compute current file hash for cache invalidation
        try:
            current_hash = self._compute_file_hash(module_path)

        except OSError as e:
            # File disappeared after resolve - clean up cache and raise
            self.module_cache.pop(module_name, None)
            self.module_hashes.pop(module_name, None)
            raise AIFPLModuleNotFoundError(
                module_name=module_name,
                search_paths=self._module_path
            ) from e

        # Check cache validity using content hash
        if module_name in self.module_cache:
            cached_hash = self.module_hashes.get(module_name)
            if cached_hash == current_hash:
                # Cache is valid - return cached AST
                return self.module_cache[module_name]

            # Cache is stale - will reload below

        # Load source code
        with open(module_path, 'r', encoding='utf-8') as f:
            code = f.read()

        # Compile through the front-end pipeline (lex, parse, analyze, resolve imports)
        # This will recursively handle any imports within this module.
        # The module resolver will call begin_loading() for each nested import,
        # which provides circular import detection.
        # Use module name with .aifpl extension for source_file (relative path)
        resolved_ast = self.compiler.compile_to_resolved_ast(code, f"{module_name}.aifpl")

        # Cache the resolved module and update hash after successful compilation
        self.module_cache[module_name] = resolved_ast
        self.module_hashes[module_name] = current_hash

        return resolved_ast

    def clear_module_cache(self) -> None:
        """Clear the module cache and hashes. Useful for development/testing."""
        self.module_cache.clear()
        self.module_hashes.clear()

    def invalidate_module(self, module_name: str) -> None:
        """
        Invalidate a specific module in the cache, forcing reload on next import.

        Args:
            module_name: Name of module to invalidate (e.g., "calendar" or "lib/validation")
        """
        self.module_cache.pop(module_name, None)
        self.module_hashes.pop(module_name, None)

    def reload_module(self, module_name: str) -> AIFPLASTNode:
        """
        Force reload a module, bypassing cache.

        Args:
            module_name: Name of module to reload

        Returns:
            Fully resolved AST of the reloaded module
        """
        self.invalidate_module(module_name)
        return self.load_module(module_name)

    def set_module_path(self, module_path: List[str]) -> None:
        """
        Set the module search path and clear the module cache.

        This should be called when the base directory changes (e.g., when switching
        mindspaces in Humbug) to ensure modules are loaded from the correct location
        and old cached modules are discarded.

        Args:
            module_path: List of directories to search for modules
        """
        self._module_path = module_path
        # Clear the cache since modules from the old path are no longer valid
        self.clear_module_cache()
        # Also clear the loading stack to ensure clean state
        self.loading_stack.clear()

    def module_path(self) -> List[str]:
        """
        Get the current module search path.

        Returns:
            List of directories in the module search path
        """
        return self._module_path

    def set_trace_watcher(self, watcher: AIFPLTraceWatcher | None) -> None:
        """
        Set the trace watcher for this AIFPL instance.

        The trace watcher receives messages from (trace ...) calls during evaluation.

        Args:
            watcher: AIFPLTraceWatcher instance or None to disable tracing
        """
        self.vm.set_trace_watcher(watcher)
