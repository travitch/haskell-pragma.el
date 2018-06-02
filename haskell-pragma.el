;;; haskell-pragma.el --- A package for inserting Haskell LANGUAGE pragmas    -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Tristan Ravitch

;; Author: Tristan Ravitch <tristan.ravitch@gmail.com>
;; Keywords: haskell, hydra
;; Version: 0.0.1

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;;  Commentary:

;; This package implements a minor mode for Haskell mode that adds a hydra for
;; manipulating LANGUAGE pragmas.  Invoke the hydra with "C-c l" (for "Language")
;; and then hit keys to add the displayed pragmas.
;;
;; Pragmas are only added if not already present.
;;
;; There is also an option to add a pragma that is not already listed.
;;
;; The hydra can be exited with Q.
;;
;; Example configuration:
;;
;; (use-package haskell-pragma
;;   :ensure nil
;;   :load-path "~/.emacs.d/local"
;;   :init (add-hook 'haskell-mode-hook 'haskell-pragma-mode))

;;; Code:

(require 'hydra)
(require 'haskell-mode)

(defconst haskell-pragma--pragma-rx "{-#\\s-*language\\s-\\(\\w+\\)\\s-*#-}")
(defconst haskell-pragma--module-rx "^module\\s-")

;; This list is taken from
;;
;; https://downloads.haskell.org/~ghc/8.4.1/docs/html/users_guide/glasgow_exts.html
(defconst haskell-pragma--ext-list
  '("AllowAmbiguousTypes"
    "ApplicativeDo"
    "Arrows"
    "BangPatterns"
    "BinaryLiterals"
    "CApiFFI"
    "ConstrainedClassMethods"
    "ConstraintKinds"
    "CPP"
    "DataKinds"
    "DatatypeContexts"
    "DefaultSignatures"
    "DeriveAnyClass"
    "DeriveDataTypeable"
    "DeriveFoldable"
    "DeriveFunctor"
    "DeriveGeneric"
    "DeriveLift"
    "DeriveTraversable"
    "DerivingStrategies"
    "DisambiguateRecordFields"
    "DuplicateRecordFields"
    "EmptyCase"
    "EmptyDataDecls"
    "EmptyDataDeriving"
    "ExistentialQuantification"
    "ExplicitForAll"
    "ExplicitNamespaces"
    "ExtendedDefaultRules"
    "FlexibleContexts"
    "FlexibleInstances"
    "ForeignFunctionInterface"
    "FunctionalDependencies"
    "GADTs"
    "GADTSyntax"
    "GeneralizedNewtypeDeriving"
    "HexFloatLiterals"
    "ImplicitParams"
    "ImplicitPrelude"
    "ImpredicativeTypes"
    "IncoherentInstances"
    "InstanceSigs"
    "InterruptibleFFI"
    "KindSignatures"
    "LambdaCase"
    "LiberalTypeSynonyms"
    "MagicHash"
    "MonadComprehensions"
    "MonadFailDesugaring"
    "MonoLocalBinds"
    "MonomorphismRestriction"
    "MultiParamTypeClasses"
    "MultiWayIf"
    "NamedFieldPuns"
    "NamedWildCards"
    "NegativeLiterals"
    "NPlusKPatterns"
    "NullaryTypeClasses"
    "NumDecimals"
    "OverlappingInstances"
    "OverloadedLabels"
    "OverloadedLists"
    "OverloadedStrings"
    "PackageImports"
    "ParallelListComp"
    "PartialTypeSignatures"
    "PatternGuards"
    "PatternSynonyms"
    "PolyKinds"
    "PostfixOperators"
    "QuasiQuotes"
    "Rank2Types"
    "RankNTypes"
    "RebindableSyntax"
    "RecordWildCards"
    "RecursiveDo"
    "RoleAnnotations"
    "Safe"
    "ScopedTypeVariables"
    "StandaloneDeriving"
    "StaticPointers"
    "Strict"
    "StrictData"
    "TemplateHaskell"
    "TemplateHaskellQuotes"
    "TraditionalRecordSyntax"
    "TransformListComp"
    "Trustworthy"
    "TupleSections"
    "TypeApplications"
    "TypeFamilies"
    "TypeFamilyDependencies"
    "TypeInType"
    "TypeOperators"
    "TypeSynonymInstances"
    "UnboxedSums"
    "UnboxedTuples"
    "UndecidableInstances"
    "UndecidableSuperClasses"
    "UnicodeSyntax"
    "Unsafe"
    "ViewPatterns"
    ))

(defun haskell-pragma--pragma-on-line (ln)
  "Return the pragma on the line LN, or nil if there is no pragma on the current line."
  (if (string-match haskell-pragma--pragma-rx ln)
      (match-string 1 ln)
    nil))

(defun haskell-pragma--record-existing-pragma (tbl)
  "If the current line contains a pragma, record it in TBL with a t value."
  (let* ((lb (line-beginning-position))
         (le (line-end-position))
         (ln (buffer-substring-no-properties lb le))
         (pragma (haskell-pragma--pragma-on-line ln)))
    (cond (pragma (puthash pragma t tbl))
          ((string-match haskell-pragma--module-rx ln) (throw 'haskell-pragma--break nil)))))

(defun haskell-pragma--index-extensions ()
  "Return an index (hash table) of all of the extensions currently enabled in the file."
  (interactive)
  (let* ((tbl (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char (point-min))
      (catch 'haskell-pragma--break
        (while (not (eobp))
          (haskell-pragma--record-existing-pragma tbl)
          (forward-line 1)))
      tbl)))

(defun haskell-pragma--unconditional-add-extension (ext-name)
  "Unconditionally add LANGUAGE EXT-NAME pragma at the top of a file."
  (save-excursion
    (goto-char (point-min))
    (insert "{-# LANGUAGE " ext-name " #-}\n")))

;;;###autoload
(defun haskell-pragma-add-extension (ext-name)
  "Add LANGUAGE pragma EXT-NAME to the top of the file, unless it is already enabled."
  (let ((current-pragmas (haskell-pragma--index-extensions)))
    (unless (gethash ext-name current-pragmas)
      (haskell-pragma--unconditional-add-extension ext-name))))

;;;###autoload
(defun haskell-pragma-add-other-extension (ext-name)
  "An interactive prompt for the extension (EXT-NAME) to add."
  (interactive (list (completing-read "Extension: " haskell-pragma--ext-list)))
  (haskell-pragma-add-extension ext-name))

(defhydra hydra-haskell-pragma (:hint nil)
  "
^Types^                           ^Syntax^                  ^Deriving^
^^^^^^^------------------------------------------------------------------
_t c_: ConstraintKinds            _s b_: BangPatterns       _d a_: DeriveAnyClass
_t d_: DataKinds                  _s e_: EmptyDataDecls     _d d_: DeriveDataTypeable
_t e_: ExistentialQuantification  _s l_: LambdaCase         _d f o_: DeriveFoldable
_t f c_: FlexibleContexts         _s m h_: MagicHash        _d f u_: DeriveFunctor
_t f i_: FlexibleInstances        _s m i_: MultiWayIf       _d g_: DeriveGeneric
_t f d_: FunctionalDependencies   _s o_: OverloadedStrings  _d l_: DeriveLift
_t g_: GADTs                      _s p_: PatternSynonyms    _d t_: DeriveTraversable
_t k_: KindSignatures             _s q_: QuasiQuotes        _d n_: GeneralizedNewtypeDeriving
_t m_: MultiParamTypeClasses      _s t h_: TemplateHaskell  _d s_: StandaloneDeriving
_t p_: PolyKinds                  _s t s_: TupleSections
_t r_: RankNTypes                 _s u s_: UnboxedSums
_t s_: ScopedTypeVariables        _s u t_: UnboxedTuples    _o c_: CPP
_t t a_: TypeApplications         _s v_: ViewPatterns       _o i_: ImplicitParams
_t t f_: TypeFamilies
_t t o_: TypeOperators
_t u_: UndecidableInstances
"
  ("t c" (haskell-pragma-add-extension "ConstraintKinds"))
  ("t d" (haskell-pragma-add-extension "DataKinds"))
  ("t e" (haskell-pragma-add-extension "ExistentialQuantification"))
  ("t f c" (haskell-pragma-add-extension "FlexibleContexts"))
  ("t f i" (haskell-pragma-add-extension "FlexibleInstances"))
  ("t f d" (haskell-pragma-add-extension "FunctionalDependencies"))
  ("t g" (haskell-pragma-add-extension "GADTs"))
  ("t k" (haskell-pragma-add-extension "KindSignatures"))
  ("t m" (haskell-pragma-add-extension "MultiParamTypeClasses"))
  ("t p" (haskell-pragma-add-extension "PolyKinds"))
  ("t r" (haskell-pragma-add-extension "RankNTypes"))
  ("t s" (haskell-pragma-add-extension "ScopedTypeVariables"))
  ("t t a" (haskell-pragma-add-extension "TypeApplications"))
  ("t t f" (haskell-pragma-add-extension "TypeFamilies"))
  ("t t o" (haskell-pragma-add-extension "TypeOperators"))
  ("t u" (haskell-pragma-add-extension "UndecidableInstances"))
  ("s b" (haskell-pragma-add-extension "BangPatterns"))
  ("s e" (haskell-pragma-add-extension "EmptyDataDecls"))
  ("s l" (haskell-pragma-add-extension "LambdaCase"))
  ("s m h" (haskell-pragma-add-extension "MagicHash"))
  ("s m i" (haskell-pragma-add-extension "MultiWayIf"))
  ("s o" (haskell-pragma-add-extension "OverloadedStrings"))
  ("s p" (haskell-pragma-add-extension "PatternSynonyms"))
  ("s q" (haskell-pragma-add-extension "QuasiQuotes"))
  ("s t h" (haskell-pragma-add-extension "TemplateHaskell"))
  ("s t s" (haskell-pragma-add-extension "TupleSections"))
  ("s u s" (haskell-pragma-add-extension "UnboxedSums"))
  ("s u t" (haskell-pragma-add-extension "UnboxedTuples"))
  ("s v" (haskell-pragma-add-extension "ViewPatterns"))
  ("d a" (haskell-pragma-add-extension "DeriveAnyClass"))
  ("d d" (haskell-pragma-add-extension "DeriveDataTypeable"))
  ("d f o" (haskell-pragma-add-extension "DeriveFoldable"))
  ("d f u" (haskell-pragma-add-extension "DeriveFunctor"))
  ("d g" (haskell-pragma-add-extension "DeriveGeneric"))
  ("d l" (haskell-pragma-add-extension "DeriveLift"))
  ("d t" (haskell-pragma-add-extension "DeriveTraversable"))
  ("d n" (haskell-pragma-add-extension "GeneralizedNewtypeDeriving"))
  ("d s" (haskell-pragma-add-extension "StandaloneDeriving"))
  ("o c" (haskell-pragma-add-extension "CPP"))
  ("o i" (haskell-pragma-add-extension "ImplicitParams"))
  ("o o" haskell-pragma-add-other-extension "other")
  ("q" nil "cancel"))

(defvar haskell-pragma-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map haskell-mode-map)
    (define-key map (kbd "C-c l") #'hydra-haskell-pragma/body)
    map)
  "Keymap for Haskell mode hydras.")

;;;###autoload
(define-minor-mode haskell-pragma-mode
  "Add a hydra for manipulating Haskell pragmas to Haskell mode."
  :keymap haskell-pragma-map
  :require 'haskell-mode)

(provide 'haskell-pragma)
;;; haskell-pragma.el ends here

