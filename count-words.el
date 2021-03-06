#! /bin/sh
":"; exec emacs --no-site-file --script "$0" -- "$@" # -*-emacs-lisp-*-
;;; count-words.el --- prepares output for wordle

;;; load s and dash
(setq package-load-list '((s t) (dash t)))
(package-initialize)
(require 'dash)
(require 's)



;;; files to operate on
(defvar my-files
  '("~/Documents/Papers/BuoyancySaturation/buoyancy_saturation.tex"
    "~/Documents/Papers/ThermalInstability/thermal_instability.tex"
    "~/Documents/Papers/OuterParts/outer-parts.tex"
    "~/Documents/Proposals/Chandra2011/Proposal.tex"
    "~/Documents/filament-proposal/proposal/proposal.tex")
  "Files to include in the word count.")



;;; words to exclude
(defvar ignore-words
  '("the" "be" "to" "of" "and" "a" "in" "that" "have" "I" "it"
    "for" "not" "on" "with" "he" "as" "you" "do" "at" "this"
    "but" "his" "by" "from" "they" "we" "say" "her" "she"
    "or" "an" "will" "my" "one" "all" "would" "there" "their"
    "what" "so" "up" "out" "if" "about" "who" "get" "which"
    "go" "me" "when" "make" "can" "like" "time" "no" "just"
    "him" "know" "take" "people" "into" "year" "your" "good"
    "some" "could" "them" "see" "other" "than" "then" "now"
    "look" "only" "come" "its" "over" "think" "also" "back"
    "after" "use" "two" "how" "our" "work" "first" "well"
    "way" "even" "new" "want" "because" "any" "these"
    "give" "day" "most" "us" "show" "shows" "are" "has" "much"
    "very" "more" "where" "thus" "may" "since" "though" "however"
    "therefore" "does")
  "Common words to ignore.")

(defvar ignore-latex
  '("label" "tsri" "left" "right" "newcommand" "citep" "citet" "section"
    "subsection" "subsubsection" "frac" "oldhat" "textsc" "textit" "eqref"
    "ref" "xspace" "sim" "gtrsim" "subequations" "cdot" "lesssim"
    "ensuremath" "textwidth" "widthof" "nabla" "usepackage" "citealt"
    "int" "inte" "sqrt" "fig" "subsec" "sec" "---" "vec" "begin" "end"
    "align" "dist" "hat" "scl" "sch" "citetalias" "tab" "partiald" "turb"
    "dot" "tsr" "renewcommand" "mathrm" "corr" "times" "text" "nat"
    "caption" "webbrown" "footnote" "mfig" "includegraphics" "mfigure"
    "baselineskip")
  "latex commands to ignore")

(defun ignore? (word)
  (or (member word ignore-words)
      (member word ignore-latex)))

(defun s-sanitize (str)
  (s-downcase str))



;;; set up a hash table to store data
(defun real-maphash (func table)
  "`maphash' doesn't return a list of the results; i.e. it isn't
a real map.  annoying!"
  (let ((result '()))
    (maphash
     (lambda (key val)
       (add-to-list 'result (funcall func key val)))
     table)
    result))

(defvar word-hash
  (make-hash-table
   :test 'equal))



;;; read data into the hash table
(with-temp-buffer
  (dolist (file my-files)
    (insert-file-contents file))
  (while (re-search-forward "[a-z][-a-z]+[a-z]" nil t)
    (let* ((word (s-sanitize (match-string-no-properties 0)))
           (num (gethash word word-hash 0)))
      (unless (ignore? word)
        (puthash word (+ 1 num) word-hash)))))



;;; combine duplicate words
(defun remove-dups (lst)
  "take a list of identical words [e.g. (\"simulation\"
\"simulations\" \"simulating\")] and combine into first element"
  (let ((base-word (car lst)))
    (dolist (word (cdr lst))
      (let ((num1 (gethash base-word word-hash 0))
            (num2 (gethash word word-hash 0)))
        (puthash base-word (+ num1 num2) word-hash)
        (remhash word word-hash)))))

(defvar stem-dups
  '(("absence" "absent")
    ("accelerate" "acceleration" "accelerating")
    ("account" "accounting")
    ("accrete" "accretion" "accreting" "accreted")
    ("accumulation" "accumulate")
    ("accurate" "accuracy")
    ("addition" "add")
    ("amplification" "amplify")
    ("amplify" "amplifies")
    ("analogous" "analogy")
    ("analyze" "analyzing" "analyzed")
    ("anisotropic" "anisotropies" "anisotropy")
    ("appear" "apparent")
    ("application" "applicable" "applicability" "applied" "applies" "apply")
    ("approximate" "approximation" "approx")
    ("arbitrary" "arbitrarily")
    ("archive" "archival")
    ("argument" "argue")
    ("assumption" "assume" "assuming")
    ("astronomical" "astronomy")
    ("astrophysical" "astrophysics" "astronomical" "astronomy")
    ("asymmetric" "asymmetry")
    ("atmosphere" "atmospheric")
    ("average" "averaging")
    ("baryon" "baryonic")
    ("boundaries" "boundary" "bound")
    ("buoyancy" "buoyant" "buoy")
    ("calculate" "calculating" "calculation" "calc")
    ("calibrate" "calibration")
    ("challenge" "challenging")
    ("collisionless" "collisionality" "collisional")
    ("compare" "comparison" "comparable" "comparing")
    ("computational" "computations" "compute" "computing")
    ("conclusion" "conclude" "conclusive")
    ("conduction" "conductivity" "conduct" "conducted" "conducting")
    ("consequence" "consequently")
    ("constrain" "constraint")
    ("convection" "convective" "conv")
    ("converge" "convergence")
    ("cosmological" "cosmic" "cosmology")
    ("depend" "dependence")
    ("describe" "describing" "description")
    ("determine" "determining" "determination" "determinations")
    ("difference" "different" "differ")
    ("diffusion" "diffuse" "diffusivity")
    ("dimension" "dimensionality" "dimensionless")
    ("dynamic" "dyn")
    ("effect" "effective" "eff")
    ("energy" "energies" "energetic")
    ("equivalent" "equiv")
    ("figure" "fig")
    ("fit" "fitting")
    ("frequencies" "freq" "frequency")
    ("grow" "growth")
    ("instability" "unstable" "instabilities" "destabilizing" "destabilizes")
    ("relation" "relationship")
    ("remain" "remainder")
    ("saturation" "saturate")
    ("similar" "similarities")
    ("stable" "stability" "stabilizing" "stabilize" "stably" "stabilizing")
    ("system" "systematic")
    ("through" "throughout")
    ("virial" "vir")
    ("wave" "wavelength")
    ("weak" "weakens"))
  "Words which should be considered the same.  Note that simple
suffixes are handles automatically below.  The rules in this list
come last, so can be used to decide which word 'wins'.")

;; find words which differ only by simple suffixes and add them to the
;; stem-dups list.  add to the beginning so as not to over-write
;; things in that list.
;; FIXME: this assumes the un-suffixed word exists in the data.  So
;; some things won't be combined.
(let ((word-list (real-maphash (lambda (key val) key) word-hash))
      (suffixes '("s" "n" "r" "d"
                  "ed" "es" "er" "est" "st"
                  "ing" "ion" "al" "ally" "ly"
                  "ment" "ments"  "ation"
                  "ize" "izes" "ized"
                  "ative"
                  "ary" "ing" "ive")))
  (dolist (word word-list)
    (dolist (suffix suffixes)
      (if (member (concat word suffix) word-list)
          (setq stem-dups
                (cons (list word (concat word suffix)) stem-dups))))))

(mapc 'remove-dups stem-dups)



(defun get-color (num)
  (let ((sl-base03  "#002b36")
        (sl-base02  "#073642")
        (sl-base01  "#586e75")
        (sl-base00  "#657b83")
        (sl-base0   "#839496")
        (sl-base1   "#93a1a1")
        (sl-base2   "#eee8d5")
        (sl-base3   "#fdf6e3")
        (sl-yellow  "#b58900")
        (sl-orange  "#cb4b16")
        (sl-red     "#dc322f")
        (sl-magenta "#d33682")
        (sl-violet  "#6c71c4")
        (sl-blue    "#268bd2")
        (sl-cyan    "#2aa198")
        (sl-green   "#859900"))
    (let ((sl-bkg   sl-base02)
          (sl-fg1   sl-base01)
          (sl-fg2   sl-base0)
          (sl-fg3   sl-base1)
          (sl-hl1   sl-yellow)
          (sl-hl2   sl-magenta))
      (let* ((sl-rndm (list sl-violet sl-blue sl-cyan sl-green))
             (rnd-i (random (length sl-rndm)))
             (rnd-color (nth rnd-i sl-rndm))
             (rnd? (< (random 500) 200)))
        (if (and (> num 5) rnd?)
            rnd-color
          (cond
           ((<= num 1)   sl-hl2)
           ((<= num 5)   sl-hl1)
           ((<= num 25)  sl-fg3)
           ((<= num 125) sl-fg2)
           (t            sl-fg1)))))))

;;; turn the hash into a string and write it to a file.
(defun sort-print-wordhash (hash)
  (let ((wordlist (real-maphash
                   (lambda (key val) (list val key))
                   hash)))
    (let ((newlist (sort wordlist
                         (lambda (a b) (> (car a) (car b))))))
      (let ((fmtlist (-map-indexed
                      (lambda (ind elt) (format "%s:%d:%s"
                                                (cadr elt) (car elt)
                                                (s-right 6 (get-color ind))))
                      newlist)))
        (mapconcat 'identity fmtlist "\n")))))

; sort and print the data
(with-temp-file "word-frequency.dat"
  (insert (sort-print-wordhash word-hash)))
