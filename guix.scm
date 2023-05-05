(use-modules (git)
             (git repository)
             (git reference)
             (git commit)
             (git oid)
             (guix git)
             (guix git-download)
             (guix packages)
             (guix build-system copy)
             (guix build-system gnu)
             (guix build-system texlive)
             (guix gexp)
             (gnu packages tex)
             ((guix licenses) #:prefix license:)
             (srfi srfi-26))

(define (head-commit checkout)
 "Get the current HEAD of CHECKOUT."
  (let* ((repo (repository-open checkout))
         (head (reference-target (repository-head repo)))
         (commit (commit-lookup repo head)))
    (repository-close! repo)
    (oid->string head)))

;; Use the HEAD of the local checkout unless running on the CI.
(define %repository
  (or (and=> (getenv "GITHUB_SERVER_URL")
             (cut string-append <> "/" (getenv "GITHUB_REPOSITORY")))
      (dirname (current-filename))))

(define %commit (or (getenv "GITHUB_SHA")
                    (head-commit %repository)))

(define cv
  (package
    (name "cv")
    (home-page "https://github.com/mbakke/cv")
    (version (string-append "0.0-" (string-take %commit 7)))
    (source (git-checkout (url %repository) (commit %commit)))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (replace 'build
                 (lambda _
                   (invoke "xelatex" "cv.tex")))
               (replace 'install
                 (lambda _
                   (install-file "cv.pdf" #$output))))))
    (inputs
     (list
      texlive-base
      texlive-enumitem
      texlive-etoolbox
      texlive-everysel
      texlive-fontspec
      texlive-hyperref
      texlive-ifmtarg
      texlive-lm-math
      texlive-ragged2e
      texlive-setspace
      texlive-tcolorbox
      texlive-unicode-math
      texlive-xcolor
      texlive-xetex
      texlive-xifthen

      texlive-sourcesanspro

      texlive-latex-base
      texlive-latex-bookmark
      texlive-latex-fancyhdr
      texlive-latex-geometry
      texlive-latex-parskip
      ;; XXX: These are not directly required and should be propagated
      ;; from somewhere else.
      texlive-latex-kvoptions
      texlive-latex-trimspaces
      texlive-latex-xkeyval

      texlive-awesome-cv
      texlive-fontawesome5
      texlive-roboto))
    (synopsis "Build a CV")
    (description
     "This package contains my CV.")
    ;; Hmm what license..?
    (license #f)))

(define texlive-awesome-cv
  (let ((commit "72a350157a505b766567d16c6dc4b03960bd7eb7")
        (revision "0"))
    (package
      (name "texlive-awesome-cv")
      (version (git-version "0.0" revision commit))
      (home-page "https://github.com/posquit0/Awesome-CV")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (sha256
                 (base32
                  "1005phd200yin6dzrxx3w1b4kfsxyvn0b3hh0rp8b8zckj1jhcnp"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan
           '(("awesome-cv.cls" "share/texmf-dist/tex/xelatex/awesome-cv.cls"))))
      (synopsis "Awesome CV template")
      (description "This package contains a LaTeX template for crating a CV.")
      (license license:lppl1.3+))))

(define simple-texlive-package
  (@@ (gnu packages tex) simple-texlive-package))

;; TODO: Upstream these!
(define texlive-fontawesome5
  (package
    (inherit (simple-texlive-package
              "texlive-fontawesome5"
              '("/doc/fonts/fontawesome5/"
                "/fonts/enc/dvips/fontawesome5/"
                "/fonts/map/dvips/fontawesome5/"
                "/fonts/opentype/public/fontawesome5/"
                ;; ^ see comment on texlive-sourcesanspro
                ;; Note: In theory we could use font-awesome from Guix,
                ;; but in practice the versions may differ and results be
                ;; subtly broken.
                "/fonts/tfm/public/fontawesome5/"
                "/fonts/type1/public/fontawesome5/"
                "/tex/latex/fontawesome5/")
              (base32
               "1mxr1vbdiwnzjfy1mkb51zgslwncbgnzc97rxxlrvhxv29bj2lra")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/fontawesome5")
    (synopsis "TeX support for the Font Awesome font family")
    (description
     "This package provides LaTeX support for the Font awesome icon set.")
    (license (list license:lppl1.3+ license:silofl1.1))))

(define texlive-roboto
  (package
    (inherit (simple-texlive-package
              "texlive-roboto"
              (list "/doc/fonts/roboto/"
                    "/fonts/enc/dvips/roboto/"
                    "/fonts/map/dvips/roboto/"
                    "/fonts/opentype/google/roboto/"

                    ;; XXX see comment aboute font-awesome
                    "/fonts/tfm/google/roboto/"
                    "/fonts/type1/google/roboto/"
                    "/fonts/vf/google/roboto/"
                    "/tex/latex/roboto/")
              (base32
               "1zly03wgrr5yjgpa8qxrxb01cqdk2yyc1vm1lzm270zrsgy6d0m2")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/roboto")
    (synopsis "TeX support for the Roboto font family")
    (description
     "This package provides LATEX, pdfLATEX, XELATEX and LuaLATEX support
for the Roboto Sans, Roboto Condensed, Roboto Mono, Roboto Slab and Roboto Serif
families of fonts, designed by Christian Robertson and Greg Gazdowicz for Google.")
    (license license:lppl1.3+)))

cv
