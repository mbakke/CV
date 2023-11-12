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
      texlive-bookmark
      texlive-enumitem
      texlive-etoolbox
      texlive-everysel
      texlive-fancyhdr
      texlive-fontspec
      texlive-geometry
      texlive-hyperref
      texlive-ifmtarg
      texlive-lm-math
      texlive-parskip
      texlive-ragged2e
      texlive-scheme-basic
      texlive-setspace
      texlive-tcolorbox
      texlive-unicode-math
      texlive-xcolor
      texlive-xetex
      texlive-xifthen

      ;; XXX: These are not directly required and should be propagated
      ;; from somewhere else.
      texlive-environ
      texlive-pgf
      texlive-tikzfill
      texlive-xkeyval

      texlive-awesome-cv
      texlive-fontawesome5
      texlive-roboto
      texlive-sourcesanspro))
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

cv
