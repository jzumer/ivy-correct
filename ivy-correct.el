;;; ivy-correct.el --- Show dictionary words close to word at point

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Jeremie Zumer <jeremie_zumer@hotmail.com>
;; Version: 1.0
;; Package-Requires: (cl-lib ivy levenshtein)
;; Keywords: ivy, spellcheck

;;; Commentary:

;; This package uses ivy to suggest possible corrections for word at point.
;; Unlike current checkers for emacs, this package does not care if the word
;; already exists in the dictionary, which is a common cause of false-positives
;; in languages that have accents or other diacritics.
;; To use it, first select a dictionary (which will be built and cached) with `ivy-correct-switch-dict'.
;; Then, bind `ivy-correct-spellcheck' to a key of your choice.

;;; Code:

(defcustom ivy-correct-dict-prefix "/usr/share/hunspell/"
  "The path to the dictionary files (.dic format), used to populate the switch-dict menu")
(defcustom ivy-correct-dict-cache (concat (file-name-directory load-file-name) ".ivy-correct-cache")
  "The path to the cached dictionary files, containing processed .dic files generated
by switch-dict.")
(defcustom ivy-correct-default-dict "en_US"
  "What directory to use at startup. `nil' to disable this.")
(defvar ivy-correct--active-dict nil)
(defvar ivy-correct--accent-table
  (let ((raw-accent-table
         '(("a" "á" "â" "à" "ã")
           ("e" "é" "ê" "ë" "è" )
           ("c" "ç")
           ("n" "ñ")
           ("o" "õ" "ó" "ô" "ö" "ò")
           ("i" "í" "î" "ï" "ì")
           ("u" "ú" "û" "ü" "ù"))))
    (append raw-accent-table
            (loop for l in raw-accent-table collect
                  (mapcar #'upcase l)))))

(defun ivy-correct-read-dictionary-lines (dict)
  (interactive "s")
  (let ((ret (make-hash-table))
        (words (mapcar (lambda (x) (first (split-string (first (split-string x "/" t)) nil t)))
                       (with-temp-buffer
                         (insert-file-contents dict)
                         (split-string (buffer-string) "\n" t)))))
    (loop for w in words do
          (setf (gethash (length w) ret) (cons (list w (downcase w) (ivy-correct--remove-accents (downcase w))) (gethash (length w) ret nil))))
    (loop for k being the hash-keys of ret do
          (setf (gethash k ret)
                (remove-duplicates (gethash k ret)
                                   :test #'equal
                                   :key #'first)))
    (setq ivy-correct--active-dict ret)
    (with-temp-file (concat ivy-correct-dict-cache "/" dict-id ".icd")
      (prin1 ret (current-buffer)))))

(defun ivy-correct--uncache (dict)
  (let ((cached-name (concat ivy-correct--dict-cache "/" dict ".icd")))
    (when (file-exists-p cached-name)
      (delete-file cached-name))))

(defun ivy-correct-force-switch-dict ()
  (interactive)
  (ivy-correct-switch-dict t))

(defun ivy-correct-switch-dict (&optional force)
  (interactive)
  (let* ((dict-id (ivy-completing-read "Dictionary: "
                                       (remove-duplicates
                                        (mapcar (lambda (x) (file-name-sans-extension x))
                                                (directory-files ivy-correct-dict-prefix))
                                        :test #'equal)))
         (path (concat ivy-correct-dict-prefix dict-id ".dic"))
         (cached-path (concat ivy-correct-dict-cache "/" dict-id ".icd")))
    (when (not (file-directory-p ivy-correct-dict-cache))
      (mkdir ivy-correct-dict-cache))
    (if (and (file-exists-p cached-path) (not force))
        (with-temp-buffer
          (insert-file-contents cached-path)
          (goto-char (point-min))
          (setq ivy-correct--active-dict (read (current-buffer))))
      (if (file-exists-p path)
          (setq ivy-correct--active-dict (ivy-correct-read-dictionary-lines path))
        (error "Couuld not find dictionary %S" dict-id))))
  nil)

(defun ivy-correct--remove-accents (word)
  (apply #'concat
         (loop for c across word collect
               (let ((strc (string c)))
                 (loop for s in ivy-correct--accent-table do
                       (when (member strc s)
                         (return (car s)))
                       finally return strc)))))

(defun ivy-correct-spellcheck ()
  (interactive)
  (when (not ivy-correct--active-dict)
    (when ivy-correct-default-dict
      (ivy-correct-switch-dict ivy-correct-default-dict)))
  (ivy-read "Correction: "
            ivy-correct--active-dict
            :initial-input (thing-at-point 'word t)
            :action (lambda (result)
                      (interactive)
                      (kill-region (beginning-of-thing 'word) (end-of-thing 'word))
                      (insert result))
            :matcher (lambda (query candidates)
                       (interactive)
                       (let* ((dc-query (ivy-correct--remove-accents (downcase (caar query))))
                              (min-size (max (- (length query) 3) 1))
                              (max-size (min (+ (length query) 3) (length query)))
                              (scores (loop for bin from min-size upto max-size append
                                            (loop for c in (gethash bin ivy-correct--active-dict)
                                                  collect
                                                  (list (levenshtein-distance dc-query (third c)) (first c) (third c))))))
                         (mapcar #'first
                                 (cl-sort
                                  (loop for index in (subseq
                                                      (cl-sort scores
                                                               #'<
                                                               :key #'first)
                                                      0
                                                      (min (length scores) 100))
                                        collect
                                        (rest index))
                                  #'string-lessp
                                  :key #'second))))))
