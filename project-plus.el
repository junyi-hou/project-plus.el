;;; project-plus.el --- light interface of project.el -*- lexical-binding: t; -*-

;; Package-requires: ((emacs "26") (project "0.3.0") (f "0.20.0"))

;;; Commentary:

;;; Code:

(require 'project)
(require 'f)

(defgroup project-plus nil
  "Small extensions to emacs' build-in project.el"
  :group 'project)

(defcustom project-plus-saved-projects-file
  (expand-file-name "known-project"
                    (if (boundp 'no-littering-var-directory)
                        no-littering-var-directory
                      (concat user-emacs-directory (file-name-as-directory "var"))))
  "path to the file saving the list of known project roots."
  :type 'string
  :group 'project-plus)

(defcustom project-plus-search-path
  '("~/projects/")
  "A list of path to search for projects."
  :type 'list
  :group 'project-plus)

(defvar project-plus-known-projects '()
  "A list of known projects.")

;; function
(defun project-plus--init-saved-projects-file ()
  "Initialize `project-plus-saved-projects-file' if the file does not exists"
  (unless (file-exists-p project-plus-saved-projects-file)
    (with-temp-buffer
      (write-file project-plus-saved-projects-file))))

(defun project-plus--retrieve-known-projects ()
  "Retrieve all known projects saved in `project-plus-saved-projects-file'."
  (project-plus--init-saved-projects-file)
  (with-temp-buffer
    (insert-file-contents project-plus-saved-projects-file)
    (seq-filter (lambda (path) (not (string= path "")))
                (split-string (buffer-string) "\n"))))

;; update `project-plus-known-projects' if we are visiting a
;; new repository
(defun project-plus--update-known-projects (&rest _)
  "Update `project-plus-known-projects' if we are visiting a new repository."
  (let ((new-vc-root (project-plus-get-root)))
    (when (and new-vc-root
               (not (seq-find (lambda (search-path)
                                (f-parent-of-p search-path new-vc-root))
                              project-plus-search-path))
               (not (member new-vc-root project-plus-known-projects)))
      (add-to-list 'project-plus-known-projects new-vc-root))))

(add-hook 'find-file-hook #'project-plus--update-known-projects)

(defun project-plus--update-known-projects-file ()
  (with-temp-buffer
    (mapc (lambda (vc-root)
            (insert vc-root)
            (insert "\n"))
          project-plus-known-projects)
    (write-file project-plus-saved-projects-file)))

(add-hook 'kill-emacs-hook #'project-plus--update-known-projects-file)

(defun project-plus-get-root (&optional dir)
  "Return the project root of DIR. If DIR is not given, use `default-directory'.  If there is no project found at DIR, return nil."
  (let ((project (project-current nil dir)))
    (when (eq 'vc (car project))
      ;; make sure we identify the project using git
      (cdr project))))

(defun project-plus-get-file-list (dir)
  "Return the file list of the project at DIR via \"git ls-files -zco --exclude-standard\"."
  (let ((default-directory (gatsby:project-get-root dir)))
    (split-string
     (shell-command-to-string "git ls-files -co --exclude-standard")
     "\n")))

;;;###autoload
(defun project-plus-find-file (&optional dir)
  "Find file in project at DIR.  If DIR does not contain a project, fall backs to `counsel-find-file'."
  (interactive)
  (let* ((default-directory (or dir default-directory))
         (proj? (gatsby:project-get-root dir))
         (collection (when proj? (gatsby:project-get-file-list proj?))))
    (if proj?
        ;; inside a project
        (thread-last collection
          (completing-read (concat "Find file in " proj? ": "))
          (concat proj?)
          find-file)
      ;; outside of project
      (call-interactively #'find-file))))

(defun project-plus--search-path ()
  "Search paths registered in `project-plus-search-path' and return a list of projects identified by git."
  (apply #'append
         (mapcar
          (lambda (path)
            (seq-filter
             (lambda (dir) (project-plus-get-root dir))
             (directory-files path 'full "[^\.]")))
          project-plus-search-path)))

;;;###autoload
(defun project-plus-switch-project ()
  "Switch to project."
  (interactive)
  (let ((enable-recursive-minibuffers t)
        (projs (append project-plus-known-projects (project-plus--search-path))))
    (project-plus-find-file (completing-read "Switch to: " projs))))


(provide 'project-plus)
;;; project-plus.el ends here
