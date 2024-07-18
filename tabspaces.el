;;; tabspaces.el --- Leverage tab-bar and project for buffer-isolated workspaces  -*- lexical-binding: t -*-

;; Author: Colin McLear <mclear@fastmail.com>
;; Maintainer: Mou Tong <mou.tong@qq.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (project "0.8.1"))
;; Keywords: convenience, frames
;; Homepage: https://github.com/dalugm/tabspaces

;; Copyright (c) 2022-2023 Colin McLear
;; Copyright (c) 2024 Mou Tong

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides several functions to facilitate a frame-based
;; tab workflow with one workspace per tab, integration with `project'
;; (for project-based workspaces) and buffer isolation per tab (i.e. a
;; "tabspace" workspace). The package assumes `project' and `tab-bar'
;; are both present (they are built-in to Emacs 27.1+).

;;; Acknowledgements
;; Much of the package code is inspired by:

;; - https://github.com/kaz-yos/emacs
;; - https://github.com/wamei/elscreen-separate-buffer-list/issues/8
;; - https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/
;; - https://github.com/minad/consult#multiple-sources
;; - https://github.com/florommel/bufferlo

;;; Code:

;;;; Requirements

(eval-when-compile (require 'cl-lib))
(require 'tab-bar)
(require 'project)
(require 'vc-git)
(require 'seq)

(declare-function magit-init "magit-status")
(declare-function magit-status-setup-buffer "magit-status")

;;;; Variables

(defgroup tabspaces nil
  "Manage tab/workspace buffers."
  :group 'convenience)

(defcustom tabspaces-include-buffers '("*scratch*" "*Messages*")
  "Buffers that should always get included in a new tabspace.
This is a string list that matches buffer names, which does not
overrides buffers excluded by `tabspaces-exclude-buffers'."
  :group 'tabspaces
  :type '(repeat string))

(defcustom tabspaces-exclude-buffers nil
  "Buffers that should always get excluded in a new tabspace.
This is a string list that matches buffer names, which overrides
buffers inside `tabspaces-include-buffers'."
  :group 'tabspaces
  :type '(repeat string))

(defcustom tabspaces-use-filtered-buffers-as-default nil
  "When t, remap `switch-to-buffer' to `tabspaces-switch-to-buffer'."
  :group 'tabspaces
  :type 'boolean)

(defcustom tabspaces-keymap-prefix (kbd "C-c C-w")
  "Prefix key for tabpsaces related commands."
  :group 'tabspaces
  :type 'key)

(defcustom tabspaces-project-switch-commands project-switch-commands
  "Available commands when switch between projects.
Change this value if you wish to run a specific command, such as
`find-file' on project switch.  Otherwise this will default to
the value of `project-switch-commands'."
  :group 'tabspaces
  :type 'sexp)

(defcustom tabspaces-use-truepath nil
  "Whether resolve \".\", \"..\", etc. in project path."
  :group 'tabspaces
  :type 'boolean)

;;;; Create Buffer Workspace

(defun tabspaces--reset-buffer-list ()
  "Reset the current TAB's `buffer-list'.
Only the buffers in `tabspaces-include-buffers' and buffers not
in `tabspaces-exclude-buffers' are kept in the `buffer-list' and
`buried-buffer-list'."
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html
  ;; The current-tab uses `buffer-list' and `buried-buffer-list'.
  ;; A hidden tab keeps these as `wc-bl' and `wc-bbl'.
  (cl-flet* ((filter-fn (buffer)
               (and (member (buffer-name buffer) tabspaces-include-buffers)
                    (not (member (buffer-name buffer) tabspaces-exclude-buffers))))
             (update-frame (param)
               (set-frame-parameter
                nil
                param
                (seq-filter #'filter-fn (frame-parameter nil param)))))
    (update-frame 'buffer-list)
    (update-frame 'buried-buffer-list)))

(defun tabspaces--tab-post-open-function (_tab)
  "Update buffer list on new tab creation."
  (tabspaces--reset-buffer-list))

;;;; Filter Workspace Buffers

(defun tabspaces--local-buffer-p (buffer)
  "Return whether BUFFER is in the list of local buffers."
  (or (member (buffer-name buffer) tabspaces-include-buffers)
      (memq buffer (frame-parameter nil 'buffer-list))))

(defun tabspaces--set-buffer-predicate (frame)
  "Set the buffer predicate of FRAME to `tabspaces--local-buffer-p'."
  (set-frame-parameter frame 'buffer-predicate #'tabspaces--local-buffer-p))

(defun tabspaces--reset-buffer-predicate (frame)
  "Reset the buffer predicate of FRAME if it is `tabspaces--local-buffer-p'."
  (when (eq (frame-parameter frame 'buffer-predicate) #'tabspaces--local-buffer-p)
    (set-frame-parameter frame 'buffer-predicate nil)))

(defun tabspaces--buffer-list (&optional frame index)
  "Return a list of live buffers associated with FRAME and INDEX.
A non-nil FRAME will select the specific frame instead of the current
one.  A non-nil INDEX will specify the corresponding tab index in the
given frame."
  (seq-filter #'buffer-live-p
              (if index
                  (let ((tab (nth index (frame-parameter frame 'tabs))))
                    (if (eq 'current-tab (car tab))
                        (frame-parameter frame 'buffer-list)
                      (or
                       (cdr (assq 'wc-bl tab))
                       (mapcar #'get-buffer
                               (cadr (assq #'tabspaces--buffer-list (assq 'ws tab)))))))
                (frame-parameter frame 'buffer-list))))

;;;; Project Workspace Helper Functions

;;;###autoload
(defun tabspaces--current-tab-name ()
  "Get name of current tab."
  (cdr (assq 'name (tab-bar--current-tab))))

;;;###autoload
(defun tabspaces--list-tabspaces ()
  "Return a list of `tab-bar' tabs/workspaces."
  (mapcar (lambda (tab) (alist-get 'name tab))
          (funcall tab-bar-tabs-function)))

;;;###autoload
(defun tabspaces--project-name ()
  "Get name for project from vc.
If not in a project return buffer filename, or `-' if not visiting a file."
  (let ((buf (buffer-file-name)))
    (cond ((and buf (vc-registered buf))
           (file-name-nondirectory (directory-file-name (vc-root-dir))))
          (t "-"))))

;;;###autoload
(defun tabspaces--name-tab-by-project-or-default ()
  "Return project name if in a project, or default tab-bar name if not.
The default tab-bar name uses the buffer name along with a counter."
  (let ((project-name (tabspaces--project-name))
        (tab (tab-bar-tab-name-current)))
    (cond ((string= tab project-name)
           (tab-bar-switch-to-tab tab))
          ((string= "-" project-name)
           (tab-bar-tab-name-current-with-count))
          (t (tabspaces--project-name)))))

;;;; Interactive Functions

;;;;; Buffer Functions

(defun tabspaces--kill-buffer (&optional buffer)
  "Bury and remove BUFFER from current tabspace.
If BUFFER is nil, remove current buffer."
  (let ((buffer (get-buffer (or buffer (current-buffer))))
        (buffer-list (frame-parameter nil 'buffer-list)))
    (cond
     ((eq buffer (window-buffer (selected-window)))
      (if (one-window-p t)
          (bury-buffer)
        (delete-window)))
     ((get-buffer-window buffer)
      (select-window (get-buffer-window buffer) t)
      (if (one-window-p t)
          (bury-buffer)
        (delete-window)))
     (t
      (message (format "Buffer `%s' removed from `%s' tabspace."
                       buffer (tabspaces--current-tab-name)))))
    (bury-buffer buffer)
    (delete buffer buffer-list)))

(defun tabspaces-kill-buffer (buffer)
  "Remove selected BUFFER from frame's buffer list."
  (interactive
   (list
    (let ((blst (mapcar (lambda (b) (buffer-name b))
                        (tabspaces--buffer-list))))
      ;; Select buffer.
      (read-buffer (format "Remove buffer from `%s' tabspace: "
                           (tabspaces--current-tab-name))
                   nil t
                   (lambda (b) (member (car b) blst))))))
  ;; Remove buffer from current tabspace's buffer list.
  (tabspaces--kill-buffer buffer))

(defun tabspaces-switch-to-buffer (buffer &optional norecord force-same-window)
  "Display the local buffer BUFFER in the selected window.
This is the frame/tab-local equivalent to `switch-to-buffer'.
The arguments NORECORD and FORCE-SAME-WINDOW are passed to `switch-to-buffer'."
  (interactive
   (list
    (let ((blst (cl-remove (buffer-name) (mapcar #'buffer-name (tabspaces--buffer-list)))))
      (read-buffer
       "Switch to local buffer: " blst nil
       (lambda (b) (member (if (stringp b) b (car b)) blst))))))
  (switch-to-buffer buffer norecord force-same-window))

;; See https://emacs.stackexchange.com/a/53016/11934
(defun tabspaces--report-dupes (xs)
  (let ((ys  ()))
    (while xs
      (unless (member (car xs) ys) ; Don't check it if already known to be a dup.
        (when (member (car xs) (cdr xs)) (push (car xs) ys)))
      (setq xs  (cdr xs)))
    ys))

(defun tabspaces-switch-buffer-and-tab (buffer &optional norecord force-same-window)
  "Switch to the tab of chosen buffer, or create buffer.
If buffer does not exist in buffer-list user can either create a
new tab with the new buffer or open a new buffer in the current
tab."
  (interactive
   (list
    (let ((blst (cl-remove (buffer-name) (mapcar #'buffer-name (buffer-list)))))
      (read-buffer
       "Switch to tab for buffer: " blst nil
       (lambda (b) (member (if (stringp b) b (car b)) blst))))))

  ;; Action on buffer
  (let* ((tabcand nil)
         (buflst nil)
         ;; Provide flat list of all buffers in all tabs (and print dupe buffers).
         ;; This is the list of all buffers to search through.
         (bufflst (flatten-tree (dolist (tab (tabspaces--list-tabspaces) buflst)
                                  (push (mapcar #'buffer-name (tabspaces--buffer-list nil (tab-bar--tab-index-by-name tab)))
                                        buflst))))
         (dupe (member buffer (tabspaces--report-dupes bufflst))))
    ;; Run through conditions:
    (cond
     ;; 1. Buffer exists and is not open in more than one tabspace.
     ((and (get-buffer buffer)
           (not dupe))
      (dolist (tab (tabspaces--list-tabspaces))
        (when (member buffer (mapcar #'buffer-name (tabspaces--buffer-list nil (tab-bar--tab-index-by-name tab))))
          (progn (tab-bar-switch-to-tab tab)
                 (tabspaces-switch-to-buffer buffer)))))
     ;; 2. Buffer exists and is open in more than one tabspace.
     ((and (get-buffer buffer) dupe)
      (dolist (tab (tabspaces--list-tabspaces) tabcand)
        (when (member buffer (mapcar #'buffer-name (tabspaces--buffer-list nil (tab-bar--tab-index-by-name tab))))
          (push tabcand tab)))
      (tab-bar-switch-to-tab (completing-read "Select tab: " tabcand))
      (tabspaces-switch-to-buffer buffer))
     ;; 3. Buffer does not exist.
     ((yes-or-no-p "Buffer not found -- create a new workspace with buffer?")
      (switch-to-buffer-other-tab buffer))
     ;; 4. Default -- create buffer in current tabspace.
     (t
      (switch-to-buffer buffer norecord force-same-window)))))

(defun tabspaces-clear-buffers (&optional frame)
  "Clear the tabspace's buffer list, except for the current buffer.
If FRAME is nil, use the current frame."
  (interactive)
  (set-frame-parameter frame 'buffer-list
                       (list (if frame
                                 (with-selected-frame frame
                                   (current-buffer))
                               (current-buffer)))))

;;;;; Switch or Create Workspace
;; Some convenience functions for opening/closing workspaces and buffers.
;; Some of these are just wrappers around built-in functions.
;;;###autoload
(defun tabspaces-switch-workspace (&optional workspace)
  "Switch to tab if it exists, otherwise create a new tabbed workspace."
  (interactive
   (if-let ((tabs (tabspaces--list-tabspaces)))
       (list (completing-read "Select or create tab: " tabs))
     (tab-new)
     (tab-rename (completing-read "Workspace name: " tabs))))
  (if (member workspace (tabspaces--list-tabspaces))
      (tab-bar-switch-to-tab workspace)
    (tab-new)
    (tab-rename workspace)))

;;;;; Forget Workspace
(defalias 'tabspaces-forget-workspace #'project-forget-project)
(defalias 'tabspaces-forget-zombie-workspaces #'project-forget-zombie-projects)

;;;;; Rename Workspace
(defalias 'tabspaces-rename-workspace #'tab-bar-rename-tab)

;;;;; Close Workspace & Kill Buffers
(defun tabspaces-close-workspace (workspace)
  "Kill all buffers and close current WORKSPACE.
When with a \\[universal-argument], select a WORKSPACE to close."
  (interactive
   (list (if (equal current-prefix-arg '(4))
             (completing-read "Close workspace: " (tabspaces--list-tabspaces))
           (tabspaces--current-tab-name))))
  (if (= 1 (length (tabspaces--list-tabspaces)))
      (user-error "Attempt to close the sole workspace")
    (let ((buf-lst (tabspaces--buffer-list
                    nil
                    (tab-bar--tab-index-by-name workspace))))
      (unwind-protect
          (cl-loop for buf in buf-lst
                   do (unless (member (buffer-name buf)
                                      tabspaces-include-buffers)
                        (kill-buffer buf)))
        (tab-bar-close-tab-by-name workspace)))))

;;;;; Open project in workspace.
(defun tabspaces--generate-tab-name (path)
  "Generate a tab name.
If the name already existed inside `tabspaces-project-tab-alist', then
generate the name based on its PATH, otherwise use its dir's name."
  (let* ((reverse-path-lst (reverse (string-split (directory-file-name path) "/")))
         (base-name  (car reverse-path-lst))
         (parent-dir (cadr reverse-path-lst))
         (gp-dir     (caddr reverse-path-lst)))
    (if (member base-name (tabspaces--list-tabspaces))
        (format "%s (%s/%s)" base-name (or gp-dir "") parent-dir)
      base-name)))

;;;###autoload
(defun tabspaces-open-workspace (&optional project)
  "Open PROJECT and its workspace with a descriptive tab name."
  (interactive (list (project-prompt-project-dir)))
  (let* ((project-switch-commands tabspaces-project-switch-commands)
         (project-dir (if tabspaces-use-truepath
                          (expand-file-name project)
                        project))
         (tab-name (tabspaces--generate-tab-name project-dir))
         (session (concat project-dir "." tab-name "-tabspaces-session.el"))
         (dir-potential-project (project--find-in-directory project-dir))
         (project-remembered-p (member (list project-dir) project--list))
         (tab-existed-p (member tab-name (tabspaces--list-tabspaces))))
    (cond
     ;; 1. If both project and tab exist, then switch to it.
     ((and project-remembered-p tab-existed-p)
      (tab-bar-switch-to-tab tab-name))
     ;; 2. If project (or a directory with actual project contents)
     ;; exists while tab not, then open tabspace and check for session
     ;; to restore, otherwise start a new session.
     ((and (or project-remembered-p dir-potential-project)
           (not tab-existed-p))
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      (let ((default-directory project-dir))
        (if (file-exists-p session)
            (tabspaces-restore-session session)
          (project-switch-project project-dir))
        (unless project-remembered-p
          (project-remember-project dir-potential-project))))
     ;; 3. Open new tab and create project.
     (t
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      (project-switch-project project-dir)
      (if (vc-git-root project-dir)
          (if (featurep 'magit)
              (magit-status-setup-buffer)
            (project-vc-dir))
        (if (featurep 'magit)
            (magit-init project-dir)
          (vc-git-create-repo)
          (project-vc-dir)))))))

;;;; Tabspace Sessions

(defconst tabspaces-session-header
  ";; -------------------------------------------------------------------------
;; Tabspaces Session File for Emacs
;; -------------------------------------------------------------------------
" "Header to place in Tabspaces session file.")

(defcustom tabspaces-session t
  "Whether to save tabspaces across sessions."
  :group 'tabspaces
  :type 'boolean)

(defcustom tabspaces-session-auto-restore nil
  "Whether to restore tabspaces on session startup."
  :group 'tabspaces
  :type 'boolean)

(defcustom tabspaces-session-file (concat user-emacs-directory "tabsession.el")
  "File for saving tabspaces session."
  :group 'tabspaces
  :type 'string)

(defvar tabspaces--session-list nil
  "Store `tabspaces' session tabs and buffers.")

;; Helper functions
(defun tabspaces--store-buffers (bufs)
  "Make list of filenames."
  (flatten-tree (mapcar #'buffer-file-name bufs)))

;; Save global session
;;;###autoload
(defun tabspaces-save-session ()
  "Save tabspace name and buffers."
  (interactive)
  ;; Start from an empty list.
  (setq tabspaces--session-list nil)
  (let ((curr (tab-bar--current-tab-index)))
    ;; loop over tabs
    (cl-loop for tab in (tabspaces--list-tabspaces)
             do (progn
                  (tab-bar-select-tab-by-name tab)
                  (setq tabspaces--session-list
                        (append tabspaces--session-list
                                (list (cons (tabspaces--store-buffers (tabspaces--buffer-list)) tab))))))
    ;; As tab-bar-select-tab starts counting from 1, we need to add 1 to the index.
    (tab-bar-select-tab (+ curr 1)))
  ;; Write to file
  (with-temp-file tabspaces-session-file
    (point-min)
    (insert ";; -*- mode: emacs-lisp; lexical-binding:t; coding: utf-8-emacs; -*-\n"
            tabspaces-session-header
            ";; Created " (current-time-string) "\n\n"
            ";; Tabs and buffers:\n")
    (insert "(setq tabspaces--session-list '" (format "%S" tabspaces--session-list) ")")))

;; Save current project session
(defun tabspaces-save-current-project-session ()
  "Save tabspace name and buffers for current tab & project."
  (interactive)
  (let ((tabspaces--session-list nil) ;; Start from an empty list.
        (ctab (tabspaces--current-tab-name))
        (current-session (with-current-buffer (buffer-name)
                           (concat (vc-root-dir)
                                   "."
                                   (tabspaces--current-tab-name)
                                   "-tabspaces-session.el"))))
    ;; Get buffers
    (add-to-list 'tabspaces--session-list (cons (tabspaces--store-buffers (tabspaces--buffer-list))
                                                ctab))
    ;; Write to file
    (with-temp-file current-session
      (point-min)
      (insert ";; -*- mode: emacs-lisp; lexical-binding:t; coding: utf-8-emacs; -*-\n"
              tabspaces-session-header
              ";; Created " (current-time-string) "\n\n"
              ";; Tab and buffers:\n")
      (insert "(setq tabspaces--session-list '" (format "%S" tabspaces--session-list) ")"))))

;; Restore session
;;;###autoload
(defun tabspaces-restore-session (&optional session)
  "Restore tabspaces session."
  (interactive)
  (load-file (or session
                 tabspaces-session-file))
  ;; Start looping through the session list, but ensure to start from a
  ;; temporary buffer "*tabspaces--placeholder*" in order not to pollute the
  ;; buffer list with the final buffer from the previous tab.
  (cl-loop for elm in tabspaces--session-list do
           (switch-to-buffer "*tabspaces--placeholder*")
           (tabspaces-switch-workspace (cdr elm))
           (mapc #'find-file (car elm)))
  ;; Once the session list is restored, remove the temporary buffer from the
  ;; buffer list.
  (cl-loop for elm in tabspaces--session-list do
           (tabspaces-switch-workspace (cdr elm))
           (tabspaces-kill-buffer "*tabspaces--placeholder*"))
  ;; Finally, kill the temporary buffer to clean up.
  (kill-buffer "*tabspaces--placeholder*"))

(defun tabspaces--create-session-file ()
  "Create the tabspaces session file if not exists."
  (unless (file-exists-p tabspaces-session-file)
    (with-temp-buffer
      (write-file tabspaces-session-file))
    (message "Created tabspaces session file: `%s'." tabspaces-session-file)))

(defun tabspaces--restore-session-on-startup ()
  "Restore tabspaces session on startup.
Unlike the interactive restore, this function does more clean up to remove
unnecessary tab."
  (message "Restoring tabspaces session on startup.")
  (tabspaces--create-session-file)
  (load-file tabspaces-session-file)
  ;; Start looping through the session list, but ensure to start from a
  ;; temporary buffer "*tabspaces--placeholder*" in order not to pollute the
  ;; buffer list with the final buffer from the previous tab.
  (cl-loop for elm in tabspaces--session-list do
           (switch-to-buffer "*tabspaces--placeholder*")
           (tabspaces-switch-workspace (cdr elm))
           (mapc #'find-file (car elm)))
  ;; Once the session list is restored, remove the temporary buffer from the
  ;; buffer list.
  (cl-loop for elm in tabspaces--session-list do
           (tabspaces-switch-workspace (cdr elm))
           (tabspaces-kill-buffer "*tabspaces--placeholder*"))
  ;; If the tab restore started from an empty tab (e.g. at startup), remove the
  ;; tab by name of "*tabspaces--placeholder*".
  ;; NOTE When restore is interactively called, it is possible that an unnamed
  ;; tab to be incorrectly closed as we call `switch-to-buffer', which would
  ;; make the tab name to be "*tabspaces--placeholder*". At the startup, this
  ;; shouldn't be an issue, but conduct a simple check before closing the tab.
  (if (eq (tab-bar--tab-index-by-name "*tabspaces--placeholder*") 0)
      ;; tab-bar-close-tab counts from 1.
      (tab-bar-close-tab 1))
  ;; Finally, kill the temporary buffer to clean up.
  (kill-buffer "*tabspaces--placeholder*"))

;;;; Define Keymaps
(defvar tabspaces-mode-map
  (let ((prefix-map (make-sparse-keymap))
        (map (make-sparse-keymap)))
    (define-key prefix-map (kbd "b")   #'tabspaces-switch-to-buffer)
    (define-key prefix-map (kbd "C-b") #'tabspaces-switch-to-buffer)
    (define-key prefix-map (kbd "c")   #'tabspaces-clear-buffers)
    (define-key prefix-map (kbd "C-c") #'tabspaces-clear-buffers)
    (define-key prefix-map (kbd "f")   #'tabspaces-forget-workspace)
    (define-key prefix-map (kbd "C-f") #'tabspaces-forget-workspace)
    (define-key prefix-map (kbd "k")   #'tabspaces-kill-buffer)
    (define-key prefix-map (kbd "C-k") #'tabspaces-kill-buffer)
    (define-key prefix-map (kbd "l")   #'tabspaces-switch-workspace)
    (define-key prefix-map (kbd "C-l") #'tabspaces-switch-workspace)
    (define-key prefix-map (kbd "n")   #'tabspaces-rename-workspace)
    (define-key prefix-map (kbd "C-n") #'tabspaces-rename-workspace)
    (define-key prefix-map (kbd "o")   #'tabspaces-open-workspace)
    (define-key prefix-map (kbd "C-o") #'tabspaces-open-workspace)
    (define-key prefix-map (kbd "q")   #'tabspaces-close-workspace)
    (define-key prefix-map (kbd "C-q") #'tabspaces-close-workspace)
    (define-key prefix-map (kbd "s")   #'tabspaces-switch-workspace)
    (define-key prefix-map (kbd "C-s") #'tabspaces-switch-workspace)
    (define-key prefix-map (kbd "t")   #'tabspaces-switch-buffer-and-tab)
    (define-key prefix-map (kbd "C-t") #'tabspaces-switch-buffer-and-tab)
    (define-key prefix-map (kbd "z")   #'tabspaces-forget-zombie-workspaces)
    (define-key prefix-map (kbd "C-z") #'tabspaces-forget-zombie-workspaces)
    (define-key map tabspaces-keymap-prefix prefix-map)
    map)
  "Keymap for Tabspaces commands.")

;;;; Define Minor Mode
;;;###autoload
(define-minor-mode tabspaces-mode
  "Minor mode for buffer-isolated workspaces."
  :keymap tabspaces-mode-map
  :global t
  (if tabspaces-mode
      (progn
        (dolist (frame (frame-list))
          (tabspaces--set-buffer-predicate frame)
          (add-hook 'after-make-frame-functions #'tabspaces--set-buffer-predicate)
          (add-to-list 'tab-bar-tab-post-open-functions #'tabspaces--tab-post-open-function)
          (when tabspaces-use-filtered-buffers-as-default
            (define-key (current-global-map) [remap switch-to-buffer] #'tabspaces-switch-to-buffer)))
        (when tabspaces-session
          (add-hook 'kill-emacs-hook #'tabspaces-save-session))
        (when tabspaces-session-auto-restore
          (tabspaces--restore-session-on-startup)))
    (progn
      (dolist (frame (frame-list))
        (tabspaces--reset-buffer-predicate frame))
      (when tabspaces-use-filtered-buffers-as-default
        (define-key (current-global-map) [remap switch-to-buffer] nil))
      (setq tab-bar-tab-post-open-functions
            (remove #'tabspaces--tab-post-open-function tab-bar-tab-post-open-functions))
      (remove-hook 'after-make-frame-functions #'tabspaces--set-buffer-predicate)
      (remove-hook 'kill-emacs-hook #'tabspaces-save-session))))

(provide 'tabspaces)
;;; tabspaces.el ends here
