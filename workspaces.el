;;; workspaces.el --- Leverage tab-bar and project for buffer-isolated workspaces  -*- lexical-binding: t -*-

;; Author: Colin McLear <mclear@fastmail.com>
;; Maintainer: Mou Tong <mou.tong@qq.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (project "0.8.1"))
;; URL: https://github.com/dalugm/workspaces
;; Keywords: convenience, frames

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
;; "workspace"). The package assumes `project' and `tab-bar' are both
;; present (they are built-in to Emacs 27.1+).

;;; Acknowledgements
;; Much of the package code is inspired by:

;; - https://github.com/kaz-yos/emacs
;; - https://github.com/wamei/elscreen-separate-buffer-list/issues/8
;; - https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/
;; - https://github.com/minad/consult#multiple-sources
;; - https://github.com/florommel/bufferlo
;; - https://github.com/natecraddock/workspaces.nvim

;;; Code:

;;;; Requirements

(eval-when-compile (require 'cl-lib))
(require 'tab-bar)
(require 'project)
(require 'vc)
(require 'vc-git)
(require 'seq)

(declare-function magit-init "magit-status")
(declare-function magit-status-setup-buffer "magit-status")

;;;; Variables

(defgroup workspaces nil
  "Manage tab/workspace buffers."
  :group 'convenience)

(defcustom workspaces-include-buffers '("*scratch*" "*Messages*")
  "Buffers that should always get included in a new workspace.
This is a string list that matches buffer names, which does not
overrides buffers excluded by `workspaces-exclude-buffers'."
  :group 'workspaces
  :type '(repeat string))

(defcustom workspaces-exclude-buffers nil
  "Buffers that should always get excluded in a new workspace.
This is a string list that matches buffer names, which overrides
buffers inside `workspaces-include-buffers'."
  :group 'workspaces
  :type '(repeat string))

(defcustom workspaces-project-switch-commands project-switch-commands
  "Available commands when switch between projects.
Change this value if you wish to run a specific command, such as
`find-file' on project switch.  Otherwise this will default to
the value of `project-switch-commands'."
  :group 'workspaces
  :type 'sexp)

(defcustom workspaces-use-truepath nil
  "Whether resolve \".\", \"..\", etc. in project path."
  :group 'workspaces
  :type 'boolean)

;;;; Create Buffer Workspace

(defun workspaces--reset-buffer-list ()
  "Reset the current TAB's `buffer-list'.
Only the buffers in `workspaces-include-buffers' and buffers not
in `workspaces-exclude-buffers' are kept in the `buffer-list' and
`buried-buffer-list'."
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html
  ;; The current-tab uses `buffer-list' and `buried-buffer-list'.
  ;; A hidden tab keeps these as `wc-bl' and `wc-bbl'.
  (cl-flet* ((filter-fn (buffer)
               (and (member (buffer-name buffer) workspaces-include-buffers)
                    (not (member (buffer-name buffer) workspaces-exclude-buffers))))
             (update-frame (param)
               (set-frame-parameter
                nil
                param
                (seq-filter #'filter-fn (frame-parameter nil param)))))
    (update-frame 'buffer-list)
    (update-frame 'buried-buffer-list)))

(defun workspaces--tab-post-open-function (_tab)
  "Update buffer list on new tab creation."
  (workspaces--reset-buffer-list))

;;;; Filter Workspace Buffers

(defun workspaces--local-buffer-p (buffer)
  "Return whether BUFFER is in the list of local buffers."
  (or (member (buffer-name buffer) workspaces-include-buffers)
      (memq buffer (frame-parameter nil 'buffer-list))))

(defun workspaces--set-buffer-predicate (frame)
  "Set the buffer predicate of FRAME to `workspaces--local-buffer-p'."
  (set-frame-parameter frame 'buffer-predicate #'workspaces--local-buffer-p))

(defun workspaces--reset-buffer-predicate (frame)
  "Reset the buffer predicate of FRAME if it is `workspaces--local-buffer-p'."
  (when (eq (frame-parameter frame 'buffer-predicate) #'workspaces--local-buffer-p)
    (set-frame-parameter frame 'buffer-predicate nil)))

(defun workspaces--buffer-list (&optional frame index)
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
                               (cadr (assq #'workspaces--buffer-list (assq 'ws tab)))))))
                (frame-parameter frame 'buffer-list))))

;;;; Project Workspace Helper Functions

(defun workspaces--current-name ()
  "Get name of current workspace."
  (cdr (assq 'name (tab-bar--current-tab))))

(defun workspaces--list ()
  "Return a list of `tab-bar' tabs/workspaces."
  (mapcar (lambda (tab) (alist-get 'name tab))
          (funcall tab-bar-tabs-function)))

;;;; Interactive Functions

;;;;; Buffer Functions

(defun workspaces--kill-buffer (&optional buffer)
  "Bury and remove BUFFER from current workspace.
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
      (message (format "Buffer `%s' removed from `%s' workspace."
                       buffer (workspaces--current-name)))))
    (bury-buffer buffer)
    (delete buffer buffer-list)))

(defun workspaces-kill-buffer (buffer)
  "Remove selected BUFFER from frame's buffer list."
  (interactive
   (list
    (let ((blst (mapcar (lambda (b) (buffer-name b))
                        (workspaces--buffer-list))))
      ;; Select buffer.
      (read-buffer (format "Remove buffer from `%s' workspace: "
                           (workspaces--current-name))
                   nil t
                   (lambda (b) (member (car b) blst))))))
  ;; Remove buffer from current workspace's buffer list.
  (workspaces--kill-buffer buffer))

(defun workspaces-switch-to-buffer (buffer &optional norecord force-same-window)
  "Display the local buffer BUFFER in the selected window.
This is the frame/tab-local equivalent to `switch-to-buffer'.
The arguments NORECORD and FORCE-SAME-WINDOW are passed to `switch-to-buffer'."
  (interactive
   (list
    (let ((blst (cl-remove (buffer-name) (mapcar #'buffer-name (workspaces--buffer-list)))))
      (read-buffer
       "Switch to local buffer: " blst nil
       (lambda (b) (member (if (stringp b) b (car b)) blst))))))
  (switch-to-buffer buffer norecord force-same-window))

;; See https://emacs.stackexchange.com/a/53016/11934
(defun workspaces--report-dupes (xs)
  (let ((ys  ()))
    (while xs
      (unless (member (car xs) ys) ; Don't check it if already known to be a dup.
        (when (member (car xs) (cdr xs)) (push (car xs) ys)))
      (setq xs  (cdr xs)))
    ys))

(defun workspaces-switch-buffer-and-tab (buffer &optional norecord force-same-window)
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
         (bufflst (flatten-tree (dolist (tab (workspaces--list) buflst)
                                  (push (mapcar #'buffer-name (workspaces--buffer-list nil (tab-bar--tab-index-by-name tab)))
                                        buflst))))
         (dupe (member buffer (workspaces--report-dupes bufflst))))
    ;; Run through conditions:
    (cond
     ;; 1. Buffer exists and is not open in more than one workspace.
     ((and (get-buffer buffer)
           (not dupe))
      (dolist (tab (workspaces--list))
        (when (member buffer (mapcar #'buffer-name (workspaces--buffer-list nil (tab-bar--tab-index-by-name tab))))
          (progn (tab-bar-switch-to-tab tab)
                 (workspaces-switch-to-buffer buffer)))))
     ;; 2. Buffer exists and is open in more than one workspace.
     ((and (get-buffer buffer) dupe)
      (dolist (tab (workspaces--list) tabcand)
        (when (member buffer (mapcar #'buffer-name (workspaces--buffer-list nil (tab-bar--tab-index-by-name tab))))
          (push tabcand tab)))
      (tab-bar-switch-to-tab (completing-read "Select tab: " tabcand))
      (workspaces-switch-to-buffer buffer))
     ;; 3. Buffer does not exist.
     ((yes-or-no-p "Buffer not found -- create a new workspace with buffer?")
      (switch-to-buffer-other-tab buffer))
     ;; 4. Default -- create buffer in current workspace.
     (t
      (switch-to-buffer buffer norecord force-same-window)))))

(defun workspaces-clear-buffers (&optional frame)
  "Clear the workspace's buffer list, except for the current buffer.
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
(defun workspaces-switch (&optional workspace)
  "Switch to tab if it exists, otherwise create a new tabbed workspace."
  (interactive
   (if-let* ((tabs (workspaces--list)))
       (list (completing-read "Select or create workspace: " tabs))
     (tab-new)
     (tab-rename (completing-read "Workspace name: " tabs))))
  (if (member workspace (workspaces--list))
      (tab-bar-switch-to-tab workspace)
    (tab-new)
    (tab-rename workspace)))

;;;;; Forget Workspace
(defalias 'workspaces-forget-workspace #'project-forget-project)
(defalias 'workspaces-forget-zombie #'project-forget-zombie-projects)

;;;;; Rename Workspace
(defalias 'workspaces-rename #'tab-bar-rename-tab)

;;;;; Close Workspace & Kill Buffers
(defun workspaces-close (workspace)
  "Kill all buffers and close current WORKSPACE.
When with a \\[universal-argument], select a WORKSPACE to close."
  (interactive
   (list (if (equal current-prefix-arg '(4))
             (completing-read "Close workspace: " (workspaces--list))
           (workspaces--current-name))))
  (if (= 1 (length (workspaces--list)))
      (user-error "Attempt to close the sole workspace")
    (let ((buf-lst (workspaces--buffer-list
                    nil
                    (tab-bar--tab-index-by-name workspace))))
      (unwind-protect
          (cl-loop for buf in buf-lst
                   do (unless (member (buffer-name buf)
                                      workspaces-include-buffers)
                        (kill-buffer buf)))
        (tab-bar-close-tab-by-name workspace)))))

;;;;; Open project in workspace.
(defun workspaces--generate-name (base-name existed-workspaces)
  "Generate a unique numbered tab name."
  (let ((counter 2)
        (new-name base-name))
    (while (member new-name existed-workspaces)
      (setq new-name (format "%s<%d>" base-name counter)
            counter (+ counter 1)))
    new-name))

;;;###autoload
(defun workspaces-open (&optional project prefix)
  "Open PROJECT and its workspace with a descriptive name.

With universal argument PREFIX, always create a new workspace."
  (interactive (list (project-prompt-project-dir)))
  (let* ((project-switch-commands workspaces-project-switch-commands)
         (project-dir (if workspaces-use-truepath
                          (expand-file-name project)
                        project))
         (existed-workspaces (workspaces--list))
         (ws-name (or (car (member project-dir existed-workspaces))
                      (workspaces--generate-name project-dir existed-workspaces)))
         (dir-potential-project (project--find-in-directory project-dir))
         (project-existed-p (member (list project-dir) project--list))
         (create-ws-p (or prefix (not (member ws-name existed-workspaces)))))
    (cond
     ;; If there is no workspace nor project, create both.
     ((not project-existed-p)
      (tab-bar-new-tab)
      (tab-bar-rename-tab ws-name)
      (project-switch-project project-dir)
      (let ((default-directory project-dir))
        (if (featurep 'magit)
            (magit-init project-dir)
          (call-interactively #'vc-create-repo))
        (delete-other-windows)
        (if (featurep 'magit)
            (magit-status-setup-buffer project-dir)
          (project-vc-dir)))
      ;; Remember new project
      (let ((pr (project--find-in-directory default-directory)))
        (project-remember-project pr)))

     ;; If project and workspace exists, but we want a new workspace.
     ((and project-existed-p (member ws-name existed-workspaces) create-ws-p)
      (let ((new-ws-name (workspaces--generate-name ws-name existed-workspaces)))
        (tab-bar-new-tab)
        (tab-bar-rename-tab new-ws-name)
        (project-switch-project project-dir)
        (setq ws-name new-ws-name)))

     ;; If project and workspace exists.
     ((and project-existed-p (member ws-name existed-workspaces))
      (project-switch-project project-dir)
      (tab-bar-switch-to-tab ws-name))

     ;; If project exists, but no corresponding workspace, create a
     ;; new workspace.
     (project-existed-p
      (tab-bar-new-tab)
      (tab-bar-rename-tab ws-name)
      (project-switch-project project-dir))

     (t
      (message "No project found or created.")
      nil))))

;;;; Define Keymaps
(defvar-keymap workspaces-prefix-map
  :doc "Keymap for workspaces commands.
The keymap should be installed globally under a prefix."
  "b"   #'workspaces-switch-to-buffer
  "C-b" #'workspaces-switch-to-buffer
  "c"   #'workspaces-clear-buffers
  "C-c" #'workspaces-clear-buffers
  "f"   #'workspaces-forget-workspace
  "C-f" #'workspaces-forget-workspace
  "k"   #'workspaces-kill-buffer
  "C-k" #'workspaces-kill-buffer
  "l"   #'workspaces-switch
  "C-l" #'workspaces-switch
  "n"   #'workspaces-rename
  "C-n" #'workspaces-rename
  "o"   #'workspaces-open
  "C-o" #'workspaces-open
  "q"   #'workspaces-close
  "C-q" #'workspaces-close
  "s"   #'workspaces-switch
  "C-s" #'workspaces-switch
  "t"   #'workspaces-switch-buffer-and-tab
  "C-t" #'workspaces-switch-buffer-and-tab
  "z"   #'workspaces-forget-zombie
  "C-z" #'workspaces-forget-zombie)

;;;###autoload (autoload 'workspaces-prefix-map "workspaces" nil t 'keymap)
(defalias 'workspaces-prefix-map workspaces-prefix-map)

;;;###autoload
(define-minor-mode workspaces-mode
  "Minor mode for buffer-isolated workspaces."
  :global t
  (if workspaces-mode
      (progn
        (dolist (frame (frame-list))
          (workspaces--set-buffer-predicate frame)
          (add-hook 'after-make-frame-functions #'workspaces--set-buffer-predicate)
          (add-to-list 'tab-bar-tab-post-open-functions #'workspaces--tab-post-open-function)))
    (progn
      (dolist (frame (frame-list))
        (workspaces--reset-buffer-predicate frame))
      (setq tab-bar-tab-post-open-functions
            (remove #'workspaces--tab-post-open-function tab-bar-tab-post-open-functions))
      (remove-hook 'after-make-frame-functions #'workspaces--set-buffer-predicate))))

(provide 'workspaces)
;;; workspaces.el ends here
