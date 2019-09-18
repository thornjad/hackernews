;;; hackernews.el --- Hacker News Emacs Client -*- lexical-binding: t -*-
;;
;; Copyright (C) 2019 Jade Michael Thornton
;; Copyright (C) 2012-2018 The Hackernews.el Authors
;;
;; This file is not part of GNU Emacs

(require 'browse-url)
(require 'cus-edit)
(require 'format-spec)
(require 'url)

(defgroup hackernews nil
  "Simple Hacker News client."
  :group 'external
  :prefix "hackernews-")

;;;; Faces

(defface hackernews-link
  '((t :inherit link :underline nil))
  "Face used for links to stories."
  :package-version '(hackernews . "1.0.0")
  :group 'hackernews)

(defface hackernews-link-visited
  '((t :inherit link-visited :underline nil))
  "Face used for visited links to stories."
  :package-version '(hackernews . "1.0.0")
  :group 'hackernews)

;;;; User options

(defcustom hackernews-items-per-page nil
  "Default number of stories to retrieve in one go.
If nil, the window will be filled with stories."
  :package-version '(hackernews . "1.1.0")
  :group 'hackernews
  :type 'integer)

(defcustom hackernews-item-format "%t\n"
  "Format string for items in buffers. t is the item title."
  :package-version '(hackernews . "1.0.0")
  :group 'hackernews
  :type 'string)

(defcustom hackernews-preserve-point t
  "Whether to preserve point when loading more stories.
When nil, point is placed on first new item retrieved."
  :package-version '(hackernews . "1.0.0")
  :group 'hackernews
  :type 'boolean)

(defcustom hackernews-before-render-hook ()
  "Hook called before rendering any new items."
  :package-version '(hackernews . "1.0.0")
  :group 'hackernews
  :type 'hook)

(defcustom hackernews-after-render-hook ()
  "Hook called after rendering any new items.
The position of point will not have been affected by the render."
  :package-version '(hackernews . "1.0.0")
  :group 'hackernews
  :type 'hook)

(defcustom hackernews-finalize-hook ()
  "Hook called as final step of loading any new items.
The position of point may have been adjusted after the render,
buffer-local feed state will have been updated and the hackernews
buffer will be current and displayed in the selected window."
  :package-version '(hackernews . "1.0.0")
  :group 'hackernews
  :type 'hook)

(defcustom hackernews-suppress-url-status t
  "Whether to suppress messages controlled by `url-show-status'.
When nil, `url-show-status' determines whether certain status
messages are displayed when retrieving online data.  This is
suppressed by default so that the hackernews progress reporter is
not interrupted."
  :package-version '(hackernews . "1.0.0")
  :group 'hackernews
  :type 'boolean)

(defcustom hackernews-show-visited-links t
  "Whether to visually distinguish links that have been visited.
For example, when a link with the `hackernews-link' face is
visited and the value of this variable is non-nil, that link's
face is changed to `hackernews-link-visited'."
  :package-version '(hackernews . "1.0.0")
  :group 'hackernews
  :type 'boolean)

(defcustom hackernews-visited-links-file
  (locate-user-emacs-file "hackernews/visited-links.el")
  "Name of file used to remember which links have been visited.
When nil, visited links are not persisted across sessions."
  :package-version '(hackernews . "1.0.0")
  :group 'hackernews
  :type '(choice file (const :tag "None" nil)))

;;;; Internal definitions

(defconst hackernews-api-version "v0"
  "Currently supported version of the Hacker News API.")

(defconst hackernews-api-format
  (format "https://hacker-news.firebaseio.com/%s/%%s.json"
          hackernews-api-version)
  "Format of targeted Hacker News API URLs.")

(defconst hackernews-site-item-format "https://news.ycombinator.com/item?id=%s"
  "Format of Hacker News website item URLs.")

(defvar hackernews--feed-state ()
  "Plist capturing state of current buffer's Hacker News feed.
:items    - Vector holding items being or last fetched.
:register - Cons of number of items currently displayed and
            vector of item IDs last read from this feed.
            The `car' is thus an offset into the `cdr'.")
(make-variable-buffer-local 'hackernews--feed-state)

(defvar hackernews-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'hackernews-reload)
    (define-key map "m" #'hackernews-load-more-stories)
    (define-key map "n" #'hackernews-next-item)
    (define-key map "j" #'hackernews-next-item)
    (define-key map "p" #'hackernews-previous-item)
    (define-key map "k" #'hackernews-previous-item)
    map)
  "Keymap used in hackernews buffer.")

(defvar hackernews-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map "R" #'hackernews-button-mark-as-unvisited)
    (define-key map "r" #'hackernews-button-mark-as-visited)
    map)
  "Keymap used on hackernews links.")

(define-button-type 'hackernews-link
  'action #'hackernews-browse-url-action
  'follow-link t
  'hackernews-face 'hackernews-link
  'hackernews-visited-face 'hackernews-link-visited
  'keymap hackernews-button-map)

;; Use `font-lock-face' on creation instead.
(button-type-put 'hackernews-link 'face nil)

(defvar hackernews--visited-ids
  (mapcar #'list '(hackernews-link))
  "Map link button types to their visited ID sets.
Values are initially nil and later replaced with a hash table.")

;; Emulate `define-error' for Emacs < 24.4.
(put 'hackernews-error 'error-conditions '(hackernews-error error))
(put 'hackernews-error 'error-message "hackernews error")

;;;; Utils

(defun hackernews--get (prop)
  "Extract value of PROP from `hackernews--feed-state'."
  (plist-get hackernews--feed-state prop))

(defun hackernews--put (prop val)
  "Change value in `hackernews--feed-state' of PROP to VAL."
  (setq hackernews--feed-state (plist-put hackernews--feed-state prop val)))

(defun hackernews--format-api-url (fmt &rest args)
  "Construct a Hacker News API URL.
The result of passing FMT and ARGS to `format' is substituted in
`hackernews-api-format'."
  (format hackernews-api-format (apply #'format fmt args)))

(defun hackernews--item-url (id)
  "Return Hacker News API URL for item with ID."
  (hackernews--format-api-url "item/%s" id))

(defun hackernews--feed-url ()
  "Return Hacker News API URL for FEED.
See `hackernews-feed-names' for supported values of FEED."
  (hackernews--format-api-url "topstories"))

;;;; Motion

(defun hackernews--forward-button (n type)
  "Move to Nth next button of TYPE (previous if N is negative)."
  (let ((pos  (point))
        (sign (cond ((> n 0)  1)
                    ((< n 0) -1)
                    (t        0)))
        msg)
    (while (let ((button (ignore-errors (forward-button sign))))
             (when button
               (when (button-has-type-p button type)
                 (setq pos (button-start button))
                 (setq msg (button-get button 'help-echo))
                 (setq n   (- n sign)))
               (/= n 0))))
    (goto-char pos)
    (when msg (message "%s" msg))))

(defun hackernews-next-item (&optional n)
  "Move to Nth next story link (previous if N is negative).
N defaults to 1."
  (interactive "p")
  ;; N is kept optional for backward compatibility
  (hackernews--forward-button (or n 1) 'hackernews-link))

(defun hackernews-previous-item (&optional n)
  "Move to Nth previous story link (next if N is negative).
N defaults to 1."
  (interactive "p")
  (hackernews-next-item (- (or n 1))))

(defun hackernews-first-item ()
  "Move point to first story link in hackernews buffer."
  (interactive)
  (goto-char (point-min))
  (hackernews-next-item))

;;;; UI

(defun hackernews--read-visited-links ()
  "Read and return contents of `hackernews-visited-links-file'.
On error, display a warning for the user and return nil."
  (when (and hackernews-visited-links-file
             (file-exists-p hackernews-visited-links-file))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents hackernews-visited-links-file)
          (read (current-buffer)))
      (error
       (ignore
        (lwarn 'hackernews :error
               "Could not read `hackernews-visited-links-file':\n      %s%s"
               (error-message-string err)
               (substitute-command-keys "
N.B.  Any valid data in the file will be overwritten next time
      Emacs is killed.  To avoid data loss, type
      \\[hackernews-load-visited-links] after fixing the error
      above.
      Alternatively, you can set `hackernews-visited-links-file'
      to nil: the file will not be overwritten, but any links
      visited in the current Emacs session will not be saved.")))))))

(defun hackernews-load-visited-links ()
  "Merge visited links on file with those in memory.
This command tries to reread `hackernews-visited-links-file',
which may be useful when, for example, the contents of the file
change and you want to update the hackernews display without
restarting Emacs, or the file could not be read initially and
risks being overwritten next time Emacs is killed."
  (interactive)
  ;; Ensure `hackernews--visited-ids' is initialized
  (dolist (entry hackernews--visited-ids)
    (unless (cdr entry)
      (setcdr entry (make-hash-table))))
  ;; Merge with `hackernews-visited-links-file'
  (dolist (entry (hackernews--read-visited-links))
    (let ((table (cdr (assq (car entry) hackernews--visited-ids))))
      (maphash (lambda (k newv)
                 (let ((oldv (gethash k table)))
                   (when (or (not oldv)
                             (time-less-p (plist-get oldv :last-visited)
                                          (plist-get newv :last-visited)))
                     (puthash k newv table))))
               (cdr entry)))))

(defun hackernews-save-visited-links ()
  "Write visited links to `hackernews-visited-links-file'."
  (when hackernews-visited-links-file
    (condition-case err
        (with-temp-file hackernews-visited-links-file
          (let ((dir (file-name-directory hackernews-visited-links-file)))
            ;; Ensure any parent directories exist
            (when dir (make-directory dir t)))
          (hackernews-load-visited-links)
          (prin1 hackernews--visited-ids (current-buffer)))
      (error (lwarn 'hackernews :error
                    "Could not write `hackernews-visited-links-file': %s"
                    (error-message-string err))))))

(defun hackernews--init-visited-links ()
  "Set up tracking of visited links.
Do nothing if `hackernews--visited-ids' is already initialized."
  (unless (cdar hackernews--visited-ids)
    (hackernews-load-visited-links)
    (add-hook 'kill-emacs-hook #'hackernews-save-visited-links)))

(defun hackernews--visit (button fn &optional unvisit)
  "Visit URL of BUTTON by passing it to FN.
If UNVISIT is non-nil, mark BUTTON as unvisited."
  (let* ((id    (button-get button 'id))
         (type  (button-type button))
         (face  (cond (unvisit 'hackernews-face)
                      (hackernews-show-visited-links
                       'hackernews-visited-face)))
         (table (cdr (assq type hackernews--visited-ids)))
         (val   (gethash id table))
         (val   (plist-put val :visited      (not unvisit)))
         (val   (plist-put val :last-visited (current-time)))
         (inhibit-read-only t))
    (puthash id val table)
    (when face
      (button-put button 'font-lock-face (button-type-get type face))))
  (funcall fn (button-get button 'shr-url)))

(defun hackernews-browse-url-action (button)
  "Pass URL of BUTTON to `browse-url'."
  (hackernews--visit button #'browse-url))

(defun hackernews-button-mark-as-visited ()
  "Mark button under point as visited."
  (interactive)
  (hackernews--visit (point) #'ignore))

(defun hackernews-button-mark-as-unvisited ()
  "Mark button under point as unvisited."
  (interactive)
  (hackernews--visit (point) #'ignore t))

(defun hackernews--button-string (type label url id)
  "Make LABEL a text button of TYPE for item ID and URL."
  (let* ((props (and hackernews-show-visited-links
                     (gethash id (cdr (assq type hackernews--visited-ids)))))
         (face  (button-type-get type (if (plist-get props :visited)
                                          'hackernews-visited-face
                                        'hackernews-face))))
    (make-text-button label nil
                      'type type 'font-lock-face face
                      'id id 'help-echo url 'shr-url url))
  label)

(defun hackernews--render-item (item)
  "Render Hacker News ITEM in current buffer."
  (let* ((id (cdr (assq 'id item)))
         (title (cdr (assq 'title item)))
         (item-url (cdr (assq 'url item))))
    (insert
     (hackernews--button-string
      'hackernews-link
      title
      item-url
      id))))

(defun hackernews--display-items ()
  "Render items associated with, and pop to, the current buffer."
  (let* ((reg (hackernews--get :register))
         (items (hackernews--get :items))
         (nitem (length items))
         (inhibit-read-only t))

    ;; Render items
    (run-hooks 'hackernews-before-render-hook)
    (save-excursion
      (goto-char (point-max))
      (mapc #'hackernews--render-item items))
    (run-hooks 'hackernews-after-render-hook)

    ;; Adjust point
    (unless (or (<= nitem 0) hackernews-preserve-point)
      (goto-char (point-max))
      (hackernews-previous-item nitem))

    ;; Persist new offset
    (setcar reg (+ (car reg) nitem)))

  (pop-to-buffer (current-buffer))
  (run-hooks 'hackernews-finalize-hook))

;; TODO: Derive from `tabulated-list-mode'?
(define-derived-mode hackernews-mode special-mode "HN"
  "Mode for browsing Hacker News.

Summary of key bindings:

key		binding
---		-------
\\<hackernews-button-map>
\\[push-button]\
		Open link at point in default (external) browser.
\\<hackernews-mode-map>
\\[hackernews-next-item]\
		Move to next title link.
\\[hackernews-previous-item]\
		Move to previous title link.
\\[hackernews-load-more-stories]\
		Load more stories.
\\[hackernews-reload]\
		Reload stories.
\\[quit-window]\
		Quit.

Official major mode key bindings:

\\{hackernews-mode-map}"
  :group 'hackernews
  (setq hackernews--feed-state ())
  (setq truncate-lines t)
  (buffer-disable-undo))

(defun hackernews--ensure-major-mode ()
  "Barf if current buffer is not derived from `hackernews-mode'."
  (unless (derived-mode-p #'hackernews-mode)
    (signal 'hackernews-error '("Not a hackernews buffer"))))

;;;; Retrieval

(defalias 'hackernews--parse-json
  (if (fboundp 'json-parse-buffer)
      (lambda ()
        (json-parse-buffer :object-type 'alist))
    (require 'json)
    (defvar json-array-type)
    (defvar json-object-type)
    (declare-function json-read "json")
    (lambda ()
      (let ((json-array-type  'vector)
            (json-object-type 'alist))
        (json-read))))
  "Read JSON object from current buffer starting at point.
Objects are decoded as alists and arrays as vectors.")

(defun hackernews--read-contents (url)
  "Retrieve and read URL contents with `hackernews--parse-json'."
  (with-temp-buffer
    (let ((url-show-status (unless hackernews-suppress-url-status
                             url-show-status)))
      (url-insert-file-contents url)
      (hackernews--parse-json))))

(defun hackernews--retrieve-items ()
  "Retrieve items associated with current buffer."
  (let* ((items (hackernews--get :items))
         (reg (hackernews--get :register))
         (nitem (length items))
         (offset (car reg))
         (ids (cdr reg)))
    (dotimes-with-progress-reporter (i nitem)
        (format "Retrieving %d..." nitem)
      (aset items i (hackernews--read-contents
                     (hackernews--item-url (aref ids (+ offset i))))))))

(defun hackernews--load-stories (n &optional append)
  "Retrieve and render at most N items.
Create and setup corresponding hackernews buffer if necessary.

If APPEND is nil, refresh the list of items and render at most N of its top
items. Any previous hackernews buffer contents are overwritten.

Otherwise, APPEND should be a cons cell (OFFSET . IDS), where IDS is the vector
of item IDs and OFFSET indicates where in IDS the previous retrieval and render
left off. At most N items starting at OFFSET are then rendered at the end of the
hackernews buffer."
  ;; TODO allow negative N?
  ;; TODO make asynchronous?
  (hackernews--init-visited-links)
  (let* (
         (offset (or (car append) 0))
         (ids    (if append
                     (cdr append)
                   ;; Display initial progress message before blocking
                   ;; to retrieve ID vector
                   (message "Retrieving links...")
                   (hackernews--read-contents (hackernews--feed-url)))))

    (with-current-buffer (get-buffer-create (format "*hackernews*"))
      (unless append
        (let ((inhibit-read-only t))
          (erase-buffer))
        (hackernews-mode))

      (hackernews--put :register (cons offset ids))
      (hackernews--put :items    (make-vector
                                  (max 0 (min (- (length ids) offset)
                                              (cond
                                               (n (prefix-numeric-value n))
                                               (hackernews-items-per-page)
                                               (t (1- (window-text-height))))))
                                  ()))

      (hackernews--retrieve-items)
      (hackernews--display-items))))

;;;; Feeds

;;;###autoload
(defun hackernews (&optional n)
  "Read top N Hacker News stories."
  (interactive "P")
  (hackernews--load-stories n))

(defun hackernews-reload (&optional n)
  "Reload top N Hacker News stories from current feed.
N defaults to `hackernews-items-per-page'."
  (interactive "P")
  (hackernews--ensure-major-mode)
  (hackernews--load-stories n))

(defun hackernews-load-more-stories (&optional n)
  "Load N more stories into hackernews buffer.
N defaults to `hackernews-items-per-page'."
  (interactive "P")
  (hackernews--ensure-major-mode)
  (let ((reg  (hackernews--get :register)))
    (unless reg
      (signal 'hackernews-error '("Buffer in invalid state")))
    (if (>= (car reg) (length (cdr reg)))
        (message "%s" (substitute-command-keys "\
End of feed; type \\[hackernews-reload] to load new items."))
      (hackernews--load-stories n reg))))

(provide 'hackernews)
