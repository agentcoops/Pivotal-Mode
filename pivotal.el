;; pivotal.el --- An emacs pivotal tracker client.

;; Author: Cooper Francis, cfrancis@qmedtrix.com
;; Keywords: organization, todo, agile, clients, pivotal-tracker

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as1
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
(require 'http-post-simple)
(require 'xml)

(defconst pivotal-url "https://www.pivotaltracker.com/services/v3/"
  "Base url for pivotal tracker api.")

(defvar pivotal-user-id nil
  "Contains your pivotal tracker user id upon authentication.")
(defvar pivotal-token nil
  "Contains your pivotal tracker token upon authentication.")

(defun pivotal-api-get (call)
  "Makes a GET call to the pivotal-tracker api. Returns SEXP of xml."
  (parse-xml-string (pivotal-api-get-xml call)))

(defun pivotal-api-get-xml (call)
  (let ((api-url (concat pivotal-url call)))
    (car (http-get-simple
          api-url
          (list (cons (quote "X-TrackerToken") (pivotal-get-token)))))))

(defun pivotal-set-ids ()
  (let ((ids (pivotal-parse-credentials-xml (pivotal-get-credentials-xml))))
    (setq pivotal-token (car ids))
    (setq pivotal-user-id (cdr ids))))

(defun pivotal-get-token ()
  (if (string= pivotal-token nil)
      (progn (pivotal-set-ids)
             (pivotal-get-token))
    pivotal-token))

(defun pivotal-get-projects ()
  (pivotal-api-get "projects"))

(defun pivotal-get-project-stories (story-id)
  (pivotal-api-get (concat "projects/" story-id "/stories/")))

(defun pivotal-get-credentials-xml ()
  "Retrieve the pivotal tracker credentials xml.
   Throws an error on non-200 http responses."
  (interactive)
  (let* ((url (concat pivotal-url "tokens/active"))
         (response
          (http-post-simple
           url
           (list (cons 'username (read-from-minibuffer "Username: "))
                 (cons 'password (read-from-minibuffer "Password: ")))
           'utf-8)))
    (if (= (nth 2 response) 200)
        (parse-xml-string (car response))
      (error "Wrong username or password."))))

(defun pivotal-parse-credentials-xml (credentials-xml)
  "Parse pivotal tracker xml for guid and id."
  (cons
   (get-xml-element credentials-xml 'guid)
   (get-xml-element credentials-xml 'id)))

(defun pivotal-status-to-org-status (status)
  (cond
   ((string= status "accepted") "DONE")
   ((string= status "started") "TODO")
   ((string= status "unstarted") "TODO")
   ((string= status "finished") "DONE")
   ((string= status "delivered") "DONE")
   ((string= status "unscheduled") "")
   (t status)))

(defun pivotal-story-to-org (story-sexp)
  (let ((id  (get-story-element 'id story-sexp))
        (name (get-story-element 'name story-sexp))
        (type (get-story-element 'story_type story-sexp))
        (estimate (get-story-element 'estimate story-sexp))
        (status (get-story-element 'current_state story-sexp))
        (user (get-story-element 'owned_by story-sexp)))
    (pivotal-format-org-line
     (pivotal-status-to-org-status status) name user type estimate)))

(defun pivotal-format-org-line (state name user type priority)
  (format "*** %s [#%s] %s :%s:%s:\n" state priority name type user))

(defun pivotal-display-project-stories (project-id)
  (let ((new-stories (pivotal-get-project-stories project-id)))
    (dolist (elt new-stories nil)
      (cond
       ((eq elt nil) nil)
       ((stringp elt) nil)
       ((listp elt) (insert (pivotal-story-to-org elt)))))))

(defun pivotal-init-buffer ()
  (insert "* Pipeline\n")
  (insert "** Current\n"))

(defun pivotal-mode ()
  (interactive)
  (pop-to-buffer "*Pivotal*" nil)
  (kill-all-local-variables)
  (org-mode)
  (pivotal-init-buffer)
  (pivotal-display-project-stories "38357"))

;;  *** TODO system-wide profiling :feature: 
;;    :PROPERTIES:
;;    :id:    blahblahlah
;;    :project_id:
;;    :url:
;;    :estimate:
;;    :description:
;;    :requested_by:
;;    :owned_by:
;;    :created_at:
;;    :updated_at: 


;; HELPER FUNCTIONS.
(defun http-get-simple (url &optional headers)
  "Makes an http GET request to URL with a set of optional headers."
  (let ((url-request-method "GET")
        (url-request-extra-headers headers))
    (let (header
          data)
      (with-current-buffer
          (url-retrieve-synchronously url)
        (goto-char (point-min))
        (if (search-forward-regexp "^$" nil t)
            (setq header (buffer-substring (point-min) (point))
                  data   (buffer-substring (1+ (point)) (point-max)))
          (setq data (buffer-string))))
      (cons data header))))

(defun parse-xml-string (xml-string)
  "Parses a string of xml into a nice 'xml sexp"
  (let ((root (with-temp-buffer
                 (insert xml-string)
                 (xml-parse-region (point-min) (point-max)))))
    (car root)))

(defun get-xml-element (xml-sexp field)
  (nth 2 (car (xml-get-children xml-sexp field))))

(defun get-story-element (field xml-sexp)
  (nth 2 (assq field xml-sexp)))

(provide 'pivotal-mode)
