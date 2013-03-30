;;
;; rtm stuff 
;;
(require 'simple-rtm)
;;(autoload 'simple-rtm-mode "simple-rtm" "Interactive mode for Remember The Milk" t)

(defun leo-simple-rtm-remove-archived-lists ()
  "Removes from `simple-rtm-lists` the lists with the \"archived\" property set to \"1\""
  (with-current-buffer (simple-rtm--buffer)
    (let ((newlist nil))
      (dolist (elt simple-rtm-lists)
        (let ((props (nth 1 elt)))
          (unless (equal (cdr (assq 'archived props)) "1")
            (push elt newlist))))
      (setq simple-rtm-lists newlist))))
  
(defun simple-rtm-reload ()
  "Reload tasks from Remember The Milk.

-- Changed by leo: ---
Reload only non-archived tasks."
  (interactive)
  (with-current-buffer (simple-rtm--buffer)
    (unless simple-rtm-lists
      (setq simple-rtm-lists (rtm-lists-get-list)))
    (unless simple-rtm-locations
      (simple-rtm--load-locations))
    (setq simple-rtm-tasks (rtm-tasks-get-list nil "status:incomplete"))
    
    (leo-simple-rtm-remove-archived-lists)

    (simple-rtm--build-data)
    (simple-rtm-redraw)
    (simple-rtm--update-mode-line-string)))

(defun simple-rtm--render-task (task)
  "Render the task in the lists buffer.

-- Changed by leo: ---
Move tags behind title."
  (let* ((taskseries-node (getf task :xml))
         (task-node (car (xml-get-children taskseries-node 'task)))
         (priority (xml-get-attribute task-node 'priority))
         (priority-str (if (string= priority "N")
                           "  "
                         (propertize (concat "P" priority)
                                     'face (intern (concat "simple-rtm-task-priority-" priority)))))
         (name (getf task :name))
         (url (xml-get-attribute taskseries-node 'url))
         (location (simple-rtm--find-location-by 'id (xml-get-attribute taskseries-node 'location_id)))
         (duedate (simple-rtm--task-duedate task-node))
         (time-estimate (xml-get-attribute task-node 'estimate))
         (num-notes (length (xml-get-children (car (xml-get-children taskseries-node 'notes))
                                              'note)))
         (tags (getf task :tags))
         (tags-str (if tags
                       (mapconcat (lambda (tag)
                                    (propertize tag 'face 'simple-rtm-task-tag))
                                  tags " ")))
         (today (format-time-string "%Y-%m-%d")))
    (insert (propertize (concat (mapconcat 'identity
                                           (delq nil
                                                 (list ""
                                                       (if (getf task :marked) "*" " ")
                                                       priority-str
                                                       (if duedate
                                                           (propertize (simple-rtm--format-duedate duedate)
                                                                       'face (if (string< today duedate)
                                                                                 'simple-rtm-task-duedate
                                                                               'simple-rtm-task-duedate-due)))
                                                       (propertize name 'face 'simple-rtm-task)
                                                       (if (not (string= time-estimate ""))
                                                           (propertize time-estimate 'face 'simple-rtm-task-time-estimate))
                                                       tags-str
                                                       (if (not (string= url ""))
                                                           (propertize url 'face 'simple-rtm-task-url))
                                                       (if location
                                                           (propertize (concat "@" (xml-get-attribute location 'name))
                                                                       'face 'simple-rtm-task-location))
                                                       (if (> num-notes 0)
                                                           (propertize (format "[%d]" num-notes)
                                                                       'face 'simple-rtm-note-title))
                                                       ))
                                           " ")
                                "\n")
                        :list-id (getf task :list-id)
                        :task-id (getf task :id)))))
