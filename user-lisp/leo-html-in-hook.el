;;
;; html-helper definitions (are loaded in html hook)
;;
;; new type
(html-helper-add-type-to-alist
    '(leos . (html-helper-leos-map "\C-c\C-o" 
				     html-helper-leos-menu 
				     "Insert Leos Styles")))
(html-helper-install-type 'leos)

;; new tags
(html-helper-add-tag
   '(leos    "s" "<span id=" "Spanning id" 
	     ("<span id=\"" (p "id: ") "\">" 'r "</span>")))
(html-helper-add-tag
   '(leos    "d" "<div id=" "Divning id" 
	     ("<div id=\"" (p "id: ") "\">" 'r "</div>")))

;; now rebuild the menu so all my new tags show up.
(html-helper-rebuild-menu)
