(require 'org)
(require 'ox)
(require 'ox-html)
(defun org-html--format-image (source attributes info)
  "Fix org's implementation, no check for svg files (object is deprecated at 
    this point and not trusted by most browsers)"
    (org-html-close-tag
     "img"
     (org-html--make-attribute-string
      (org-combine-plists
       (list :src source
             :alt (if (string-match-p "^ltxpng/" source)
                      (org-html-encode-plain-text
                       (org-find-text-property-in-string 'org-latex-src source))
                    (file-name-nondirectory source)))
       attributes))
     info))

(require 's)
(require 'dash)
(require 'org-element)
(require 'org-ref)

(org-export-define-derived-backend 'pelican-html 'html
  :translate-alist '((src-block .  pelican/pygments-org-html-code)
                     (example-block . pelican/pygments-org-html-code)
                     ))

(defvar pygments-path "pygmentize")

(defun pelican/pygments-org-html-code (code contents info)
  "Process code block with Pygments
See http://pygments.org/ for details"

  ;; Generate temp file path by hashing the code
  (setq temp-source-file (format "/tmp/pygmentize-%s.txt"(md5 (org-element-property :value code))))
  (setq temp-result-file (format "/tmp/pygmentize-%s.html"(md5 (org-element-property :value code))))
  ;; if file exists use that (code change will have different hash)
  (when (not (file-readable-p temp-result-file))
    ;; Writing temp file
    (with-temp-file temp-source-file (insert (org-element-property :value code)))
    ;; Processing
    (with-temp-file temp-result-file (insert (shell-command-to-string (format "%s -l \"%s\" -f html %s"
                                                                    pygments-path
                                                                    (or (org-element-property :language code)
                                                                        "")
                                                                    temp-source-file))))
  )
	(format "<div class=\"src-block\">\n%s%s\n</div>"
        ;; read results
        (with-temp-buffer
          (insert-file-contents temp-result-file)
          (buffer-string))
		;; Build caption.
		(let ((caption (org-export-get-caption code)))
		  (if (not caption) ""
			(let ((listing-number
			   (format
				"<span class=\"listing-number\">%s </span>"
				(format
				 (org-html--translate "Source block %d:" info)
				 (org-export-get-ordinal
				  code info nil #'org-html--has-caption-p)))))
			  (format "<label class=\"org-src-name\">%s%s</label>"
				  listing-number
				  (org-trim (org-export-data caption info))))))
      )
	)


(provide 'pelican-html)
