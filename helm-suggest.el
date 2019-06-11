;;; helm-suggest.el --- Web suggest (autocomplete) with helm  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/helm-suggest
;; Package-Requires: ((emacs "25.1") (helm-core "3.0"))
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
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

;; Some websites provides suggest (autocomplete) feature while you're typing,
;; let's use that in helm.

;;; Code:

(require 'helm)
(require 'url)
(require 'json)
(require 'subr-x)                       ; `string-trim'
(require 'dom)

(defgroup helm-suggest nil
  "Web suggest (autocomplete) with helm."
  :group 'helm)

(defcustom helm-suggest-input-idle-delay 0.6
  "Same as `helm-input-idle-delay' but for helm suggest commands.
It have a higher value than `helm-input-idle-delay' to avoid
too many requests."
  :type 'float)

(defun helm-suggest--json-read ()
  (let ((json-object-type 'alist)
        (json-array-type  'list)
        (json-key-type    'symbol)
        (json-false       nil)
        (json-null        nil))
    (json-read)))

(defun helm-suggest--url-retrieve-sync (url parser)
  (with-current-buffer
      (let ((url-show-status nil))
        (url-retrieve-synchronously url))
    (set-buffer-multibyte t)
    ;; (goto-char url-http-end-of-headers)
    (goto-char (point-min))
    (re-search-forward "^\r?\n")
    ;; FIXME Handle HTTP error
    (prog1 (funcall parser)
      (kill-buffer (current-buffer)))))

(defun helm-suggest--url-encode-params (params)
  (mapconcat
   (pcase-lambda (`(,key . ,val))
     (concat (url-hexify-string key)
             "="
             (url-hexify-string val)))
   params "&"))

;;; * [百度汉语 - 更懂汉语，更懂你](https://dict.baidu.com/)
;; 
;; API: https://dict.baidu.com/hanyu/ajax/sugs?mainkey=%E7%A0%B4%E5%B8%BD

(defun helm-suggest--baidu-dict-fetch (query)
  (let ((url (format "https://dict.baidu.com/hanyu/ajax/sugs?mainkey=%s"
                     (url-hexify-string query))))
    (let-alist (helm-suggest--url-retrieve-sync url #'helm-suggest--json-read)
      .data.ret_array)))

(defun helm-suggest--baidu-dict-candidates ()
  (mapcar
   (lambda (x)
     (let ((type (car (alist-get 'type x))))
       (pcase type
         ((guard (member type '("query" "term" "idiom")))
          (let ((sid (car (alist-get 'sid x)))
                (name (car (alist-get 'name x))))
            (cons name sid)))
         ("poemline"
          (let ((title (car (alist-get 'source_poem x)))
                (body (car (alist-get 'source_poem_body x)))
                (author (car (alist-get 'literature_author x)))
                (sid (car (alist-get 'source_poem_sid x))))
            (cons
             (format "%s (%s)\n%s"
                     title (propertize author 'face font-lock-comment-face)
                     (propertize body 'face font-lock-comment-face))
             sid)))
         ("poem"
          (let ((title (car (alist-get 'display_name x)))
                (body (car (alist-get 'body x)))
                (author (car (alist-get 'literature_author x)))
                (sid (car (alist-get 'sid x))))
            (cons
             (format "%s (%s)\n%s"
                     title (propertize author 'face font-lock-comment-face)
                     (propertize body 'face font-lock-comment-face))
             sid)))
         (_ (warn "WARNING: Unsupport data type: %S" x)
            nil))))
   (helm-suggest--baidu-dict-fetch helm-pattern)))

(defvar helm-suggest--baidu-dict-actions
  (let ((URL "https://dict.baidu.com/shici/detail?pid=%s"))
    (helm-make-actions
     "Browse URL"
     (lambda (sid) (browse-url (format URL sid)))
     "EWW URL"
     (lambda (sid) (eww (format URL sid))))))

(defvar helm-suggest--baidu-dict-source
  (helm-build-sync-source "百度汉语"
    :header-name (lambda (name) (format "%s <%s>" name "https://dict.baidu.com/"))
    :candidates #'helm-suggest--baidu-dict-candidates
    :action helm-suggest--baidu-dict-actions
    :volatile t
    :multiline t
    :requires-pattern 1))

;;;###autoload
(defun helm-suggest-baidu-dict ()
  "Search with suggestion with 百度汉语."
  (interactive)
  (helm-set-local-variable 'helm-input-idle-delay helm-suggest-input-idle-delay)
  (helm :sources helm-suggest--baidu-dict-source
        :full-frame t
        :buffer "*helm 百度汉语*"))

;;; * [百度翻译](https://fanyi.baidu.com/)
;;
;; API: curl https://fanyi.baidu.com/sug -d kw=grea

(defun helm-suggest--baidu-fanyi-fetch (query)
  (let ((json (let ((url-request-method "POST")
                    (url-request-extra-headers
                     '(("Content-Type" . "application/x-www-form-urlencoded")))
                    (url-request-data
                     (encode-coding-string
                      (helm-suggest--url-encode-params `(("kw" . ,query)))
                      'utf-8)))
                (helm-suggest--url-retrieve-sync
                 "https://fanyi.baidu.com/sug"
                 #'helm-suggest--json-read))))
    (mapcar
     (lambda (x) (let-alist x (cons .k .v)))
     (alist-get 'data json))))

(defun helm-suggest--baidu-fanyi-candidates ()
  (mapcar
   (pcase-lambda (`(,word . ,meaning))
     (format "%-20s %s" word
             ;; 有时开头会有空格
             (string-trim meaning)))
   (helm-suggest--baidu-fanyi-fetch helm-pattern)))

(defvar helm-suggest--baidu-fanyi-actions
  (helm-make-actions
   "Browse URL"
   (lambda (candidate)
     (let* ((query (car (split-string candidate "  " t)))
            ;; NOTE 只考虑中文 ⇔ 英文
            ;; https://fanyi.baidu.com/#en/zh/aggressive
            ;; https://fanyi.baidu.com/#zh/en/%E4%B8%AD%E5%9B%BD
            (from (if (string-match-p "\\cC" query) 'zh 'en))
            (to (pcase from
                  ('zh 'en)
                  ('en 'zh))))
       (browse-url
        (format "https://fanyi.baidu.com/#%s/%s/%s"
                from to (url-hexify-string query)))))
   "Insert Query"
   (lambda (candidate)
     ;; NOTE 单词和解释之间至少间隔 2 个空格
     (insert (car (split-string candidate "  " t))))))

(defvar helm-suggest--baidu-fanyi-source
  (helm-build-sync-source "百度翻译"
    :header-name
    (lambda (name)
      (format "%s <%s>" name "https://fanyi.baidu.com/"))
    :candidates #'helm-suggest--baidu-fanyi-candidates
    :action helm-suggest--baidu-fanyi-actions
    :volatile t
    :requires-pattern 1))

;;;###autoload
(defun helm-suggest-baidu-fanyi ()
  "Search with suggestion with 百度翻译."
  ;; IDEA Fill initial input from the region?
  (interactive)
  (helm-set-local-variable 'helm-input-idle-delay helm-suggest-input-idle-delay)  
  (helm :sources helm-suggest--baidu-fanyi-source
        :buffer "*helm 百度翻译*"))

;;; * [Longman Dictionary of Contemporary English | LDOCE](https://www.ldoceonline.com/)
;;
;; API: https://www.ldoceonline.com/autocomplete/english/?q=prett&contentType=application%2Fjson%3B+charset%3Dutf-8

(defun helm-suggest--ldoceonline-fetch (query)
  (helm-suggest--url-retrieve-sync
   (concat "https://www.ldoceonline.com/autocomplete/english/?"
           (helm-suggest--url-encode-params
            `(("contentType" . "application/json;+charset=utf-8")
              ("q" . ,query))))
   #'helm-suggest--json-read))

(defun helm-suggest--ldoceonline-candidates ()
  (mapcar
   (lambda (alist) (alist-get 'searchtext alist))
   (alist-get
    'results
    (helm-suggest--ldoceonline-fetch helm-pattern))))

(defvar helm-suggest--ldoceonline-actions
  (helm-make-actions
   "Browse URL"
   (lambda (word)
     (browse-url
      (format "https://www.ldoceonline.com/dictionary/%s"
              (url-hexify-string word))))
   "Insert" #'insert))

(defvar helm-suggest--ldoceonline-source
  (helm-build-sync-source "Ldoceonline"
    :header-name
    (lambda (name)
      (format "%s <%s>" name "https://www.ldoceonline.com/"))
    :candidates #'helm-suggest--ldoceonline-candidates
    :action helm-suggest--ldoceonline-actions
    :volatile t
    :requires-pattern 2))

;;;###autoload
(defun helm-suggest-ldoceonline ()
  "Search with suggestion with ldoceonline."
  (interactive)
  (helm-set-local-variable 'helm-input-idle-delay helm-suggest-input-idle-delay)  
  (helm :sources helm-suggest--ldoceonline-source
        :buffer "*helm ldoceonline*"))

;;; * [豆瓣读书](https://book.douban.com/)
;;
;; API: https://book.douban.com/j/subject_suggest?q=%E5%9B%BD%E5%8F%B2

(defun helm-suggest--douban-book-fetch (query)
  (helm-suggest--url-retrieve-sync
   (format "https://book.douban.com/j/subject_suggest?q=%s"
           (url-hexify-string query))
   #'helm-suggest--json-read))

(defun helm-suggest--douban-book-candidates ()
  (mapcar
   (lambda (alist)
     (let-alist alist
       (cons
        (format "%s %s\n%s"
                .title (or .year "")
                (or .author_name .en_name))
        alist)))
   (helm-suggest--douban-book-fetch helm-pattern)))

(defvar helm-suggest--douban-book-actions
  (helm-make-actions
   "Browse URL"
   (lambda (alist) (browse-url (let-alist alist .url)))
   "Insert"
   (lambda (alist) (insert (let-alist alist .title)))))

(defvar helm-suggest--douban-book-source
  (helm-build-sync-source "豆瓣读书"
    :header-name
    (lambda (name)
      (format "%s <%s>" name "https://book.douban.com/"))
    :candidates #'helm-suggest--douban-book-candidates
    :multiline t
    :action helm-suggest--douban-book-actions
    :volatile t
    :requires-pattern 1))

;;;###autoload
(defun helm-suggest-douban-book ()
  "Search with suggestion with 豆瓣读书."
  (interactive)
  (helm-set-local-variable 'helm-input-idle-delay helm-suggest-input-idle-delay)  
  (helm :sources helm-suggest--douban-book-source
        :full-frame t
        :buffer "*helm 豆瓣读书*"))

;; * [Urban Dictionary, June 10: Bend and Snap](https://www.urbandictionary.com/)
;;
;; API: https://api.urbandictionary.com/v0/autocomplete-extra?key=ab71d33b15d36506acf1e379b0ed07ee&term=accep

(defun helm-suggest--urban-fetch (query)
  (helm-suggest--url-retrieve-sync
   (format
    "https://api.urbandictionary.com/v0/autocomplete-extra?key=ab71d33b15d36506acf1e379b0ed07ee&term=%s"
    (url-hexify-string query))
   #'helm-suggest--json-read))

(defun helm-suggest--urban-candidates ()
  (mapcar
   (lambda (alist)
     (let-alist alist
       (format "%-20s  %s" .term .preview)))
   (alist-get 'results (helm-suggest--urban-fetch helm-pattern))))

(defvar helm-suggest--urban-actions
  (helm-make-actions
   "Browse URL"
   (lambda (candidate)
     (browse-url
      (format "https://www.urbandictionary.com/define.php?term=%s"
              (url-hexify-string (car (split-string candidate "  "))))))
   "Insert Term"
   (lambda (candidate)
     (insert (car (split-string candidate "  "))))))

(defvar helm-suggest--urban-source
  (helm-build-sync-source "Urban Dictionary"
    :header-name
    (lambda (name)
      (format "%s <%s>" name "https://www.urbandictionary.com/"))
    :candidates #'helm-suggest--urban-candidates
    :action helm-suggest--urban-actions
    :volatile t
    :nohighlight t
    :requires-pattern 1))

;;;###autoload
(defun helm-suggest-urban ()
  "Search with suggestion with Urban Dictionary."
  (interactive)
  (helm-set-local-variable 'helm-input-idle-delay helm-suggest-input-idle-delay)  
  (helm :sources helm-suggest--urban-source
        :full-frame t
        :buffer "*helm Urban Dictionary*"))

;; * [commandlinefu](https://www.commandlinefu.com/)
;;
;; API: curl 'https://www.commandlinefu.com/search/autocomplete' -d q=grep

(defun helm-suggest--commandlinefu-trim (s)
  (string-trim
   (replace-regexp-in-string
    " +" " "
    (replace-regexp-in-string
     "\n" " "
     s))))

(defun helm-suggest--commandlinefu-fetch (query)
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (encode-coding-string
          (helm-suggest--url-encode-params `(("q" . ,query)))
          'utf-8)))
    (helm-suggest--url-retrieve-sync
     "https://www.commandlinefu.com/search/autocomplete"
     (lambda () (libxml-parse-html-region (point) (point-max))))))

;; <ul>
;;   <li>
;;     <div class="autocomplete-command"><strong>grep</strong> -RnisI &lt;pattern&gt; *</div>
;;     <div class="autocomplete-description">
;;       Search for a <pattern> string inside all files in the current directory (49 votes, 3 comments)
;;     </div>
;;     <a style="display:none" class="destination" href="/commands/view/3573/search-for-a-string-inside-all-files-in-the-current-directory"></a>
;;   </li>
;; </ul>
(defun helm-suggest--commandlinefu-candidates ()
  (delq
   nil
   (mapcar
    (lambda (li)
      (let ((command (helm-suggest--commandlinefu-trim
                      (dom-texts (dom-by-class li "autocomplete-command") "")))
            (description (helm-suggest--commandlinefu-trim
                          (dom-texts (dom-by-class li "autocomplete-description"))))
            (link (concat "https://www.commandlinefu.com/" (dom-attr (dom-by-class li "destination") 'href))))
        ;; <li style="display:none"></li><li>   
        (unless (string-empty-p command)
          (cons (format "%s\n%s" command description)
                `((command . ,command)
                  (description . ,description)
                  (link . ,link))))))
    (dom-by-tag (helm-suggest--commandlinefu-fetch helm-pattern) 'li))))

(defvar helm-suggest--commandlinefu-actions
  (helm-make-actions
   "Browse URL"
   (lambda (alist)
     (browse-url (let-alist alist .link)))
   "Insert Command"
   (lambda (alist)
     (insert (let-alist alist .command)))))

(defvar helm-suggest--commandlinefu-source
  (helm-build-sync-source "Commandlinefu"
    :header-name
    (lambda (name)
      (format "%s <%s>" name "https://www.commandlinefu.com/"))
    :candidates #'helm-suggest--commandlinefu-candidates
    :multiline t
    :nohighlight t
    :action helm-suggest--commandlinefu-actions
    :volatile t
    :requires-pattern 1))

;;;###autoload
(defun helm-suggest-commandlinefu ()
  "Search with suggestion with commandlinefu."
  (interactive)
  (helm-set-local-variable 'helm-input-idle-delay helm-suggest-input-idle-delay)  
  (helm :sources helm-suggest--commandlinefu-source
        :full-frame t
        :buffer "*helm commandlinefu*"))

(provide 'helm-suggest)
;;; helm-suggest.el ends here
