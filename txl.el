;;; txl.el --- Provides machine translation via DeepL's REST API -*- lexical-binding: t -*-

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; Description: Provides machine translation via DeepL's REST API
;; Keywords: wp
;; Version: 0.0.2
;; Package-Requires: ((request "0.3.2") (guess-language "0.0.1") (emacs "24.4"))
;; URL: https://github.com/tmalsburg/txl.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TXL provides machine translation through DeepL's REST API.
;; Minimally the user needs to specify a pair of languages in the
;; customization variable `txl-languages' and an authentication key
;; for DeepL's REST API via `txl-deepl-api-url'.

;; The command `txl-translate-region-or-paragraph' translates the
;; marked region or, if no region is active, the paragraph to the
;; respective other language.  The current language is detected using
;; the package guess-language.  The retrieved translation is shown in
;; a separate buffer where it can be reviewed and edited.  The
;; original text can be replaced with the (edited) translation via
;; <C-c C-c>.  The translation can be dismissed (without touching the
;; original text) using <C-c C-k>.  If a prefix argument is given
;; (<C-u>), the text will be translated round-trip to the other
;; language and back.

;;; Code:

(require 'request)
(require 'org)
(require 'guess-language)

(defconst txl-translation-buffer-name "*TXL translation result*"
  "Name of the buffer used for reviewing and editing proposed translations.")

(defvar txl-source-buffer nil
  "Buffer for which a translation was requested.")

(defvar txl-original-window-configuration nil
  "Window configuration when a translation was requested.

Will be restored when the buffer for reviewing the translation is closed.")

(defvar txl-highlight-overlay nil
  "Overlay variable for highlighting the source region for translation.")

(defvar txl-deepl-formality-options
  '(("Default" . default)
    ("More formal language if available" . prefer_more)
    ("Less formal language if available" . prefer_less))
  "Possible values for the DeepL 'formality' parameter.

DeepL also supports the options 'more' and 'less', but they are not
supported for all target languages, and we don't know which of the
languages in `txl-languages' will be the target language.")

(defvar txl-deepl-model-options
  '(("Latency" . nil)
    ("Quality" . quality_optimized)
    ("Prefer latency" . prefer_latency_optimized)
    ("Prefer quality" . prefer_quality_optimized))
  "Possible values for the DeepL `model_type' parameter.")

(defvar-local txl-glossary ""
  "Name of a DeepL glossary to be used for translations.

When translating, we try to find a glossary with this name for
the requested language pair.  If no matching glossary can be
found, no glossary will be used.")

(defgroup txl nil
  "Use online machine translation services."
  :group 'text)

(defcustom txl-deepl-api-url "https://api.deepl.com/v2/translate"
  "URL of the translation API.  Depends on which plan is used."
  :type '(choice (const :tag "DeepL API Pro" "https://api.deepl.com/v2/translate")
                 (const :tag "DeepL API Free" "https://api-free.deepl.com/v2/translate")))

(defcustom txl-languages '(DE . EN-US)
  "The two languages between which DeepL will translate."
  :local t
  :type '(cons
          (choice
           (const :tag "German" DE)
           (const :tag "British English" EN-GB)
           (const :tag "American English" EN-US)
           (const :tag "French" FR)
           (const :tag "Italian" IT)
           (const :tag "Japanese" JA)
           (const :tag "Spanish" ES)
           (const :tag "Dutch" NL)
           (const :tag "Polish" PL)
           (const :tag "Portuguese, all Portuguese varieties excluding Brazilian Portuguese" PT-PT)
           (const :tag "Brazilian Portuguese" PT-BR)
           (const :tag "Russian" RU)
           (const :tag "Chinese" ZH))
          (choice
           (const :tag "German" DE)
           (const :tag "British English" EN-GB)
           (const :tag "American English" EN-US)
           (const :tag "French" FR)
           (const :tag "Italian" IT)
           (const :tag "Japanese" JA)
           (const :tag "Spanish" ES)
           (const :tag "Dutch" NL)
           (const :tag "Polish" PL)
           (const :tag "Portuguese, all Portuguese varieties excluding Brazilian Portuguese" PT-PT)
           (const :tag "Brazilian Portuguese" PT-BR)
           (const :tag "Russian" RU)
           (const :tag "Chinese" ZH))))

(defcustom txl-deepl-split-sentences 'nonewlines
  "Whether to input into sentences which are translated individually."
  :type '(choice (const :tag "No splitting" nil)
                 (const :tag "Split on interpunction and on newlines" t)
                 (const :tag "Split on interpunction only, ignoring newlines " nonewlines)))

(defcustom txl-deepl-preserve-formatting t
  "Whether the translation engine should respect the original formatting.

The formatting aspects affected by this setting include:
Punctuation at the beginning and end of the sentence.
Upper/lower case at the beginning of the sentence."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)))

(defcustom txl-deepl-formality 'default
  "Whether the translated text should lean towards formal or informal language.

This feature currently works for all target languages except
EN (English), EN-GB (British English), EN-US (American English),
ES (Spanish), JA (Japanese) and ZH (Chinese)."
  :local t
  :type '(choice (const :tag "Default" default)
                 (const :tag "More formal language" more)
                 (const :tag "Less formal language" less)))

(defcustom txl-deepl-model nil
  "Specifies which DeepL model should be used for translation."
  :local t
  :type '(choice (const :tag "latency_optimized" nil)
                 (const :tag "quality_optimized" quality_optimized)
                 (const :tag "prefer_latency_optimized" prefer_latency_optimized)
                 (const :tag "prefer_quality_optimized" prefer_quality_optimized)))

(defcustom txl-deepl-api-key ""
  "The authentication key used to access the translation API."
  :type 'string)

(defface txl-highlight-face
  '((default . (:inherit highlight)))
  "Face for highlighting the translation source text.")

;;; Error handling
(defun txl-handle-request-error (status-code response)
  ;; (error "Request failed with status code %s" (request-response-status-code response))
  ;; (message "xxx %d %S" status-code (cdr (assoc 'message (request-response-data response))))
  (let ((msg (cdr (assoc 'message (request-response-data response)))))
    (pcase status-code
      (400 (error "Bad request.  Please check error message and your parameters: %s" msg))
      (403 (error "Authorization failed.  Please supply a valid Authorization header: %s" msg))
      (404 (error "The requested resource could not be found: %s" msg))
      (413 (error "The request size exceeds the limit: %s" msg))
      (429 (error "Too many requests.  Please wait and resend your request: %s" msg))
      (456 (error "Quota exceeded.  The character limit has been reached: %s" msg))
      (503 (error "Resource currently unavailable.  Try again later: %s" msg))
      (_   (error "Internal error: %s" msg)))
    ))

(defun txl-get-usage ()
  "Query the DeepL server for usage data.

This function is currently not used by any other function."
  (let* ((request-backend 'url-retrieve)
	 (response (request "https://api.deepl.com/v2/usage"
                     :type "POST"
	             :sync t
	             :data `(("type" . "target"))
                     :headers `(("Authorization" . ,(concat "DeepL-Auth-Key " txl-deepl-api-key)))
                     :complete (cl-function
                                (lambda (&key response &allow-other-keys)
                                  (unless (eq 200 (request-response-status-code response))
                                    (txl-handle-request-error (request-response-status-code response) response))))
	             :parser 'json-read)))
    (request-response-data response)))

;; [TODO] Option to get target or source languages, see
;; <https://www.deepl.com/docs-api/general/get-languages/>

(defun txl-get-supported-languages-internal ()
  "Query DeepL server for known target languages."
  (let* ((request-backend 'url-retrieve)
         (response (request
                     "https://api.deepl.com/v2/languages"
                     :type "POST"
                     :sync t
                     :data `(("type" . "target"))
                     :headers `(("Authorization" . ,(concat "DeepL-Auth-Key " txl-deepl-api-key)))
                     :complete (cl-function
                                (lambda (&key response &allow-other-keys)
                                  (unless (eq 200 (request-response-status-code response))
                                    (txl-handle-request-error (request-response-status-code response) response))))
                     :parser 'json-read)))
    (request-response-data response)))

(defun txl-get-supported-languages ()
  "Return an alist containing the names and codes of supported languages.

This is a wrapper for `txl-get-supported-languages-internal' that returns
a subset of the information in a form suitable for `completing-read-multiple'."
  (map 'list (lambda (item)
               (list (alist-get 'name item)
                     (make-symbol (alist-get 'language item))))
       (txl-get-supported-languages-internal)))

(defun txl-set-languages (lang1 lang2)
  "Set the two languages between which DeepL will translate (`txl-languages').

Note that the order doesn't imply any translation direction.
`txl-translate-string' automatically sets the target language on
the basis of the detected language of the source text, and DeepL
automatically identifies the source language, if it isn't
specified in the request."
  (interactive
   (let ((completion-ignore-case t))
     (completing-read-multiple "Languages: " (txl-get-supported-languages))))

  (let ((codes
         (mapcar
          (lambda (item)
            (cadr (assoc item (txl-get-supported-languages))))
          (list lang1 lang2))))
    (setq txl-languages (cons (car codes) (cadr codes)))))

(defun txl-set-deepl-formality (formality)
  "Set whether the translated text should lean towards formal or informal language.

This function sets the value of `txl-deepl-formality'."
  (interactive
   (let ((completion-ignore-case t))
     (list
      (cdr (assoc
             (completing-read "Formality: " txl-deepl-formality-options nil t nil nil
                              (list (car (rassq (default-value 'txl-deepl-formality)
                                                txl-deepl-formality-options))))
             txl-deepl-formality-options)))))
  (setq txl-deepl-formality formality))

(defun txl-set-deepl-model (model)
  "Specify which DeepL model should be used for translation.

This function sets the value of `txl-deepl-model'."
  (interactive
   (let ((completion-ignore-case t))
     (list
      (cdr (assoc
            (completing-read "Model type: " txl-deepl-model-options nil t nil nil
                             (list (car (rassq (default-value 'txl-deepl-model)
                                               txl-deepl-model-options))))
            txl-deepl-model-options)))))
  (setq-local txl-deepl-model model))

(defun txl-set-glossary (name)
  "Set a glossary to be used for translations.

This function queries the DeepL server, and only names known to
the server can be selected.  However, whether a glossary of this
name is available for a specific language pair is only checked at
translation time.

This function sets the value of `txl-glossary'."
  (interactive
   (let ((completion-ignore-case t))
     (list
     (completing-read "Glossary: " (txl-glossary-get-unique-names)))))
  (setq txl-glossary name))

(defun txl-glossary-get-unique-names ()
  "Return a list of all glossary names known to DeepL."
  (let* ((request-backend 'url-retrieve)
         (response (request
                     "https://api.deepl.com/v2/glossaries" ;txl-deepl-api-url
                     :type "GET"
                     :sync t
                     :parser 'json-read
                     :headers `(("Authorization" . ,(concat "DeepL-Auth-Key " txl-deepl-api-key)))
                     :complete (cl-function
                                (lambda (&key response &allow-other-keys)
                                  (unless (eq 200 (request-response-status-code response))
                                    (txl-handle-request-error (request-response-status-code response) response))))
                     ))
         (data (request-response-data response)))

    (seq-uniq (seq-map (lambda (elt) (cdr (assoc 'name elt)))
             (cdr (assoc 'glossaries data))))))

(defun txl-glossary-get-id-by-name (name source-lang target-lang)
  "Return the ID of a glossary matching NAME, SOURCE-LANG, and TARGET-LANG.

Otherwise return nil.

DeepL glossaries always have a direction."
  (let* ((request-backend 'url-retrieve)
         (response (request
                     "https://api.deepl.com/v2/glossaries" ;txl-deepl-api-url
                     :type "GET"
                     :sync t
                     :parser 'json-read
                     :headers `(("Authorization" . ,(concat "DeepL-Auth-Key " txl-deepl-api-key)))
                     :complete (cl-function
                                (lambda (&key response &allow-other-keys)
                                  (unless (eq 200 (request-response-status-code response))
                                    (txl-handle-request-error (request-response-status-code response) response))))
                     ))
         (data (request-response-data response))
         (source-lang (downcase (substring (symbol-name source-lang) 0 2)))
         (target-lang (downcase (substring (symbol-name target-lang) 0 2)))
         (id   (cdr (assoc 'glossary_id
                           (seq-find (lambda (elt)
                                       (and
                                        (string-equal name (cdr (assoc 'name elt)))
                                        (string-equal source-lang (cdr (assoc 'source_lang elt)))
                                        (string-equal target-lang (cdr (assoc 'target_lang elt)))
                                        ))
                                     (cdr (assoc 'glossaries data)))))))
    (message "Found glossary `%s' %s → %s: %s" name source-lang target-lang id) ; [DEBUG]
    id))

(defun txl-translate-string (text target-lang &rest more-target-langs)
  "Translate TEXT to TARGET-LANG.

If `txl-glossary' is set, this function will attempt to use a glossary.

If MORE-TARGET-LANGS is non-nil, translation will be applied
recursively for all languages in MORE-TARGET-LANGS.  This allows,
for example, to translate to another language and back in one
go."
  (message "Requesting translation from %s to %s... %s" ; [DEBUG]
           (txl-guess-string-language text) target-lang (symbolp target-lang))
  (let* (
         ;; source_lang MUST be specified when using a glossary, and
         ;; the language ID must be without variant.
         (source-lang (txl-guess-string-language text))
         (glossary-id (unless (string-empty-p txl-glossary)
                        (txl-glossary-get-id-by-name txl-glossary source-lang target-lang)))
         ;; (request-backend 'url-retrieve) ; The re-encoding below is only necessary for `url-retrieve'.
         (response (request
                   txl-deepl-api-url
                   :type "POST"
                   :sync t
                   :parser 'json-read
                   :encoding 'utf-8
                   :data `(("split_sentences"     . ,(pcase txl-deepl-split-sentences
                                                       ((pred not) "0")
                                                       ('nonewlines "nonewlines")
                                                       ((pred (lambda (x) (eq t x))) "1")))
                           ("preserve_formatting" . ,(if txl-deepl-preserve-formatting "1" "0"))
                           ("formality"           . ,(symbol-name txl-deepl-formality))
                           ("text"                . ,text)
                           ("target_lang"         . ,target-lang)
                           ,(if glossary-id
                                `("glossary_id"   . ,glossary-id))
                           ("source_lang"         . ,source-lang) ; optional unless we're using a glossary
                           ,(if txl-deepl-model
                                `("model_type"    . ,txl-deepl-model))
                           )
                   :headers `(("Authorization" . ,(concat "DeepL-Auth-Key " txl-deepl-api-key)))
                   :complete (cl-function
                              (lambda (&key response &allow-other-keys)
                                (unless (eq 200 (request-response-status-code response))
                                  (txl-handle-request-error (request-response-status-code response) response))))
                   ))
         (data (request-response-data response))
         )
    (let* ((translations (cdr (assoc 'translations data)))
           (translation (cdr (assoc 'text (aref translations 0))))
           ;; (translation (decode-coding-string (encode-coding-string translation 'latin-1) 'utf-8))
           )
      (if more-target-langs
          (apply #'txl-translate-string
                 translation (car more-target-langs) (cdr more-target-langs))
        translation))))

(defun txl-beginning ()
  "Return beginning of region or, if inactive, paragraph."
  (if (region-active-p)
      (region-beginning)
    (save-excursion
      (if (derived-mode-p 'org-mode)
          ;; When in list, go to the beginning of the top-level list:
          (if (org-in-item-p)
              (org-beginning-of-item-list)
            (org-backward-paragraph))
        (end-of-paragraph-text)
        (start-of-paragraph-text))
      (point))))

(defun txl-end ()
  "Return end of region or, if inactive, paragraph."
  (if (region-active-p)
      (region-end)
    (save-excursion
      (if (derived-mode-p 'org-mode)
          (if (org-in-item-p)
              (org-end-of-item-list)
            (org-forward-paragraph))
        (end-of-paragraph-text))
      (min (point-max) (point)))))

(defun txl-translate (target-lang &rest more-target-langs)
  "Translate region or paragraph to TARGET-LANG and return translation.

If MORE-TARGET-LANGS is non-nil, translation will be applied
recursively for all languages in MORE-TARGET-LANGS.  This allows,
for example, to translate to another language and back in one
go."
  ;; [TODO] Delete overlay in case of error (e.g., no Internet)
  (setq txl-highlight-overlay (make-overlay (txl-beginning) (txl-end)))
  (overlay-put txl-highlight-overlay 'face 'txl-highlight-face)
  (let ((text (buffer-substring-no-properties (txl-beginning) (txl-end))))
    (apply #'txl-translate-string text target-lang more-target-langs)))

(defun txl-guess-language ()
  "Guess the language of the region or paragraph."
  (let* ((language (guess-language-region (txl-beginning) (txl-end)))
         (language (upcase (symbol-name language))))
    (if (string-prefix-p language (symbol-name (car txl-languages)))
        (car txl-languages)
      (cdr txl-languages))))

(defun txl-guess-string-language (string)
  "Guess the language of STRING.

This function is primarily intended for use with glossaries,
which requires specifying the source language.  This is also why
this functions returns the raw language code returned by
`guess-language-region', unlike `txl-guess-language', which may
return a variant code (e.g., EN-US)."
    (with-temp-buffer
      (insert string)
      (mark-whole-buffer)
      (guess-language-region (region-beginning) (region-end))))

(defun txl-other-language ()
  "Return the respective other language of the region or paragraph.

The other language is the one language specified in
`txl-languages' in which the region or paragraph is *not*
written, i.e. the target language of a translation."
  (if (eq (txl-guess-language) (car txl-languages))
      (cdr txl-languages)
    (car txl-languages)))

;;; DeepL Write

(defun txl-rephrase-string (text &optional target-lang style-or-tone)
  "Rephrase TEXT in TARGET-LANG.

[TODO] Work in progress, see URL `https://developers.deepl.com/docs/api-reference/improve-text'

Should allow setting writing style or tone (it’s not possible to
include both writing_style and tone in a request; only one or the
other can be included.)
"
  (message "Requesting rephrasing of text in %s... style-or-tone %s" target-lang style-or-tone)
  (let* ((request-backend 'url-retrieve)
         (response (request
                     "https://api.deepl.com/v2/write/rephrase" ;txl-deepl-api-url
                     :type "POST"
                     :sync t
                     :parser 'json-read
                     :data `(("text"             . ,text)
                             ("target_lang"      . ,(or target-lang ""))
                             ,(txl-style-or-tone style-or-tone)
                             )
                     :headers `(("Authorization" . ,(concat "DeepL-Auth-Key " txl-deepl-api-key)))
                     :complete (cl-function
                                (lambda (&key response &allow-other-keys)
                                  (unless (eq 200 (request-response-status-code response))
                                    (txl-handle-request-error (request-response-status-code response) response))))
                     ))
         (data (request-response-data response)))
    (decode-coding-string
     (encode-coding-string 
      (cdr (assoc 'text (aref (cdr (assoc 'improvements data)) 0)))
      'latin-1)
     'utf-8)))

(defun txl-style-or-tone (val)
  "Return a pair `(\"writing_style\" . VAL)' or `(\"tone\" . VAL)' depending on
whether VAL is a valid writing style or tone for the DeepL text
improvement function.

This function is primarily intended for use by
`txl-rephrase-string'.

Currently supported values for writing style are `simple',
`business', `academic', and `casual'.

Currently supported values for tone are `enthusiastic',
`friendly', `confident', and `diplomatic'.

Since they may not be supported for the current language, prefix
supported values with `prefer_'.

For all other values, return `(\"style\" . \"default\")'.

See URL `https://developers.deepl.com/docs/api-reference/improve-text'."
  (let ((val (or val ""))
        (styles (regexp-opt '("simple" "business" "academic" "casual") t))
        (tones (regexp-opt '("enthusiastic" "friendly" "confident" "diplomatic") t)))

    (cond ((string-match styles val) `("writing_style" . ,(concat "prefer_" (match-string 1 val))))
          ((string-match tones val) `("tone" . ,(concat "prefer_" (match-string 1 val))))
          (t '("style" . "default")))))

(defun txl-rephrase-region-or-paragraph (&optional prefix)
  "[TODO]

This is a hacked version of `txl-translate-region-or-paragraph'
for testing.  It currently directly calls `txl-rephrase-string',
without a function in between that does the highlighting.
There's also no way to specify the writing style or tone.
"
  (interactive "P")
  (setq txl-source-buffer (current-buffer))
  (setq txl-original-window-configuration (current-window-configuration))
  (let* ((translation (txl-rephrase-string (buffer-substring-no-properties (txl-beginning) (txl-end))))
         )
    (with-current-buffer (get-buffer-create txl-translation-buffer-name)
      (unless (derived-mode-p 'text-mode)
        (text-mode))
      (erase-buffer)
      (insert translation)
      (txl-edit-translation-mode)
      (goto-char (point-min))))
  (display-buffer txl-translation-buffer-name
                  '((display-buffer-below-selected display-buffer-at-bottom)
                    (inhibit-same-window . t)
                    (window-height . fit-window-to-buffer)))
  (select-window (get-buffer-window txl-translation-buffer-name)))

;;;###autoload
(defun txl-translate-region-or-paragraph (&optional roundtrip)
  "Translate the region or paragraph and display result in a separate buffer.

By default the text is translated to the other language specified
in `txl-languages'.  If ROUNDTRIP is non-nil, the text is
translated to the other language and back.

The translation is displayed in a separate buffer.  There it can
be edited there and, if desired, the original text can be
replaced with the (edited) translation using
\\<txl-edit-translation-mode-map> \\[txl-accept-translation].  The
translation can be dismissed via \\[txl-dismiss-translation]."
  (interactive "P")
  (setq txl-source-buffer (current-buffer))
  (setq txl-original-window-configuration (current-window-configuration))
  (let* ((route (if roundtrip
                    (list (txl-other-language) (txl-guess-language))
                  (list (txl-other-language))))
         (translation (apply #'txl-translate route)))
    (with-current-buffer (get-buffer-create txl-translation-buffer-name)
      (unless (derived-mode-p 'text-mode)
        (text-mode))
      (erase-buffer)
      (insert translation)
      (txl-edit-translation-mode)
      (goto-char (point-min))))
  (display-buffer txl-translation-buffer-name
                  '((display-buffer-below-selected display-buffer-at-bottom)
                    (inhibit-same-window . t)
                    (window-height . fit-window-to-buffer)))
  (select-window (get-buffer-window txl-translation-buffer-name)))

(defun txl-accept-translation ()
  "Hide buffer for reviewing and editing, replace original text with translation."
  (interactive)

  (with-current-buffer txl-source-buffer
    ;; It doesn't seem necessary to explicitly delete the overlay here
    (replace-region-contents
     (txl-beginning) (txl-end)
     (lambda () (get-buffer txl-translation-buffer-name))))
  (txl-dismiss-translation))

(defun txl-dismiss-translation ()
  "Hide buffer for reviewing and editing translation."
  (interactive)
  (when txl-highlight-overlay
      (delete-overlay txl-highlight-overlay))
  (setq-local header-line-format nil)
  (set-window-configuration txl-original-window-configuration))

(define-minor-mode txl-edit-translation-mode
  "Minor mode for reviewing and editing translations."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'txl-accept-translation)
            (define-key map (kbd "C-c C-k") 'txl-dismiss-translation)
            map)
  (setq-local
   header-line-format
   (substitute-command-keys
    " Accept translation \\[txl-accept-translation], dismiss translation \\[txl-dismiss-translation]")))

;; Define global minor mode.  This is needed to the toggle minor mode.
;;;###autoload
(define-globalized-minor-mode txl-edit-translation-global-mode
  txl-edit-translation-mode txl-edit-translation-mode)

(provide 'txl)

;;; txl.el ends here
