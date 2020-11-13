;;; txl.el --- Provides machine translation via DeepL's REST API

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; Version: 0.0.1
;; Package-Requires: ((request "0.3.2") (guess-language "0.0.1") (emacs "28"))
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
;; for DeepL's REST API via `txl-deepl-api-url'.  Then the function
;; `txl-translate-to-other-language' can be used to translate the
;; current paragraph or the marked region to the respective other
;; language.  (The current language is detected using
;; `guess-language'.)  The function `txl-translate-roundtrip'
;; translate to the other language and back, which can yield
;; alternative, sometimes more idiomatic formulations.  Both of these
;; functions display the translation in the minibuffer.
;; Alternatively the functions `txl-replace-with-other-language' and
;; `txl-replace-with-roundtrip' can be used to replace the paragraph
;; or marked region with the translation.  Future versions may support
;; more than two languages and other translation services.

;;; Code:

(require 'request)
(require 'guess-language)

(defconst txl-translation-buffer-name "*TXL translation result*"
  "Name of the buffer used for reviewing and editing proposed
  translations.")

(defvar txl-source-buffer nil
  "Buffer for which a translation was requested.")

(defvar txl-deepl-api-url "https://api.deepl.com/v2/translate"
  "URL of the translation API.")

(defgroup txl nil
  "Use online machine translation services."
  :group 'text)

(defcustom txl-languages '("DE" . "EN-US")
  "The two languages between which DeepL will translate."
  :type '(cons
          (choice
           (const :tag "German" "DE")
           (const :tag "British English" "EN-GB")
           (const :tag "American English" "EN-US")
           (const :tag "French" "FR")
           (const :tag "Italian" "IT")
           (const :tag "Japanese" "JA")
           (const :tag "Spanish" "ES")
           (const :tag "Dutch" "NL")
           (const :tag "Polish" "PL")
           (const :tag "Portuguese, all Portuguese varieties excluding Brazilian Portuguese" "PT-PT")
           (const :tag "Brazilian Portuguese" "PT-BR")
           (const :tag "Russian" "RU")
           (const :tag "Chinese" "ZH"))
          (choice
           (const :tag "German" "DE")
           (const :tag "British English" "EN-GB")
           (const :tag "American English" "EN-US")
           (const :tag "French" "FR")
           (const :tag "Italian" "IT")
           (const :tag "Japanese" "JA")
           (const :tag "Spanish" "ES")
           (const :tag "Dutch" "NL")
           (const :tag "Polish" "PL")
           (const :tag "Portuguese, all Portuguese varieties excluding Brazilian Portuguese" "PT-PT")
           (const :tag "Brazilian Portuguese" "PT-BR")
           (const :tag "Russian" "RU")
           (const :tag "Chinese" "ZH"))))

(defcustom txl-deepl-split-sentences "nonewlines"
  "Whether the translation engine splits input into sentences which are translated individually."
  :type '(choice (const :tag "No splitting" "0")
                 (const :tag "Split on interpunction and on newlines" "1")
                 (const :tag "Split on interpunction only, ignoring newlines " "nonewlines")))

(defcustom txl-deepl-preserve-formatting "1"
  "Whether the translation engine should respect the original formatting.

The formatting aspects affected by this setting include:
Punctuation at the beginning and end of the sentence.
Upper/lower case at the beginning of the sentence."
  :type '(choice (const :tag "No" "0")
                 (const :tag "Yes" "1")))

(defcustom txl-deepl-formality "default"
  "Whether the translated text should lean towards formal or informal language.

This feature currently works for all target languages except
EN (English), EN-GB (British English), EN-US (American English),
ES (Spanish), JA (Japanese) and ZH (Chinese)."
  :type '(choice (const :tag "Default" "default")
                 (const :tag "More formal language" "more")
                 (const :tag "Less formal language" "less")))

(defcustom txl-deepl-api-key ""
  "The authentication key used to access the translation API."
  :type 'string)

(defun txl-translate-string (text target-lang &rest more-target-langs)
  "Translate TEXT to TARGET-LANG.

If MORE-TARGET-LANGS is non-nil, translation will be applied
recursively for all languages in MORE-TARGET-LANGS.  This allows,
for example, to translate to another language and back in one
go."
  (message "Retrieving translation from DeepL ... (target language %s)" target-lang)
  (let* ((request-backend 'url-retrieve)
         (response (request
                     txl-deepl-api-url
                     :type "POST"
                     :sync t
                     :parser 'json-read
                     :data `(("auth_key"            . ,txl-deepl-api-key)
                             ("split_sentences"     . ,txl-deepl-split-sentences)
                             ("preserve_formatting" . ,txl-deepl-preserve-formatting)
                             ("formality"           . ,txl-deepl-formality)
                             ("text"                . ,text)
                             ("target_lang"         . ,target-lang)))))
    (pcase (request-response-status-code response)
      (200
       (let* ((data (request-response-data response))
              (translations (cdr (assoc 'translations data)))
              (translation (cdr (assoc 'text (aref translations 0))))
              (translation (decode-coding-string (encode-coding-string translation 'latin-1) 'utf-8)))
         (if more-target-langs
             (apply 'txl-translate-string translation (car more-target-langs) (cdr more-target-langs))
           translation)))
      (400 (error "Bad request.  Please check error message and your parameters"))
      (403 (error "Authorization failed.  Please supply a valid auth_key parameter"))
      (404 (error "The requested resource could not be found"))
      (413 (error "The request size exceeds the limit"))
      (429 (error "Too many requests.  Please wait and resend your request"))
      (456 (error "Quota exceeded.  The character limit has been reached"))
      (503 (error "Resource currently unavailable.  Try again later"))
      (_   (error "Internal error")))))

(defun txl-translate (target-lang &rest more-target-langs)
  "Translate the region or paragraph to TARGET-LANG and return translation as string.

If MORE-TARGET-LANGS is non-nil, translation will be applied
recursively for all languages in MORE-TARGET-LANGS.  This allows,
for example, to translate to another language and back in one
go."
  (let* ((beginning (if (region-active-p) (region-beginning) (save-excursion (guess-language-backward-paragraph) (point))))
         (end       (if (region-active-p) (region-end)       (save-excursion (guess-language-forward-paragraph) (point))))
         (text (buffer-substring-no-properties beginning end)))
    (apply 'txl-translate-string text target-lang more-target-langs)))

(defun txl-guess-language ()
  "Guess the language of the region or paragraph."
  (let* ((beginning (if (region-active-p) (region-beginning) (save-excursion (guess-language-backward-paragraph) (point))))
         (end       (if (region-active-p) (region-end)       (save-excursion (guess-language-forward-paragraph) (point))))
         (language  (upcase (symbol-name (guess-language-region beginning end)))))
    (if (string-prefix-p language (car txl-languages))
        (car txl-languages)
      (cdr txl-languages))))

(defun txl-other-language ()
  "Return the respective other language of the region or paragraph.

The other language is the one language specified in
`txl-languages' in which the region or paragraph is *not*
written, i.e. the target language of a translation."
  (if (string= (txl-guess-language) (car txl-languages))
      (cdr txl-languages)
    (car txl-languages)))

(defun txl-replace-with-string (string)
  "Replace region or paragraph with STRING."
  (let* ((beginning (if (region-active-p) (region-beginning) (save-excursion (guess-language-backward-paragraph) (point))))
         (end       (if (region-active-p) (region-end)       (save-excursion (guess-language-forward-paragraph) (point)))))
    (delete-region beginning end)
    (insert string)))

;;;###autoload
(defun txl-translate-region-or-paragraph (&optional prefix-arg)
  "Translate the region or paragraph and display result in a separate buffer.

By default the text is translated to the other language specified
in `txl-languages'.  If PREFIX-ARG is non-nil, the text is
translated to the other language and back.

The translation is displayed in a separate buffer.  There it can
be edited there and, if desired, the original text can be
replaced with the (edited) translation using C-c C-c.  The
translation can be dismissed via C-c C-k."
  (interactive "P")
  (setq txl-source-buffer (current-buffer))
  (let* ((route (if prefix-arg
                    (list (txl-other-language) (txl-guess-language))
                  (list (txl-other-language))))
         (translation (apply 'txl-translate route)))
    (with-current-buffer (get-buffer-create txl-translation-buffer-name)
      (unless (derived-mode-p 'text-mode)
        (text-mode))
      (erase-buffer)
      (insert translation)
      (txl-edit-translation-mode)))
  (display-buffer txl-translation-buffer-name))

(defun txl-accept-translation ()
  "Hide buffer for reviewing and editing, replace original text with translation."
  (interactive)
  (let ((translation (buffer-string)))
    (txl-dismiss-translation)
    (with-current-buffer txl-source-buffer
      (txl-replace-with-string translation))))

(defun txl-dismiss-translation ()
  "Hide buffer for reviewing and editing translation."
  (interactive)
  (setq-local header-line-format nil)
  (replace-buffer-in-windows))

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
