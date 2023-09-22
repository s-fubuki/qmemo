;;; qmemo.el --- Quick memo pad -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shiina fubuki

;; Author: Shiina fubuki <fubukiATfrill.org>
;; Keywords: data
;; Version: $Revision: 1.30 $

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

;; Quick memo pad.
;; - A new note is added to `qmemo-file-name', bottom-to-up.
;; - The number of lines in the window is `qmemo-window-height'.
;; - The format of the memo is set with `qmemo-templete-format'.

;; Although not recommended, it does the following:

;; (global-set-key "\M-sm" 'qmemo)

;;; Code:
(defconst qmemo-version "$Revision: 1.30 $")
(defgroup qmemo nil "qmemo group."
  :prefix "qmemo-"
  :prefix "qp-"
  :group 'data)

(defcustom qmemo-file-name
  (let ((dir  "~/.emacs.d/qmemo")
        (file "%Y-%02m"))
    (when (not (file-exists-p dir))
      (make-directory dir))
    (format-time-string (expand-file-name file dir)))
  "File name."
  :type  'file
  :group 'qmemo)

(defcustom qmemo-window-height 8
  "Window height.
Total number including mode-line and minibuffer."
  :type  'integer
  :group 'qmemo)

(defcustom qmemo-description-length 32
  "Max length of description."
  :type  'integer
  :group 'qmemo)

(defcustom qmemo-system-name (system-name)
  "Host name."
  :type  'string
  :group 'qmemo)

(defcustom qmemo-host-name (or mail-host-address (getenv "HOSTNAME") "unknown")
  "Host name."
  :type  'string
  :group 'qmemo)

(defcustom qmemo-user-name (user-login-name)
  "User name."
  :type  'string
  :group 'qmemo)

(defcustom qmemo-user-full-name (user-full-name)
  "User full name."
  :type  'string
  :group 'qmemo)

(defcustom qmemo-user-mail-address user-mail-address
  "Mail address."
  :type  'string
  :group 'qmemo)

(defcustom qmemo-initial-templete nil
  "付けてはみたが廃止の可能性大."
  :type  '(choice string (const nil))
  :group 'qmemo)

(defcustom qmemo-templete-dsc '("" qmemo-leader qmemo-dsc "\n")
  "メモのタイトル行."
  :type '(repeat (choice string symbol sexp))
  :group 'qmemo)

(defcustom qmemo-templete-prefix
  '(qmemo-post-prefix (:eval (format "SCHEDULED: <%s>\n" (qmemo-calendar-date-string))))
  "flush時 prefix 在りのとき追加されるテキスト."
  :type '(repeat (choice string symbol sexp))
  :group 'qmemo)
  
(defcustom qmemo-templete-propaties '(:eval (qmemo-properties-string))
  "追加されるプロパティ."
  :type '(repeat (choice string symbol sexp))
  :group 'qmemo)

(defcustom qmemo-templete-format
  '("" qmemo-templete-dsc qmemo-templete-prefix qmemo-templete-propaties qmemo-body "\n")
  "メモ ブロックのテンプレートの構成を決めるリストもしくは文字列.
各エレメントが 関数 `qmemo-conlist' によって連結される."
  :type '(choice string (repeat (choice string symbol sexp)))
  :group 'qmemo)

(defcustom qmemo-time-format nil
  "format-time-string for %T."
  :type  '(choice (const nil) string)
  :group 'qmemo)
  
(defcustom qmemo-quit-query nil
  "Quit query."
  :type  'boolean
  :group 'qmemo)

(defcustom qmemo-filter-function nil
  "qmemo filter function."
  :type  '(choice function (const nil))
  :group 'qmemo)

(defcustom qmemo-create-lockfiles create-lockfiles
  "ファイル書き込み時の `create-lockfiles' の値."
  :type  'boolean
  :group 'qmemo)

(defcustom qp-prompt "Pickup: "
  "Qmemo pickup prompt."
  :type  'string
  :group 'qmemo)

(defcustom qp-dir t
  "`qmemo-file-name' のあるディレクトリのファイルすべてをピックアップ対象とする.
但し `qp-ignored-extensions' 指定拡張子は対象外となる."
  :type  'boolean
  :group 'qmemo)

(defcustom qp-base-mode 'outline-mode
  "qpick mode のベースになるメジャーモード."
  :type  'function
  :group 'qmemo)

(defcustom qhead-default 3
  "qhead の前起き引数なしのデフォルト値."
  :type  'integer
  :group 'qmemo)

(defvar qmemo-leader          "* ")
(defvar qmemo-buffer-name     "*qmemo*")
(defvar qmemo-user-at-system  (format "%s@%s" qmemo-user-name qmemo-system-name))
(defvar qp-buffer             "*qpick*")
(defvar qp-ignored-extensions (cons "#" completion-ignored-extensions))

(defvar qmemo-dsc              "")
(defvar qmemo-body             "")
(defvar qmemo-buffer-file-name "")

(defvar qmemo-format-spec-alist
  '((?s . qmemo-system-name)            ; System name (Machine name)
    (?h . qmemo-host-name)              ; Host name
    (?u . qmemo-user-name)              ; User name
    (?n . qmemo-user-full-name)         ; User full name
    (?m . qmemo-user-mail-address)      ; Mail address
    (?l . qmemo-leader)                 ; Head leader string
    (?d . qmemo-dsc)                    ; Description (body 1st line) or Start point
    (?b . qmemo-body)                   ; Memo body or Start point
    (?U . qmemo-user-at-system)         ; user@system
    (?p . qmemo-properties-string)      ; Property block.
    (?f . qmemo-buffer-file-name)       ; Buffer or file name at Qmemo start
    (?T . qmemo-current-time-string)    ; Current time
    (?C . qmemo-calendar-date-string)   ; *Calendar* point date
    (?% . "%")))                        ; % itselt

(defcustom qmemo-properties-string-alist
  '((":CREATED: %s" . qmemo-current-time-string)
    (":BUFFER/FILE: %s" . qmemo-buffer-file-name)
    (":PERSON: %s" . qmemo-user-at-system))
  ""
  :type '(repeat (cons string (choice function variable)))
  :group 'qmemo)

(defun qmemo-current-time-string ()
  (format-time-string (or qmemo-time-format "[%Y-%m-%d %a %T %z]")))

(defun qmemo-properties-string ()
  (concat ":PROPERTIES:\n"
          (mapconcat #'(lambda (a) (concat (format (car a) (cdr a)) "\n"))
                     (qmemo-eval-mapcdr qmemo-properties-string-alist) "")
          ":END:\n"))

(defun qmemo-calendar-date-string ()
  (format-time-string "%Y-%m-%d %a" (or qmemo-calendar-date nil)))

(defun qmemo-make-template (obj)
  (qmemo-conlist (if (stringp obj) (list obj) obj)))

(defun qmemo-conlist (lst &optional depth)
  "LST のエレメンツを再帰的に concat する.
基本的に mode-line-format の展開と同じ(だと思う)だが簡易版になっている.
エレメントがリストのとき CAR がシンボル :eval なら CDR を評価し
その他のシンボルの場合、評価して NON-NIL なら nth 1 を、
NIL なら nth 2 を選択する. \(COND THEN ELSE).
ELSE は省略できる.
シンボルは他の位置ならシンボルとして評価されるが t と nil は無視される.
DEPTH が nil のとき string は関数 `qmemo-format' にかけられる.
それはつまりカスタム変数 `qmemo-templete-format' では、\
リテラル文字列にだけ %LETTER 展開が適用される."
  (let ((elt (car lst))
        (depth (or depth 0)))
    (cond
     ((null lst) nil)
     ((stringp elt)
      (concat (if (zerop depth) (qmemo-format elt) elt)
              (qmemo-conlist (cdr lst) (1+ depth))))
     ((memq elt '(t nil)) ; Through symbols.
      (qmemo-conlist (cdr lst) (1+ depth)))
     ((symbolp elt)
      (concat (qmemo-conlist (cons (eval elt) (cdr lst)) (1+ depth))))
     ((consp elt)
      (concat
       (cond
        ((eq (car elt) :eval)
         (qmemo-conlist (list (eval (cadr elt))) (1+ depth)))
        ((symbolp (car elt))
         (qmemo-conlist (list (nth (if (eval (car elt)) 1 2) elt)) (1+ depth)))
        (t
         (qmemo-conlist elt (1+ depth))))
       (qmemo-conlist (cdr lst) (1+ depth)))))))

(defun qmemo-prepend-region (beg end file)
  "BEG から END の範囲を文字列として FILE 先頭にプリペンド.
BEG が文字列なら BEG を使う."
  (let ((str (if (stringp beg) beg (buffer-substring-no-properties beg end)))
        (create-lockfiles qmemo-create-lockfiles))
    (with-temp-buffer
      (insert str)
      (insert-file-contents file)
      (write-region (point-min) (point-max) file))))

(defun qmemo-make-description (body len)
  "BODY 先頭から 長さ LEN だけ切り取った文字列を返す.
先頭の空白は読み飛ばされる."
  (let (str)
    (string-match "^[ \t\n]*\\(?1:.*?\\)[ \t]*$" body)
    (setq str (match-string 1 body))
    (if (< len (length str))
        (concat (substring str 0 len) "...")
      str)))

(defvar qmemo-org-headline-escape ",") ; "*" にすると1段深いヘッドラインになる.

(defun qmemo-org-headline-escape ()
  "org-mode, outline-mode の  Headline character を escaping.
`qmemo-flush-hook' 用関数."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(?1:[*]+ \\)" nil t)
      (replace-match (concat qmemo-org-headline-escape (match-string 1))))))

(defvar qmemo-flush-hook nil "file 書き出し直前に実行される.")
(defvar qmemo-pre-prefix nil)
(defvar qmemo-post-prefix nil)
(defvar qmemo-calendar-date nil)

(defcustom qmemo-calendar-link t
  "calendar の日付とのリンク."
  :type  'boolean
  :group 'qmemo)

(defun qmemo-flush (prefix)
  "カレントバッファをメモの体裁にして `qmemo-file-name' 先頭に継ぎ足し\
ウィンドウを閉じカレントバッファ(メモ)を削除する."
  (interactive "P")
  (let ((date (and qmemo-calendar-link
                   (equal qmemo-buffer-file-name "<*Calendar*>") 
                   (with-current-buffer "*Calendar*"
                     (calendar-cursor-to-date)))))
    (setq qmemo-calendar-date
          (and date (apply #'encode-time 0 0 0 (nth 1 date)
                           (car date) (cddr date)))
          qmemo-post-prefix (or date prefix))
    (run-hooks 'qmemo-flush-hook)
    (qmemo-prepend-file (buffer-substring-no-properties (point-min) (point-max))
                        qmemo-file-name)
    (unless (one-window-p 'no-mini) (delete-window))
    (kill-buffer qmemo-buffer-name)
    (and date (quit-window))))

(defun qmemo-cancel ()
  "メモウィンドウを閉じそのバッファ(メモ)を削除."
  (interactive)
  (and qmemo-quit-query
       (not (y-or-n-p "Cancel?")) (error "Session has been canceled."))
  (unless (one-window-p 'no-mini) (delete-window))
  (kill-buffer qmemo-buffer-name))

(defun qmemo-hold ()
  "メモウィンドウを閉じる.
メモバッファは残るので次に追記できる."
  (interactive)
  (if (one-window-p 'no-mini)
      (set-window-buffer (selected-window) (cadr (buffer-list)))
    (delete-window)))

(defun qmemo-eval-mapcdr (alst)
  (mapcar
   #'(lambda (a)
       (cons
        (car a)
        (if (functionp (cdr a))
            (funcall (cdr a))
          (eval (cdr a)))))
   alst))

(defun qmemo-format (fmt)
  (format-spec fmt (qmemo-eval-mapcdr qmemo-format-spec-alist)))

(defun qmemo-add-lf (str)
  (if (not (equal "\n" (substring str -1)))
      (concat str "\n")
    str))

(defun qmemo-prepend-file (body file)
  "BODY を `qmemo-format' に依りメモの体裁にし FILE 先頭に追加する.
FILE は無ければ新規作成する."
  (let* ((buff (get-file-buffer file))
         (create-lockfiles qmemo-create-lockfiles)
         qmemo)
    (setq qmemo-body (qmemo-add-lf body)
          qmemo-dsc  (qmemo-make-description body qmemo-description-length)
          qmemo      (qmemo-make-template qmemo-templete-format))
    (cond
     (buff
      (with-current-buffer buff
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (insert qmemo)
            (write-file file)
            (revert-buffer t t)))))
     ((file-exists-p file)
      (qmemo-prepend-region qmemo nil file))
     (t
      (write-region qmemo nil file nil t)))))

(defun qp-get-record (re-beg &optional re-end)
  "ポイント位置以降にあるレコードブロックとそのプロパティをコンセルにしてひとつ返す. 
なければ nil. プロパティは :point に開始と終了のポイントがコンスセルで入る.
レコード終端に改行がなければ追加する.
開始ポイントまで読み飛ばされるので、
基本的には開始時のポイントがバッファ先頭かレコード先頭を指していることを前提にしている.
RE-BEG はレコード開始行が定義された正規表現.
レコードの終端は次のレコードの前までだが \
RE-END があればそれが明示的なレコード終端の正規表現になる.
RE-BEG: \"^[^ \\t\\n]\", RE-END: \"\\n\\n\" \
等とすると空行で区切られたデータに対応することができる.
実行後ポイントはレコード終端の後、つまり次のレコード先頭まで進む."
  (let (beg end)
    (and
     (setq beg (and (re-search-forward re-beg nil t) (match-beginning 0)))
     (setq end
           (if re-end
               (progn (re-search-forward re-end nil 0) (point))
             (if (re-search-forward re-beg nil 0)
                 (goto-char (match-beginning 0))
               (point))))
     (cons (list :point (cons beg end))
           (concat (buffer-substring-no-properties beg end)
                   (if (not (eq (char-after (1- end)) ?\n)) "\n"))))))

(defun qp-pure-files (files)
  "FILES から `qp-ignored-extensions' にマッチする要素を除去して戻す."
  (let ((del (concat (regexp-opt (cons "." qp-ignored-extensions)) "\\'")))
    (delq nil (mapcar #'(lambda (f) (and (not (string-match del f)) f)) files))))

(defun qp-string-and-match (args str)
  "正規表現リスト ARGS すべてが STR に含まれていれば non-nil."
  (let ((loop t))
    (eval (cons
           'and
           (mapcar
            #'(lambda (re) (and loop (setq loop (string-match-p re str))))
            args)))))

(defun qmemo-head (times)
  (with-temp-buffer
    (let ((beg (concat "^" qmemo-leader))
          result)
      (insert-file-contents qmemo-file-name)
      (if (zerop times)
          (list
           (cons (list :point (cons (point-min) (point-max)))
                 (buffer-string)))
        (goto-char (point-min))
        (catch 'break
          (dotimes (i times)
            (let ((ret (qp-get-record beg)))
              (if ret
                  (setq result (cons ret result))
                (throw 'break i)))))
        (nreverse result)))))

(defun qp-created-date (str)
  (if (string-match ":CREATED: +\\[\\(?1:.+\\)\\]\n" str)
      (progn
        (parse-time-string (match-string 1 str))
        (encode-time (parse-time-string (match-string 1 str))))
    '(0 0)))

(defun qp-sort (lst)
  (sort lst
        #'(lambda (a b)
            (let ((a (qp-created-date (cdr a)))
                  (b (qp-created-date (cdr b))))
              (time-less-p a b)))))

(defun qp-exsist-xz-files (files)
  "FILES の要素の末尾に \".xz\" をつけたものが存在すればそれに置き換える."
  (mapcar
   (lambda (f)
     (if (file-exists-p (concat f ".xz"))
         (concat f ".xz")
       f))
   files))

(defun qp-display
    (regexp files &optional beg end buff mode filter and-match font-lock)
  "REGEXP を含むレコードブロックを FILES リストから抽出して BUFF に表示.
FILTER はマッチ結果であるレコードリストを引数にしてフィルタリングして戻す関数を指定する.
nil ならその処理はしない.
レコードブロックとは BEG から END までの領域で REGEXP, BEG, END はすべて正規表現.
BEG, END は nil ならそれぞれ `qmemo-leader'、 次の BEG 直前までとなる.
BUFF, MODE は nil なら `qp-buffer'、`qp-mode' となる.
AND-MATCH が non-nil なら REGEXP を空白で分解してその総てがマッチしたときだけマッチ成立となる.
FONT-LOCK が non-nil なら BEG をハイライトし、 face なら BEG を face でハイライトする."
  (let* ((buff (or buff qp-buffer))
         (mode (or mode 'qp-mode))
         (beg  (or beg  (concat "^" qmemo-leader)))
         (match-fun (if and-match #'qp-string-and-match #'string-match))
         (regexp (if (equal regexp "") nil (if and-match (split-string regexp) regexp)))
         result record)
    (unless regexp (error "Input error"))
    (dolist (f (qp-exsist-xz-files files))
      (with-temp-buffer
        (insert-file-contents f)
        (while (setq record (qp-get-record beg end))
          (when (funcall match-fun regexp (cdr record))
            (setq result (cons
                          (cons (append (list :file f) (car record)) (cdr record))
                          result))))))
    (cond
     ((null result)
      (message "No match"))
     (t
      (and (get-buffer buff) (kill-buffer buff))
      (message "%d record(s) matchd." (length result))
      (and filter (setq result (funcall filter result)))
      (with-current-buffer (get-buffer-create buff)
        (mapc #'(lambda (s) (insert (apply #'propertize (cdr s) (car s))))
              (reverse result))
        (funcall mode)
        (setq mode-name (concat mode-name ":"
                      (if (listp regexp) (mapconcat #'identity regexp ":") regexp)))
        (goto-char (point-min))
        (and font-lock
             (if (facep font-lock)
                 (font-lock-add-keywords nil `((,beg . ',font-lock)))
               (font-lock-add-keywords nil `(,beg))))
        (save-excursion
          (dolist (re (if (consp regexp) regexp (list regexp)))
            (while (re-search-forward re nil t)
              (overlay-put
               (make-overlay (match-beginning 0) (match-end 0))
               'face 'match))
            (goto-char (point-min))))
        (setq buffer-read-only t))
      (pop-to-buffer buff)))))

(defun qp-next-match ()
  "ポイントより後のマッチにポイントを移動."
  (interactive)
  (goto-char (next-overlay-change (next-overlay-change (point))))
  (message (get-text-property (point) :dsc)))

(defun qp-previous-match ()
  "ポイントより前のマッチにポイントを移動."
  (interactive)
  (goto-char (previous-overlay-change (previous-overlay-change (point))))
  (message (get-text-property (point) :dsc)))

(defun qp-file-open ()
  "抽出されたメモの実ファイルを read only で開きそこに飛ぶ."
  (interactive)
  (let* ((result (text-properties-at (point)))
         (file   (plist-get result :file))
         (point  (car (plist-get result :point))))
    (pop-to-buffer (find-file-noselect file))
    (setq buffer-read-only t)
    (goto-char point)))

(defvar qp-prefix "qp-")
(defmacro define-qp (name doc-string files &rest args)
  "Block record pickup command の生成.
NAME       - 生成する関数名. bp-NAME の形式になる.
DOC-STRING - 説明文.
FILES      - データファイル名かファイル名のリスト.\n
ARGS       - 必要なものだけ以下と対にしてプロパティリストで指定する.
 :beg           レコードブロックのヘッダにマッチする正規表現. 省略すると `qmemo-leader'. 
 :end           レコードブロックのエンドにマッチする正規表現. 省略すると :beg 直前まで.
 :buffer :buff  結果表示バッファ名. 省略すると `qp-buffer'.
 :mode          マッチバッファのモード. 省略すると `qp-mode'.
 :filter        マッチレコード・リストを引数にした濾過関数.
 :prefix        コマンド名プレフィクス. 省略すると `bp-prefix'. 
 :prompt        プロンプト. 省略すると `qp-prompt'. 
 :and           non-nil なら サーチワードを空白で分解して and を取る.
 :font-lock     non-nil なら :beg を `font-lock-keywords' に追加する."
  (let* ((sym name)
         (files  (if (consp files) files `(list ,files)))
         (prefix (or (plist-get args :prefix) qp-prefix))
         (prompt (or (plist-get args :prompt) qp-prompt))
         (beg    (plist-get args :beg))
         (end    (plist-get args :end))
         (buff   (or (plist-get args :buff) (plist-get args :buffer)))
         (mode   (plist-get args :mode))
         (filter (plist-get args :filter))
         (and-match (plist-get args :and))
         (add-font-lock (plist-get args :font-lock)))
    `(defun ,(intern (concat prefix (symbol-name sym))) (arg)
       ,doc-string
       (interactive
        (let ((arg (read-regexp ,prompt))) (list arg)))
       (qp-display
        arg ,files ,beg ,end ,buff ,mode ,filter ,and-match ,add-font-lock))))

;;;###autoload
(defun qhead (times)
  "`qmemo-file-name' から最初の TIMES ブロックを表示.
TIMES が 0 ならすべて表示."
  (interactive "p")
  (let ((buff "*qmemo head*")
        (times (if current-prefix-arg times qhead-default)))
    (and (get-buffer buff) (kill-buffer buff))
    (with-current-buffer (get-buffer-create buff)
      (save-excursion
        (mapc #'(lambda (n) (insert (cdr n))) (qmemo-head times))
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (qp-mode))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun qpick (re)
  "正規表現 RE を含むメモを抽出してバッファに表示.
\\{qp-mode-map}"
  (interactive (let ((re (read-regexp qp-prompt))) (list re)))
  (let* ((files
          (if qp-dir
              (qp-pure-files
               (directory-files (file-name-directory qmemo-file-name) t))
            (list qmemo-file-name))))
    (qp-display re files nil nil nil nil #'qp-sort)))

;;;###autoload
(defun qmemo (prefix)
  "Quick memo.
\\{qmemo-mode-map}"
  (interactive "P")
  (let ((height qmemo-window-height))
    (setq qmemo-buffer-file-name
          (if (buffer-file-name)
              (format "[[%s]]" (buffer-file-name))
            (format "<%s>" (buffer-name)))
          qmemo-pre-prefix prefix)
    (pop-to-buffer
     (get-buffer-create qmemo-buffer-name)
     `((display-buffer-at-bottom)
       (inhibit-same-window . t)
       (window-height . ,height)))
    (qmemo-mode)
    (buffer-disable-undo)
    (when (and (eq (point-min) (point-max))
               ;; qmemo-initial-templete を使いたい場合は
               ;; qmemo-templete-format を nil にしておく.
               ;; qmemo-templete-format 優先で
               ;; セットされていれば qmemo-initial-templete は無視される.
               (null qmemo-templete-format) 
               qmemo-initial-templete)      
      (setq qmemo-dsc   (propertize "@" 'start t)
            qmemo-body  (propertize "@" 'start t))
      (insert (qmemo-make-template qmemo-initial-templete))
      (goto-char (or (next-single-property-change (point-min) 'start)
                     (point-max)))
      (save-excursion
        (let (pos)
          (goto-char (point-min))
          (while (setq pos (next-single-property-change (point) 'start))
            (goto-char pos)
            (delete-char 1)))))
    (buffer-enable-undo)
    (set-buffer-modified-p nil)))

(defvar qmemo-mode-map
  (let ((map  (make-sparse-keymap))
        (menu (make-sparse-keymap "qmemo")))
    (define-key map  "\C-c\C-c" 'qmemo-flush)
    (define-key map  "\C-c\C-k" 'qmemo-cancel)
    (define-key map  "\C-c\C-q" 'qmemo-hold)
    (define-key map  [menu-bar qmemo] (cons "Qmemo" menu))
    (define-key menu [qmemo-flush]    '("Done"    . qmemo-flush))
    (define-key menu [qmemo-hold]     '("Suspend" . qmemo-hold))
    (define-key menu [qmemo-cancel]   '("Cancel"  . qmemo-cancel))
    map)
  "Qmemo mode Keymap.")

(defvar qp-mode-map
  (let ((map  (make-sparse-keymap))
        (menu (make-sparse-keymap "qpick")))
    (define-key map " "         'scroll-up-command)
    (define-key map [backspace] 'scroll-down-command)
    (define-key map "q"         'qmemo-hold)
    (define-key map "\M-n"      'qp-next-match)
    (define-key map "\M-p"      'qp-previous-match)
    (define-key map "\M-o"      'qp-file-open)
    (define-key map [menu-bar qpick] (cons "Qpick" menu))
    (define-key menu [qp-previous-match] '("Previous Match" . qp-previous-match))
    (define-key menu [qp-next-match] '("Next Match" . qp-next-match))
    (define-key menu [qmemo-hold] '("Close" . qmemo-hold))
    map)
  "Qpick mode Keymap.")

(defvar qmemo-menu-map
  (let ((map (make-sparse-keymap "Qmemo")))
    (define-key map [qhead] '("Qhead" . qhead))
    (define-key map [qpick] '("Qpick" . qpick))
    (define-key map [qmemo] '("Qmemo" . qmemo))
    map))

(define-key-after
  (lookup-key global-map [menu-bar Tools])
  [qmemo] (list 'menu-item "Qmemo" qmemo-menu-map)
  'simple-calculator)

(define-derived-mode qmemo-mode outline-mode "Qmemo"
  "Quick memo mode. 
\\{qmemo-mode-map}")

(eval (list 'define-derived-mode 'qp-mode qp-base-mode "Qpick"
            "Quick pickup mode.\n\\{qp-mode-map}"))

(provide 'qmemo)
;; fin.
