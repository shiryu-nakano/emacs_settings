;; .EMACS.D/INIT.EL
;; pYTHON, oRG mODE, eVIL(vIM)キーバインドを含む総合的なeMACS設定

;; =======================================================================
;; §1. パッケージ管理 (MELPA)
;; -----------------------------------------------------------------------
;; パッケージシステムを初期化し、MELPAリポジトリからパッケージをインストールします。
;; =======================================================================

(require 'package)

;; MELPAリポジトリを追加
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; パッケージキャッシュがなければ更新
(when (not package-archive-contents)
  (package-refresh-contents))

;; 不足パッケージの自動インストール関数
(defun ensure-package-installed (package)
  "パッケージが未インストールなら自動インストール"
  (unless (package-installed-p package)
    (unless package-archive-contents (package-refresh-contents))
    (package-install package)))

;; 必要パッケージの自動インストール
(ensure-package-installed 'ox-hugo)
(ensure-package-installed 'evil)
(ensure-package-installed 'key-chord)
;;(ensure-package-installed 'atom-one-dark-theme)

(require 'ox-hugo)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org-roam/"))
  :bind (("C-c z l" . org-roam-buffer-toggle)
         ("C-c z f" . org-roam-node-find)
         ("C-c z i" . org-roam-node-insert)
         ("C-c z c" . org-roam-capture)
         ("C-c z g" . org-roam-graph))
  :config
  (org-roam-db-autosync-mode)
  
  ;; キャプチャテンプレートの設定
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+filetags: \n\n")
           :unnarrowed t)
          
          ("l" "log" plain "%?"
           :target (file+head "logs/%<%Y/%m/%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+date: %U\n#+filetags: :log:\n\n")
           :unnarrowed t))))


;; =======================================================================
;; §2. Emacsの基本設定
;; -----------------------------------------------------------------------
;; Emacs全体の基本的な外観や挙動を設定
;; =======================================================================

(setq inhibit-startup-message t)    ;; 起動メッセージを非表示に
;;(load-theme 'material t)            ;; `material`テーマを読み込み
(load-theme 'tango-dark t)
(global-display-line-numbers-mode t) ;; 行番号を常に表示

;; 便利なキーバインド設定
;;;; init.elを一発で開く
(global-set-key (kbd "C-c e i")
                (lambda () (interactive) (find-file user-init-file)))  
;;;; projects.orgを一発で開く
(global-set-key (kbd "C-c e p")
                (lambda () (interactive)
                  (find-file (expand-file-name "projects.org" my/org-base-directory))))  

;; デバッグ用
(setq debug-on-error t)

;; doom-themes の設定（use-package版）
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-dark+ t)
  (set-frame-parameter nil 'alpha 80)
  (add-to-list 'default-frame-alist '(alpha . 80)))

;; macOSのタイトルバーを完全に非表示
;;(when (eq system-type 'darwin)
;;  (set-frame-parameter nil 'undecorated t)
;;  (add-to-list 'default-frame-alist '(undecorated . t)))


;; macOS: タイトルバーを透明化（ボタンは残るがリサイズ可能）
(when (eq system-type 'darwin)
  (set-frame-parameter nil 'ns-transparent-titlebar t)
  (set-frame-parameter nil 'ns-appearance 'dark)  ; darkテーマに合わせる
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))
;; ツールバーを非表示
(tool-bar-mode 0)


;; =======================================================================
;; §3. Evil (Vimキーバインド) の設定
;; -----
;; Vimのようなモーダル編集を可能にします。
;; =======================================================================

;; Evilモードを有効化
(when (require 'evil nil 'noerror)
  (evil-mode 1))

;; "jj"でインサートモードからノーマルモードへ移行
(when (require 'key-chord nil 'noerror))
(setq key-chord-two-keys-delay 0.5)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)







;; org roamのテスト運用
(defun org-dblock-write:org-roam-tags (params)
  "タグでフィルタリングしたノード一覧を生成するダイナミックブロック"
  (let* ((tags (plist-get params :tags))
         (tags-list (if (listp tags) tags (list tags)))
         (nodes (org-roam-node-list))
         (filtered-nodes
          (seq-filter
           (lambda (node)
             (let ((node-tags (org-roam-node-tags node)))
               ;; すべてのタグを持っているかチェック（AND条件）
               (seq-every-p
                (lambda (tag) (member tag node-tags))
                tags-list)))
           nodes)))
    ;; リストを挿入
    (insert "| タイトル | タグ |\n")
    (insert "|----------+------|\n")
    (dolist (node (seq-sort-by #'org-roam-node-title #'string< filtered-nodes))
      (insert (format "| [[id:%s][%s]] | %s |\n"
                      (org-roam-node-id node)
                      (org-roam-node-title node)
                      (string-join (org-roam-node-tags node) ", "))))))

;; タグ組み合わせ検索関数
(defun my/org-roam-find-by-multiple-tags (tags)
  "複数タグを持つノードを検索（AND条件）"
  (interactive
   (list (split-string
          (read-string "Tags (スペース区切り): ")
          " " t)))
  (org-roam-node-find
   nil nil
   (lambda (node)
     (let ((node-tags (org-roam-node-tags node)))
       (seq-every-p
        (lambda (tag) (member tag node-tags))
        tags)))))

;; よく使う組み合わせのショートカット
(defun my/org-roam-dsb-papers ()
  "DSB研究関連の論文を一覧表示"
  (interactive)
  (org-roam-node-find
   nil nil
   (lambda (node)
     (let ((tags (org-roam-node-tags node)))
       (and (member "paper" tags)
            (member "DSB" tags))))))

(global-set-key (kbd "C-c z m") #'my/org-roam-find-by-multiple-tags)
(global-set-key (kbd "C-c z d") #'my/org-roam-dsb-papers)



;; =======================================================================
;; §4. Org Mode の設定
;; -----------------------------------------------------------------------
;; ノート作成、タスク管理、文書作成のための設定です。
;; =======================================================================
(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode)
  (defun my/org-super-agenda-project-top-level (item)
    "Return top-level heading for ITEM only in projects.org."
    (let ((marker (org-super-agenda--get-marker item)))
      (when marker
        (org-super-agenda--when-with-marker-buffer marker
          (let ((file (buffer-file-name)))
            (when (and file
                       (string-equal (file-truename file)
                                     (file-truename (expand-file-name "projects.org" my/org-base-directory))))
              (org-back-to-heading t)
              (while (> (org-outline-level) 1)
                (org-up-heading-safe))
              (org-get-heading t t t t)))))))
  (setq org-super-agenda-groups
        '((:auto-map my/org-super-agenda-project-top-level)
          (:auto-parent t))))



(setq system-time-locale "C") ;; 英語表記にする（December など）
;; OrgをMarkdownにエクスポートできるようにする
(require 'ox-md)

(require 'org)
;; グローバルキーバインドの設定
;;(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") #'my/org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Orgファイルの保存場所とアジェンダの設定
;;(setq org-directory "~/org-files")
;;(setq org-agenda-files (list org-directory))
;;(setq org-default-notes-file (concat org-directory "/notes.org"))

;; インライン画像の幅をファイルの記述に合わせる
(setq org-image-actual-width nil)

;; クリップボードからの画像貼り付け機能
;; (macOSの'pngpaste'コマンドが必要です)
(with-eval-after-load "org"
  (defun org-insert-clipboard-image ()
    "Generate png file from a clipboard image and insert a link to current buffer."
    (interactive)
    (let* ((filename
            (concat (file-name-nondirectory (buffer-file-name))
                    "_image/"
                    (format-time-string "%Y%m%d_%H%M%S")
                    ".png")))
      (unless (file-exists-p (file-name-directory filename))
        (make-directory (file-name-directory filename)))
      (shell-command (concat "pngpaste " filename))
      (if (file-exists-p filename)
          (insert (format "#+ATTR_ORG: :width %d\n" 40))) ; デフォルト幅を設定
      (if (file-exists-p filename)
        (insert (concat "[[file:" filename "]]")))
      (org-display-inline-images)))

  (global-set-key (kbd "C-x C-y") 'org-insert-clipboard-image))



;; =======================================================================
;; Org Directory Configuration
;; -----------------------------------------------------------------------
;; org関連のファイルのベースディレクトリを設定します。
;; このディレクトリを変更すれば、すべてのorg関連ファイルのパスが自動的に更新されます。
;; =======================================================================

;; OS別にorg-directoryを設定
(setq my/org-base-directory
      (cond
       ((eq system-type 'darwin)        ; macOS
        "~/CABiNET/org2")
       ((eq system-type 'windows-nt)    ; Windows  
        "C:/Users/NakanoShiryu/Documents/CABiNET/org2")  ; 実際のWindows上のCABiNETディレクトリ
       (t "~/CABiNET/org2")))           ; その他のOS（Ubuntu等）

;; Orgファイルの保存場所とアジェンダの設定
(setq org-directory my/org-base-directory)

(defvar my/org-agenda-base-files
  (list (expand-file-name "inbox.org" my/org-base-directory)
        (expand-file-name "projects.org" my/org-base-directory)
        (expand-file-name "someday.org" my/org-base-directory))
  "Base files always included in the agenda (not used for daily view).")

;; Agenda uses only today's daily log file
(defun my/org-agenda-files-today ()
  "Return a list with today's daily log file, or an empty list."
  (let* ((base my/org-base-directory)
         (date (or (and (boundp 'org-agenda-current-date)
                        org-agenda-current-date)
                   (calendar-current-date)))
         (month (nth 0 date))
         (day (nth 1 date))
         (year (nth 2 date))
         (daily (expand-file-name
                 (format "daily/%04d/%02d/%04d-%02d-%02d.org"
                         year month year month day)
                 base)))
    (if (file-exists-p daily)
        (list daily)
      '())))

(defun my/org-agenda ()
  "Open agenda using only today's daily log file."
  (interactive)
  (let ((org-agenda-files (my/org-agenda-files-today)))
    (call-interactively #'org-agenda)))

;;(setq org-default-notes-file (concat org-directory "/home.org"))

;; Agenda: show subtask context under top-level projects.org headings.
(defun my/org-agenda-project-subpath ()
  "Return indent and parent-path under top-level projects.org headings."
  (let ((marker (or (org-get-at-bol 'org-hd-marker)
                    (org-get-at-bol 'org-marker))))
    (if (not marker)
        ""
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (let ((file (buffer-file-name)))
            (if (not (and file
                          (string-equal (file-truename file)
                                        (file-truename (expand-file-name "projects.org" my/org-base-directory)))))
                ""
              (let* ((level (org-outline-level))
                     (indent (make-string (* 2 (max 0 (1- level))) ?\s))
                     (path (org-get-outline-path nil t)) ;; parents only
                     (subpath (cdr path))
                     (label (if (and subpath (car subpath))
                                (concat (string-join subpath " > ") " / ")
                              "")))
                (concat indent label)))))))))

(setq org-agenda-prefix-format
      '((agenda . "  %?-12t% s")
        (todo . " %i %-12:c %(my/org-agenda-project-subpath)")
        (tags . " %i %-12:c %(my/org-agenda-project-subpath)")
        (search . " %i %-12:c %(my/org-agenda-project-subpath)")))

;;Journal
;;(defun my/org-journal-file ()
;;  "今年のジャーナルファイル ~/org/log-YYYY.org を返す."
;;  (expand-file-name
;;   (format "log-%s.org" (format-time-string "%Y"))
;;   "~/org"))


;;日時ログ用のヘルパー関数

;; 前日のファイルから指定セクションの内容を取得
(defun my/get-section-content-from-file (file section-name)
  "FILE から SECTION-NAME 見出しの内容（サブツリー全体）を返す。
見出しが存在しない場合は空文字列を返す。"
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (delay-mode-hooks (org-mode))
        (goto-char (point-min))
        (if (re-search-forward (concat "^\\* " (regexp-quote section-name) "\\b") nil t)
            (let ((start (line-beginning-position)))
              (org-end-of-subtree t t)
              (buffer-substring-no-properties start (point)))
          ""))
    ""))

(defun my/org-daily-log-section (section-name)
  "Return the end-of-subtree position of SECTION-NAME in today's daily log file.
If SECTION-NAME doesn't exist, create it at the end of the file."
  (let* ((buf (my/open-today-daily-log-impl)))
    (set-buffer buf)
    (goto-char (point-min))
    (if (re-search-forward (concat "^\\* " (regexp-quote section-name) "\\b") nil t)
        (progn
          (org-end-of-subtree t t)  ;; section-name のサブツリー終わりへ
          (unless (bolp) (insert "\n"))
          (current-buffer))
      ;; section が見つからない場合：最後に作成
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (concat "* " section-name "\n"))
      (current-buffer))))


(defun my/org-pick-project-tag ()
  "projects.org の最上位見出しからプロジェクト名を拾い、タグ文字列を返す。空入力ならタグなし。"
  (let* ((projects-file (expand-file-name "projects.org" org-directory))
         (cands
          (when (file-exists-p projects-file)
            (with-current-buffer (find-file-noselect projects-file)
              (org-mode)
              (org-map-entries
               (lambda ()
                 ;; レベル1見出しのタイトルを取り出す
                 (nth 4 (org-heading-components)))
               "LEVEL=1"))))
         (cands (delete-dups (delq nil cands)))
         (choice (completing-read
                  "Project tag (空ならなし): "
                  cands nil t nil nil "")))
    (if (string= choice "")
        ""                              ; タグなし
      (format " :%s:" (upcase choice))))) ; 例: :arcanain2025:

(defun my/org-daily-log-file ()
  "Ensure today's daily log exists and return its file path."
  (let ((buf (my/open-today-daily-log-impl)))
    (buffer-file-name buf)))

(defun my/org-capture-inbox-target ()
  "Ensure today's daily log exists and return the inbox target buffer."
  (my/open-today-daily-log-impl)
  (my/org-daily-log-section "inbox"))


(defun my/open-today-daily-log-impl ()
  "Ensure today's daily log file exists with all sections initialized.
Returns the buffer (for compatibility with capture)."
  (let* ((base-dir (expand-file-name "daily" org-directory))
         (rel-path (format-time-string "%Y/%m/%Y-%m-%d.org"))
         (file     (expand-file-name rel-path base-dir))
         (new-file (not (file-exists-p file)))
         (yesterday (time-subtract (current-time) (days-to-time 1)))
         (yesterday-rel-path (format-time-string "%Y/%m/%Y-%m-%d.org" yesterday))
         (yesterday-file (expand-file-name yesterday-rel-path base-dir)))
    ;; ディレクトリがなければ作成
    (make-directory (file-name-directory file) t)
    ;; ファイルを開く
    (set-buffer (find-file-noselect file))
    ;; 新規ファイルならヘッダと骨組みを挿入
    (when new-file
      ;; 前日のファイルから各セクションを取得
      (let* (;; 前日のファイルから各セクションを取得
             (home-content (my/get-section-content-from-file yesterday-file "Home"))
             (deadline-content (my/get-section-content-from-file yesterday-file "直近の予定締め切り"))
             (task-content (my/get-section-content-from-file yesterday-file "TASK"))
             (task-organize-content (my/get-section-content-from-file yesterday-file "タスク整理"))
             (agile-content (my/get-section-content-from-file yesterday-file "Agile"))
             (inbox-content (my/get-section-content-from-file yesterday-file "inbox")))
        (erase-buffer)
        (insert (format "#+title: %s\n#+filetags: :daily:\n#+OPTIONS: toc:nil num:nil ^:nil tags:nil todo:nil H:10\n#+OPTIONS: broken-links:mark\n#+OPTIONS: tex:t\n#+OPTIONS: html-postamble:nil\n#+OPTIONS: links:nil\n\n"
                        (format-time-string "%Y-%m-%d")))
        ;; 前日からコピーするか、空のセクションを作成
        (if (string-empty-p home-content)
            (insert "* Home\n\n")
          (insert home-content "\n"))
        (if (string-empty-p deadline-content)
            (insert "* 直近の予定締め切り\n\n")
          (insert deadline-content "\n"))
        (if (string-empty-p task-content)
            (insert "* TASK\n\n")
          (insert task-content "\n"))
        (insert "* LOG\n\n")
        (if (string-empty-p task-organize-content)
            (insert "* タスク整理\n\n")
          (insert task-organize-content "\n"))
        (if (string-empty-p agile-content)
            (progn
              (insert "* Agile\n")
              (insert "** Engineer\n\n")
              (insert "** Study\n\n")
              (insert "** 研究\n\n")
              (insert "** 海外研究\n\n")
              (insert "** 音楽\n\n"))
          (insert agile-content "\n"))
        (insert "* 所感\n\n")
        (if (string-empty-p inbox-content)
            (insert "* inbox\n\n")
          (insert inbox-content "\n"))))
    (unless (save-excursion
              (goto-char (point-min))
              (re-search-forward "^\\* inbox\\b" nil t))
      (let ((inbox-content (my/get-section-content-from-file yesterday-file "inbox")))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (if (string-empty-p inbox-content)
            (insert "* inbox\n\n")
          (insert inbox-content "\n"))))
    (current-buffer)))
(defun my/open-today-daily-log ()
  "今日の日次ログファイルを一発で開く。
必要ならファイルと見出しを作成する。"
  (interactive)
  ;; my/open-today-daily-log-impl はバッファを返すので、それを switch-to-buffer で表示
  (let ((buf (my/open-today-daily-log-impl)))
    (switch-to-buffer buf)
    ;; 好みで位置を調整：LOGの先頭 or ファイル先頭など
    ;; LOGの位置に飛びたいなら：
    (goto-char (point-min))
    (when (re-search-forward "^\\* LOG\\b" nil t)
      (forward-line 1))))

(global-set-key (kbd "C-c n d") #'my/open-today-daily-log)

(defun my/org-archive-done-to-task-archive ()
  "Archive DONE tasks in the current buffer to archives/task.org."
  (interactive)
  (let* ((archive-file (expand-file-name "archives/task.org" my/org-base-directory))
         (org-archive-location (concat archive-file "::"))
         (positions nil))
    (org-map-entries
     (lambda () (push (point-marker) positions))
     "TODO=\"DONE\""
     'file)
    (if (null positions)
        (message "No DONE tasks to archive.")
      (setq positions
            (sort positions
                  (lambda (a b)
                    (> (marker-position a) (marker-position b)))))
      (dolist (m positions)
        (when (marker-buffer m)
          (with-current-buffer (marker-buffer m)
            (goto-char m)
            (org-archive-subtree))))
      (message "Archived %d DONE task(s)." (length positions)))))

(global-set-key (kbd "C-c n a") #'my/org-archive-done-to-task-archive)

;; org captureのショートカット設定
(setq org-capture-templates
      `(
        ;; Inbox (captured in today's daily log file inbox section)
        ("i" "Inbox task" plain
         (function my/org-capture-inbox-target)
         "** INBOX %?")

	;; Daily log （日時ログ）
	("j" "Daily Log" plain
         (function (lambda () (my/org-daily-log-section "LOG")))
         "** %<%Y-%m-%d %H:%M> %(my/org-pick-project-tag)\n%?")

	;; knowledge
	("k" "Knowledge" entry
	 (file (lambda () (expand-file-name "knowledge.org" my/org-base-directory)))
	  "* %?\n  Created on %U")

	;;temp
	("t" "Temp" entry
	 (file (lambda () (expand-file-name "temp.org" my/org-base-directory)))
	 "* %?\n  Created on %U")

	;;research idea
	("r" "research idea" entry
	 (file (lambda () (expand-file-name "idea.org" my/org-base-directory)))
	 "* %?\n  Created on %U")
	))


;; Refile 設定（inbox から projects / someday に送る）
(setq org-refile-targets
      `((,(expand-file-name "projects.org" my/org-base-directory) :maxlevel . 1)
        (,(expand-file-name "someday.org" my/org-base-directory)  :maxlevel . 1)
	(,(expand-file-name "tips.org" my/org-base-directory)  :maxlevel . 1)
	(,(expand-file-name "papers.org" my/org-base-directory)  :maxlevel . 1)
	(,(expand-file-name "archive.org" my/org-base-directory)  :maxlevel . 1)
	))

(setq org-outline-path-complete-in-steps nil) ; 一発でパス補完
(setq org-refile-use-outline-path 'file)

;; ショートカット（お好み）
(global-set-key (kbd "C-c r") #'org-refile)


;; TODO 状態（GTD用）
(setq org-todo-keywords
      '((sequence
         "INBOX(i)"   ; 未整理
         "NEXT(n)"    ; 次にとるべき行動
         "WIP(c)"     ; 作業中
         "WAIT(w)"    ; 待ち状態
         "HOLD(h)"    ; 保留
         "|"
         "DONE(d)"    ; 完了
         "CANCEL(x)"  ; 中止
         )))

;; 見た目上わかりやすく
(setq org-todo-keyword-faces
      '(("INBOX" . "orange")
        ("NEXT"  . "cyan")
        ("WIP"   . "deep sky blue")
        ("WAIT"  . "yellow")
        ("HOLD"  . "magenta")
        ("CANCEL" . "grey")))


(setq org-agenda-custom-commands
       '(("n" "Next Actions"
         ((todo "NEXT|WIP")))
        ("w" "Waiting"
         ((todo "WAIT|WIP")))
        ("h" "On Hold"
         ((todo "HOLD|WIP")))))



(setq org-agenda-custom-commands
      '(
        ;; ---- 既存 ----
        ("n" "Next Actions" ((todo "NEXT")))
        ("w" "Waiting"      ((todo "WAIT")))
        ("h" "On Hold"      ((todo "HOLD")))
        ("T" "All Tasks"    ((todo "")))

        ;; ---- ★ 新規：週次レビュー ★ ----
        ("R" "Weekly Review"
         (
          ;; 1. 今週完了したタスク
          (tags-todo "+DONE"
                     ((org-agenda-overriding-header "① 今週の DONE（完了タスク）")))

          ;; 2. Next Action が存在するプロジェクト
          (todo "NEXT"
                ((org-agenda-overriding-header "② 次に進めるタスク（Next Actions）")
                 (org-agenda-sorting-strategy '(priority-down))))

          ;; 3. 依頼中（WAIT）の棚卸し
          (todo "WAIT"
                ((org-agenda-overriding-header "③ 他人待ち（Waiting）")))

          ;; 4. 保留（HOLD）の見直し
          (todo "HOLD"
                ((org-agenda-overriding-header "④ 保留中（Hold）")))

          ;; 5. Someday の見直し
          (tags-todo "HOLD"
                     ((org-agenda-files (list (expand-file-name "someday.org" my/org-base-directory)))
                      (org-agenda-overriding-header "⑤ Someday / Maybe の棚卸し")))
          )
         )
        ))


;; 今日のノートを開くショートカット
;;(global-set-key (kbd "C-c n d") #'org-roam-dailies-capture-today)
;;(use-package org-roam
;;  :ensure t
;;  :custom
;; (org-roam-directory "~/org/roam")
;; :config
;;  (org-roam-db-autosync-mode))
;;(setq org-roam-dailies-directory "daily/")
;;
;;(setq org-roam-dailies-capture-templates
;;      '(("d" "default" entry
;;         "* 直近の予定締め切り\n\n\n* TASK\n\n* LOG\n** %<%H:%M>\n%?\n\n* タスク整理\n\n* 所感\n"
;;         :target (file+head "%<%Y/%m/%Y-%m-%d>.org"
;;                            "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n"))))


;; ====== 練習用↑


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. 必須パッケージ (コア機能)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 言語機能の中核 (LSP連携)
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (python-mode . lsp-deferred) ; .pyファイルを開いたらLSPを起動
  :init
  (setq lsp-keymap-prefix "C-c l") ; LSP関連コマンドのキーを C-c l から始める
  :config
  ;; Pyrightを言語サーバーとして使う設定
  (setq lsp-python-ms-executable "pyright-langserver")
  (setq lsp-python-ms-args '("--stdio")))

;; 補完UI
(use-package company
  :ensure t
  ;;:hook (after-init . global-company-mode) ; Emacs起動時に有効化
  ;; python-modeのときだけ有効にするように変更
  :hook (python-mode . flycheck-mode)
  :config
  (setq company-idle-delay 0.2)) ; 0.2秒待ってから補完候補を表示


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. 推奨パッケージ (コード品質)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 構文チェック (flycheckがruffを自動で認識して使います)
(use-package flycheck
  :ensure t
  ;;:hook (after-init . global-flycheck-mode)) ; Emacs起動時に有効化
  ;; python-modeのときだけ有効にするように変更
  :hook (python-mode . flycheck-mode));; python-modeのときだけ有効にするように変更
  
;; Pythonモード全体に適用する設定
(add-hook 'python-mode-hook
          (lambda ()
            ;; 保存時に自動でフォーマット(black)を実行する
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. オプションパッケージ (便利な追加機能)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 仮想環境の管理
(use-package pyvenv
  :ensure t
  :hook (python-mode . pyvenv-mode) ; .pyファイルを開いたらpyvenvを有効化
  :config ;; pyenvが管理する仮想環境の場所を教える
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

;; git操作
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)) ; C-x g でmagitを起動

;; デバッグ機能
(use-package dap-mode
  :ensure t
  :hook (python-mode . dap-auto-configure-mode)
  :config
  (require 'dap-python))

;; --- 設定ファイルの終わり ---
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(manoj-dark))
 '(custom-safe-themes
   '("2ab8cb6d21d3aa5b821fa638c118892049796d693d1e6cd88cb0d3d7c3ed07fc" "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "b99ff6bfa13f0273ff8d0d0fd17cc44fab71dfdc293c7a8528280e690f084ef0" "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0" "bb0f3ae2f6f6f6dbbbe03df66d74ca0aecefa6723ac1686f421dd1ffe26b71c3" "e622620e5f31216fd71e492fc1476e1fe7c21b8dc5811bf9e640c4b1fd6cfac1" "5244ba0273a952a536e07abaad1fdf7c90d7ebb3647f36269c23bfd1cf20b0b8" "9e5e0ff3a81344c9b1e6bfc9b3dcf9b96d5ec6a60d8de6d4c762ee9e2121dfb2" "70c88c01b0b5fde9ecf3bb23d542acba45bb4c5ae0c1330b965def2b6ce6fac3" "166a2faa9dc5b5b3359f7a31a09127ebf7a7926562710367086fcc8fc72145da" "7de64ff2bb2f94d7679a7e9019e23c3bf1a6a04ba54341c36e7cf2d2e56e2bcc" "75eef60308d7328ed14fa27002e85de255c2342e73275173a14ed3aa1643d545" "4d5d11bfef87416d85673947e3ca3d3d5d985ad57b02a7bb2e32beaf785a100e" "a6920ee8b55c441ada9a19a44e9048be3bfb1338d06fc41bce3819ac22e4b5a1" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290" "ba4f725d8e906551cfab8c5f67e71339f60fac11a8815f51051ddb8409ea6e5c" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "c9d837f562685309358d8dc7fccb371ed507c0ae19cf3c9ae67875db0c038632" "f6ea954a9544b0174a876d195387f444da441535ee88c7fb0fc346af08b0d228" "c07f072a88bed384e51833e09948a8ab7ca88ad0e8b5352334de6d80e502da8c" "6963de2ec3f8313bb95505f96bf0cf2025e7b07cefdb93e3d2e348720d401425" "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176" "f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326" "b7a09eb77a1e9b98cafba8ef1bd58871f91958538f6671b22976ea38c2580755" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" "e1df746a4fa8ab920aafb96c39cd0ab0f1bac558eff34532f453bd32c687b9d6" "4b88b7ca61eb48bb22e2a4b589be66ba31ba805860db9ed51b4c484f3ef612a7" "c3c135e69890de6a85ebf791017d458d3deb3954f81dcb7ac8c430e1620bb0f1" "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" "599f72b66933ea8ba6fce3ae9e5e0b4e00311c2cbf01a6f46ac789227803dd96" "22a0d47fe2e6159e2f15449fcb90bbf2fe1940b185ff143995cc604ead1ea171" "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641" "83550d0386203f010fa42ad1af064a766cfec06fc2f42eb4f2d89ab646f3ac01" "9b9d7a851a8e26f294e778e02c8df25c8a3b15170e6f9fd6965ac5f2544ef2a9" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "720838034f1dd3b3da66f6bd4d053ee67c93a747b219d1c546c41c4e425daf93" "1f292969fc19ba45fbc6542ed54e58ab5ad3dbe41b70d8cb2d1f85c22d07e518" "7c3d62a64bafb2cc95cd2de70f7e4446de85e40098ad314ba2291fc07501b70c" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" default))
 '(package-selected-packages
   '(atom-one-dark-theme org-super-agenda org-bullets ox-hugo py-autopep8 material-theme magit key-chord flycheck evil elpy ein dap-mode blacken better-defaults)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
