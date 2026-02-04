;; .EMACS.D/INIT.EL
;; pYTHON, oRG mODE, eVIL(vIM)キーバインドを含む総合的なeMACS設定

;; =======================================================================
;; §1. パッケージ管理 (MELPA)
;; -----------------------------------------------------------------------
;; パッケージシステムを初期化し、MELPAリポジトリからパッケージをインストールします。
;; =======================================================================

(require 'package)

;; org-roam 関連の変数は先に定義して未ロード時の参照エラーを防ぐ
(defvar my/org-base-directory nil)
(defvar my/org-roam-projects-dir nil)
(defvar my/org-roam-books-dir nil)
(defvar my/org-roam-papers-dir nil)
(defvar my/org-roam-cabinet-dir nil)
(defvar my/org-roam-weekly-dir nil)
(defvar my/org-roam-archive-dir nil)

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
;; use-package を必ず有効化
(ensure-package-installed 'use-package)
(require 'use-package)
;;(ensure-package-installed 'atom-one-dark-theme)

(require 'ox-hugo)

;; 参照用ディレクトリのベースを先に定義
(setq my/org-base-directory (file-truename "~/org-roam/"))
;; org-roam ディレクトリ構造の設定（先に定義して未ロード時の参照エラーを防ぐ）
(setq my/org-roam-projects-dir (expand-file-name "projects/" my/org-base-directory))
(setq my/org-roam-books-dir (expand-file-name "books/" my/org-base-directory))
(setq my/org-roam-papers-dir (expand-file-name "papers/" my/org-base-directory))
(setq my/org-roam-cabinet-dir (expand-file-name "cabinet/" my/org-base-directory))
(setq my/org-roam-weekly-dir (expand-file-name "weekly/" my/org-base-directory))
(setq my/org-roam-archive-dir (expand-file-name "archive/" my/org-base-directory))

(use-package org-roam
  :ensure t
  :demand t
  :custom
  (org-roam-directory (file-truename "~/org-roam/"))
  :bind (("C-c z l" . org-roam-buffer-toggle)
         ("C-c z f" . org-roam-node-find)
         ("C-c z i" . org-roam-node-insert)
         ("C-c z c" . org-roam-capture)
         ("C-c z g" . org-roam-graph))
  :config
  (org-roam-db-autosync-mode)

  ;; org-roam-capture-templatesの設定
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "cabinet/${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+filetags: \n\n")
           :unnarrowed t)

          ("p" "paper" plain "%?"
           :target (file+head "papers/${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+filetags: :paper:\n\n* 概要\n\n* メモ\n\n")
           :unnarrowed t)))

  ;; org-capture-templatesの設定
  (setq org-capture-templates
        `(("i" "Inbox (org-roam)" entry
           (file ,(expand-file-name "inbox.org" my/org-roam-cabinet-dir))
           "* INBOX %?\n"
           :prepend nil
           :empty-lines 1))))

;; =======================================================================
;; org-roam 新機能: inbox, 週次ページ, プロジェクト管理
;; -----------------------------------------------------------------------
;; org-roamを使ったGTDワークフローの機能を提供
;; =======================================================================

(with-eval-after-load 'org-roam
  ;; -----------------------------------------------------------------------
  ;; inbox機能
  ;; -----------------------------------------------------------------------
  (defun my/org-roam-inbox-open ()
    "Open or create org-roam inbox file."
    (interactive)
    (let ((inbox-file (expand-file-name "inbox.org" my/org-roam-cabinet-dir)))
      (unless (file-exists-p inbox-file)
        (make-directory my/org-roam-cabinet-dir t)
        (with-temp-file inbox-file
          (insert "#+title: Inbox\n")
          (insert "#+date: " (format-time-string "[%Y-%m-%d %a]") "\n")
          (insert "#+filetags: :inbox:\n\n")
          (insert "* INBOX\n\n")))
      (find-file inbox-file)))

  (defun my/org-roam-inbox-capture ()
    "Capture to org-roam inbox using org-capture."
    (interactive)
    (make-directory my/org-roam-cabinet-dir t)
    (org-capture nil "i")))

(global-set-key (kbd "C-c n i") #'my/org-roam-inbox-capture)
(global-set-key (kbd "C-c n o") #'my/org-roam-inbox-open)

;; -----------------------------------------------------------------------
;; 週次ページ作成機能
;; -----------------------------------------------------------------------
(defun my/org-roam-weekly-create ()
  "Create or open this week's org-roam weekly file with daily headings from Monday to Sunday."
  (interactive)
  (let* ((week-num (format-time-string "%Y-W%V"))
         (filename (expand-file-name
                    (format "%s.org" week-num)
                    my/org-roam-weekly-dir))
         ;; 今日の日付を取得
         (now (current-time))
         ;; 今週の月曜日を計算（0=日曜日, 1=月曜日, ..., 6=土曜日）
         (day-of-week (string-to-number (format-time-string "%w" now)))
         ;; 月曜日を0とするため調整（日曜日の場合は-6）
         (days-since-monday (if (= day-of-week 0) 6 (- day-of-week 1)))
         (monday (time-subtract now (days-to-time days-since-monday))))
    (make-directory my/org-roam-weekly-dir t)
    (find-file filename)
    ;; バッファが空の場合のみテンプレートを挿入（二重挿入を防ぐ）
    (when (= (buffer-size) 0)
      (insert (format "#+title: %s\n" week-num))
      (insert "#+date: " (format-time-string "[%Y-%m-%d %a]") "\n")
      (insert "#+filetags: :log:weekly:\n\n")
      (insert "* Week " (format-time-string "%V" now) "の計画\n\n")

      ;; 日曜日から月曜日の順に日付の大見出しを作成
      (dotimes (i 7)
        (let* ((day (time-add monday (days-to-time (- 6 i))))
               (date-str (format-time-string "%Y-%m-%d %a" day)))
          (insert "* " date-str "\n\n")))
      (save-buffer))
    (when (fboundp 'org-roam-db-update-file)
      (org-roam-db-update-file))))

(global-set-key (kbd "C-c n w") #'my/org-roam-weekly-create)

;; -----------------------------------------------------------------------
;; プロジェクト作成機能（タグ + プロパティのハイブリッド方式）
;; -----------------------------------------------------------------------
(defun my/org-roam-project-create (project-name)
  "Create a new project file in org-roam projects directory."
  (interactive "sProject name: ")
  (let* ((slug (downcase (replace-regexp-in-string " " "_" project-name)))
         (tag (downcase (replace-regexp-in-string " " "" project-name)))
         (filename (expand-file-name
                    (format "project_%s.org" slug)
                    my/org-roam-projects-dir)))
    (make-directory my/org-roam-projects-dir t)
    (find-file filename)
    (insert ":PROPERTIES:\n")
    (insert ":ID:       " (org-id-new) "\n")
    (insert ":OWNER:    nakano\n")
    (insert ":DEADLINE: \n")
    (insert ":PRIORITY: \n")
    (insert ":END:\n")
    (insert "#+title: project-" project-name "\n")
    (insert "#+date: " (format-time-string "[%Y-%m-%d %a]") "\n")
    (insert "#+filetags: :" tag ":open:\n\n")
    (insert "* HOME\n\n")
    (insert "* Documents\n")
    (insert "#+BEGIN: org-roam-tags :tags \"" tag "\"\n")
    (insert "#+END:\n\n")
    (insert "* Development\n\n")
    (insert "* LOG\n\n")
    (save-buffer)
    (when (fboundp 'org-roam-db-update-file)
      (org-roam-db-update-file))))

(global-set-key (kbd "C-c n p") #'my/org-roam-project-create)

;; -----------------------------------------------------------------------
;; プロジェクトステータス管理関数（タグベース）
;; -----------------------------------------------------------------------

(defun my/org-roam-project-get-open-projects ()
  "Get list of open project tags (using :open: tag)."
  (let ((project-files (when (file-directory-p my/org-roam-projects-dir)
                         (directory-files my/org-roam-projects-dir t "^project_.*\\.org$"))))
    (delq nil
          (mapcar
           (lambda (f)
             (with-temp-buffer
               (insert-file-contents f)
               (goto-char (point-min))
               (when (re-search-forward "^#\\+filetags:.*:open:" nil t)
                 (goto-char (point-min))
                 (when (re-search-forward "^#\\+filetags: *:\\([^:]+\\):" nil t)
                   (match-string 1)))))
           project-files))))

(defun my/org-roam-project-get-all-projects ()
  "Get list of all project tags."
  (let ((project-files (when (file-directory-p my/org-roam-projects-dir)
                         (directory-files my/org-roam-projects-dir t "^project_.*\\.org$"))))
    (delq nil
          (mapcar
           (lambda (f)
             (with-temp-buffer
               (insert-file-contents f)
               (goto-char (point-min))
               (when (re-search-forward "^#\\+filetags: *:\\([^:]+\\):" nil t)
                 (match-string 1))))
           project-files))))

(defun my/org-roam-book-get-all-books ()
  "Get list of all book tags."
  (let ((book-files (when (file-directory-p my/org-roam-books-dir)
                      (directory-files my/org-roam-books-dir t "^book_.*\\.org$"))))
    (delq nil
          (mapcar
           (lambda (f)
             (with-temp-buffer
               (insert-file-contents f)
               (goto-char (point-min))
               (when (re-search-forward "^#\\+filetags: *:book:\\([^:]+\\):" nil t)
                 (match-string 1))))
           book-files))))

(defun my/org-roam-book-create (book-name)
  "Create a new book file in org-roam books directory."
  (interactive "sBook name: ")
  (let* ((slug (downcase (replace-regexp-in-string " " "_" book-name)))
         (tag (downcase (replace-regexp-in-string " " "" book-name)))
         (projects (my/org-roam-project-get-open-projects))
         (selected-project (completing-read "Project tag (empty for none): " projects nil t))
         (project-tag (if (string-empty-p selected-project) "" (concat ":" selected-project ":")))
         (filename (expand-file-name
                    (format "book_%s.org" slug)
                    my/org-roam-books-dir)))
    (make-directory my/org-roam-books-dir t)
    (find-file filename)
    (insert ":PROPERTIES:\n")
    (insert ":ID:       " (org-id-new) "\n")
    (insert ":OWNER:    nakano\n")
    (insert ":END:\n")
    (insert "#+title: book-" book-name "\n")
    (insert "#+date: " (format-time-string "[%Y-%m-%d %a]") "\n")
    (insert "#+filetags: :book:" tag project-tag "\n\n")
    (insert "* Notes\n\n")
    (save-buffer)
    (when (fboundp 'org-roam-db-update-file)
      (org-roam-db-update-file))))

(global-set-key (kbd "C-c n b") #'my/org-roam-book-create)

(defun my/org-roam-project-toggle-status ()
  "Toggle project status between open and closed in current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\+filetags: *:\\([^:]*\\):open:\\([^:]*\\):" nil t)
        (progn
          (replace-match "#+filetags: :\\1:closed:\\2:" nil nil)
          (message "Project status changed to: closed"))
      (if (re-search-forward "^#\\+filetags: *:\\([^:]*\\):closed:\\([^:]*\\):" nil t)
          (progn
            (replace-match "#+filetags: :\\1:open:\\2:" nil nil)
            (message "Project status changed to: open"))
        (message "No project status tag found"))))
  (when (fboundp 'org-roam-db-update-file)
    (org-roam-db-update-file)))

(global-set-key (kbd "C-c n s") #'my/org-roam-project-toggle-status)

;; -----------------------------------------------------------------------
;; プロジェクトタグ付きノード作成機能（note/paper両対応）
;; -----------------------------------------------------------------------
(defun my/org-roam-capture-with-project-tag ()
  "Capture new org-roam node in cabinet/ or papers/ with project tag selection."
  (interactive)
  (let* ((type (completing-read "Type: " '("note" "paper") nil t))
         (projects (my/org-roam-project-get-open-projects))
         (selected-project (completing-read "Project tag (empty for none): " projects nil t))
         (title (read-string "Title: "))
         (slug (if (fboundp 'org-roam-title-to-slug)
                   (org-roam-title-to-slug title)
                 (downcase (replace-regexp-in-string " " "_" title))))
         (base-dir (if (string= type "paper") my/org-roam-papers-dir my/org-roam-cabinet-dir))
         (filename (expand-file-name
                    (format "%s.org" slug)
                    base-dir))
         (base-tags (if (string= type "paper") ":paper:" ":docs:"))
         (project-tag (if (string-empty-p selected-project) "" (concat ":" selected-project ":")))
         (tags (concat base-tags project-tag)))
    (make-directory base-dir t)
    (find-file filename)
    (insert ":PROPERTIES:\n")
    (insert ":ID:       " (org-id-new) "\n")
    (insert ":END:\n")
    (insert "#+title: " title "\n")
    (insert "#+date: " (format-time-string "[%Y-%m-%d %a]") "\n")
    (insert "#+filetags: " tags "\n\n")
    (when (string= type "paper")
      (insert "* 概要\n\n* メモ\n\n"))
    (save-buffer)
    (when (fboundp 'org-roam-db-update-file)
      (org-roam-db-update-file))))

(global-set-key (kbd "C-c n c") #'my/org-roam-capture-with-project-tag)

;; -----------------------------------------------------------------------
;; Refile設定（org-roamプロジェクトファイルへのrefile）
;; -----------------------------------------------------------------------
(defun my/org-refile-targets-dynamic ()
  "Dynamically generate refile targets including all project files."
  (let ((project-files (when (file-directory-p my/org-roam-projects-dir)
                         (directory-files my/org-roam-projects-dir t "^project_.*\\.org$")))
        (inbox-file (expand-file-name "inbox.org" my/org-roam-cabinet-dir)))
    (append
     (when project-files
       (list (cons project-files '(:maxlevel . 2))))
     (when (file-exists-p inbox-file)
       (list (cons (list inbox-file) '(:maxlevel . 1)))))))

(setq org-refile-targets nil)
(setq org-refile-target-verify-function
      (lambda ()
        (setq org-refile-targets (my/org-refile-targets-dynamic))
        t))

(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)

(global-set-key (kbd "C-c r") #'org-refile)



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
;;;; init.elをeval-buffer
(global-set-key (kbd "C-c e e") #'eval-buffer)
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

;; macOSのタイトルバーを完全に非表示 → ウィンドウの大きさと形状がいじれなくなったので流石に中止
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
(when (require 'key-chord nil 'noerror)
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state))







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
    (insert "| TITLE | TAG |\n")
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

(global-set-key (kbd "C-c z m") #'my/org-roam-find-by-multiple-tags)



;; =======================================================================
;; §4. Org Mode の設定
;; -----------------------------------------------------------------------
;; ノート作成、タスク管理、文書作成のための設定です。
;; =======================================================================
(use-package treemacs
  :ensure t
  :bind
  (:map global-map
        ("C-c t" . treemacs)))

(use-package org-super-agenda
  :ensure t
  :after org-roam
  :config
  (org-super-agenda-mode)
  (defun my/org-super-agenda-project-top-level (item)
    "Return top-level heading for ITEM only in org-roam project files."
    (let ((marker (org-super-agenda--get-marker item)))
      (when marker
        (org-super-agenda--when-with-marker-buffer marker
          (let ((file (buffer-file-name)))
            (when (and file
                       (string-prefix-p (file-truename my/org-roam-projects-dir)
                                       (file-truename file)))
              (org-back-to-heading t)
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




(defun my/org-agenda-files-roam ()
  "Return selected org-roam files for agenda."
  (let ((project-files (when (file-directory-p my/org-roam-projects-dir)
                         (directory-files my/org-roam-projects-dir t "^project_.*\\.org$")))
        (inbox-file (expand-file-name "inbox.org" my/org-roam-cabinet-dir))
        (weekly-files (when (file-directory-p my/org-roam-weekly-dir)
                        (directory-files my/org-roam-weekly-dir t "^[0-9].*\\.org$"))))
    (append
     project-files
     (when (file-exists-p inbox-file) (list inbox-file))
     weekly-files)))

(defun my/org-agenda ()
  "Open agenda using org-roam files."
  (interactive)
  (let ((org-agenda-files (my/org-agenda-files-roam)))
    (call-interactively #'org-agenda)))

;;(setq org-default-notes-file (concat org-directory "/home.org"))

(setq org-agenda-prefix-format
      '((agenda . "  %?-12t% s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))


;; TODO 状態（GTD用）
(setq org-todo-keywords
      '((sequence
         "INBOX(i)"   ; 未整理
         "NEXT(n)"    ; 次にとるべき行動
         "DONE(d)"    ; 完了
         "WIP(c)"     ; 作業中
         "WAIT(w)"    ; 待ち状態
         "HOLD(h)"    ; 保留
         "|"
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
  :hook (python-mode . company-mode)
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
  :hook (python-mode . flycheck-mode))
  
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
 '(custom-enabled-themes '(tango-dark))
 '(package-selected-packages
   '(pyvenv company lsp-mode treemacs doom-themes org-roam atom-one-dark-theme org-super-agenda org-bullets ox-hugo py-autopep8 material-theme magit key-chord flycheck evil elpy ein dap-mode blacken better-defaults)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
