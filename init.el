;; .emacs.d/init.el
;; Python, Org Mode, Evil(Vim)キーバインドを含む総合的なEmacs設定

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
(ensure-package-installed 'atom-one-dark-theme)

(require 'ox-hugo)


;; =======================================================================
;; §2. Emacsの基本設定
;; -----------------------------------------------------------------------
;; Emacs全体の基本的な外観や挙動を設定します。
;; =======================================================================

(setq inhibit-startup-message t)    ;; 起動メッセージを非表示に
;;(load-theme 'material t)            ;; `material`テーマを読み込み
(load-theme 'atom-one-dark t)
(global-display-line-numbers-mode t) ;; 行番号を常に表示

;; 便利なキーバインド設定
(global-set-key (kbd "C-c e i")
                (lambda () (interactive) (find-file user-init-file)))  ;; init.elを一発で開く

;; デバッグ用
(setq debug-on-error t)


;; =======================================================================
;; §3. Evil (Vimキーバインド) の設定
;; -----
;; Vimのようなモーダル編集を可能にします。
;; =======================================================================

(when (require 'evil nil 'noerror)
  (evil-mode 1)) ;; Evilモードを有効化

;; "jj"でインサートモードからノーマルモードへ移行
(when (require 'key-chord nil 'noerror))
(setq key-chord-two-keys-delay 0.5)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)


;; =======================================================================
;; §4. Org Mode の設定
;; -----------------------------------------------------------------------
;; ノート作成、タスク管理、文書作成のための設定です。
;; =======================================================================
(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode))

(setq org-super-agenda-groups
      '((:auto-parent t)))  ;; 親見出しごとに自動グループ化


;; org-bullets: 見出しの * を丸いアイコンにしてくれる
;;(use-package org-bullets
;;  :ensure t
;;  :hook (org-mode . org-bullets-mode))


(setq system-time-locale "C") ;; 英語表記にする（December など）
;; OrgをMarkdownにエクスポートできるようにする
(require 'ox-md)

(require 'org)
;; グローバルキーバインドの設定
;;(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
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
        "C:/Users/Owner/CABiNET/org2")  ; 実際のWindows上のCABiNETディレクトリ
       (t "~/CABiNET/org2")))           ; その他のOS（Ubuntu等）

;; Orgファイルの保存場所とアジェンダの設定
(setq org-directory my/org-base-directory)
(setq org-agenda-files (list (expand-file-name "inbox.org" my/org-base-directory)
                             (expand-file-name "projects.org" my/org-base-directory)
                             (expand-file-name "someday.org" my/org-base-directory)))

;;(setq org-default-notes-file (concat org-directory "/home.org"))

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

(defun my/org-daily-log-target ()
  "日次ログ用ファイル ~/org/daily/YYYY/MM/YYYY-MM-DD.org の
* LOG 見出しの末尾（子見出しと本文を含むサブツリーの終わり）を
capture の挿入位置として返す。
新規ファイル作成時は前日のファイルから Home, 直近の予定締め切り, TASK, TASK整理, Agile をコピーする。"
  (let* ((base-dir (expand-file-name "daily" org-directory))
         (rel-path (format-time-string "%Y/%m/%Y-%m-%d.org"))
         (file     (expand-file-name rel-path base-dir))
         (new-file (not (file-exists-p file))))
    ;; ディレクトリがなければ作成
    (make-directory (file-name-directory file) t)
    ;; ファイルを開く
    (set-buffer (find-file-noselect file))
    ;; 新規ファイルならヘッダと骨組みを挿入
    (when new-file
      ;; 前日のファイルパスを計算（月またぎに対応）
      (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
             (yesterday-rel-path (format-time-string "%Y/%m/%Y-%m-%d.org" yesterday))
             (yesterday-file (expand-file-name yesterday-rel-path base-dir))
             ;; 前日のファイルから各セクションを取得
             (home-content (my/get-section-content-from-file yesterday-file "Home"))
             (deadline-content (my/get-section-content-from-file yesterday-file "直近の予定締め切り"))
             (task-content (my/get-section-content-from-file yesterday-file "TASK"))
             (task-organize-content (my/get-section-content-from-file yesterday-file "タスク整理"))
             (agile-content (my/get-section-content-from-file yesterday-file "Agile")))
        (erase-buffer)
        (insert (format "#+title: %s\n#+filetags: :daily:\n\n"
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
        (insert "* 所感\n\n")))
    ;; * LOG を探して、そのサブツリーの末尾へ
    (goto-char (point-min))
    (if (re-search-forward "^\\* LOG\\b" nil t)
        (progn
          (org-end-of-subtree t t)  ;; LOG のサブツリー（子＋本文）終わりへ
          (unless (bolp) (insert "\n"))
          (current-buffer))
      ;; LOG が見つからない場合の保険
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* LOG\n")
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


(defun my/open-today-daily-log ()
  "今日の日次ログファイルを一発で開く。
必要ならファイルと見出しを作成する。"
  (interactive)
  ;; my/org-daily-log-target はバッファを返すので、それを switch-to-buffer で表示
  (let ((buf (my/org-daily-log-target)))
    (switch-to-buffer buf)
    ;; 好みで位置を調整：LOGの先頭 or ファイル先頭など
    ;; LOGの位置に飛びたいなら：
    (goto-char (point-min))
    (when (re-search-forward "^\\* LOG\\b" nil t)
      (forward-line 1)))
)

(global-set-key (kbd "C-c n d") #'my/open-today-daily-log)

;; org captureのショートカット設定
(setq org-capture-templates
      `(
        ;; Inbox
        ("i" "Inbox task" entry
         (file (lambda () (expand-file-name "inbox.org" my/org-base-directory)))
         "* INBOX %?\n  Created on %U")

	;; Daily log （日時ログ）
        ;;("j" "Daily Log" plain
        ;; (function my/org-daily-log-target)
        ;;"** %<%H:%M>\n%?")
	("j" "Daily Log" plain
         (function my/org-daily-log-target)
         "** %<%Y-%m-%d %H:%M> %(my/org-pick-project-tag)\n%?")

	;; knowledge
	("k" "Knowledge" entry
	 (file (lambda () (expand-file-name "knowledge.org" my/org-base-directory)))
	  "* %?\n  Created on %U")

	;;temp
	("t" "Temp" entry
	 (file (lambda () (expand-file-name "temp.org" my/org-base-directory)))
	 "* %?\n  Created on %U")

	;;temp
	("r" "research idea" entry
	 (file (lambda () (expand-file-name "idea.org" my/org-base-directory)))
	 "* %?\n  Created on %U")
	)
      )


;; Refile 設定（inbox から projects / someday に送る）
(setq org-refile-targets
      `((,(expand-file-name "projects.org" my/org-base-directory) :maxlevel . 1)
        (,(expand-file-name "someday.org" my/org-base-directory)  :maxlevel . 1)
	(,(expand-file-name "tips.org" my/org-base-directory)  :maxlevel . 1)
	(,(expand-file-name "papers.org" my/org-base-directory)  :maxlevel . 1)
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
         "WAIT(w)"    ; 待ち状態
         "HOLD(h)"    ; 保留
         "|"
         "DONE(d)"    ; 完了
         "CANCEL(c)"  ; 中止
         )))

;; 見た目上わかりやすく
(setq org-todo-keyword-faces
      '(("INBOX" . "orange")
        ("NEXT"  . "cyan")
        ("WAIT"  . "yellow")
        ("HOLD"  . "magenta")
        ("CANCEL" . "grey")))


(setq org-agenda-custom-commands
       '(("n" "Next Actions"
         ((todo "NEXT")))
        ("w" "Waiting"
         ((todo "WAIT")))
        ("h" "On Hold"
         ((todo "HOLD")))))



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
 '(custom-enabled-themes '(tango-dark))
 '(package-selected-packages
   '(atom-one-dark-theme org-super-agenda org-bullets ox-hugo py-autopep8 material-theme magit key-chord flycheck evil elpy ein dap-mode blacken better-defaults)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
