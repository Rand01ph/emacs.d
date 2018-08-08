
;; Rust support for lsp-mode using the Rust Language Server.
;; Install: rustup component add rls-preview rust-analysis rust-src
(use-package lsp-rust
  :commands lsp-rust-enable
  :hook (rust-mode . lsp-rust-enable))


(provide 'custom-rust)
