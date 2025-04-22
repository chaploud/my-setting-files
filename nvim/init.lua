-- クリップボード共有
vim.opt.clipboard:append("unnamedplus")

-- ヤンクでハイライト
vim.api.nvim_create_autocmd("TextYankPost", {
  group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
  callback = function()
    vim.highlight.on_yank({ timeout = 200 })
  end,
})

-- Emacsキーバインド
vim.cmd([[
  " Escape with fd
  " inoremap <silent> fd <ESC>
  
  " Save
  nnoremap <C-s> :w<CR>
  inoremap <C-s> <ESC>:w<CR>
  
  " Undo
  nnoremap <C-z> :undo<CR>
  inoremap <C-z> <ESC>:undo<CR>
  
  " Basic cursor movement and deletion keybindings from emacs, for vim.
  " insert mode
  imap <C-p> <Up>
  imap <C-n> <Down>
  imap <C-b> <Left>
  imap <C-f> <Right>
  imap <C-a> <C-o>^
  imap <C-e> <End>
  imap <C-d> <Del>
  imap <C-h> <BS>
  imap <C-k> <C-r>=<SID>kill_line()<CR>
  imap <C-y> <C-r>+
  
  " command line mode
  cmap <C-p> <Up>
  cmap <C-n> <Down>
  cmap <C-b> <Left>
  cmap <C-f> <Right>
  cmap <C-a> <Home>
  cmap <C-e> <End>
  cnoremap <C-d> <Del>
  cnoremap <C-h> <BS>
  cnoremap <C-k> <C-f>D<C-c><C-c>:<Up>
  
  " command-T window
  let g:CommandTCursorLeftMap  = ['<Left>',  '<C-b>']
  let g:CommandTCursorRightMap = ['<Right>', '<C-f>']
  let g:CommandTBackspaceMap   = ['<BS>',    '<C-h>']
  let g:CommandTDeleteMap      = ['<Del>',   '<C-d>']
  
  xnoremap p "_dP
  
  function! s:kill_line()
    let current_line = getline('.')
    let current_col = col('.')
    let text_before_cursor = (current_col > 1) ? strpart(current_line, 0, current_col - 1) : ''
    let text_after_cursor = strpart(current_line, current_col - 1)
    if len(text_after_cursor) == 0
      let current_line_num = line('.')
      if current_line_num < line('$')
        call setreg('+', "\n", 'c')
	call setreg('"', "\n", 'c')
	execute 'normal! J'
      else
	return ''
      endif
    else
      call setline('.', text_before_cursor)
      call setreg('+', text_after_cursor, 'c')
      call setreg('"', text_after_cursor, 'c')
    endif
    return ''
  endfunction
]])
