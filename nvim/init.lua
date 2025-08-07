-- Recenter Top Bottom
-- A function that cycles the cursor position between center, top, and bottom
-- of the screen by pressing ctrl+l repeatedly
-- If ctrl+l is not pressed for 2 seconds, the next execution starts from centering
local recenter_state = 0
local last_recenter_time = 0
local CONSECUTIVE_THRESHOLD = 2000 -- milliseconds
local function recenter_cycle()
  -- Reset if a certain amount of time has passed
  local current_time = vim.uv.now()
  if current_time - last_recenter_time > CONSECUTIVE_THRESHOLD then
    recenter_state = 0
  end

  -- Define positions and commands in an array
  local positions = {
    { pos = "center", cmd = "zz" },
    { pos = "top",    cmd = "zt" },
    { pos = "bottom", cmd = "zb" }
  }

  -- Get the position corresponding to the current state
  local current = positions[recenter_state + 1]

  -- Execute appropriately depending on whether it's VSCode or native Neovim
  if vim.g.vscode then
    vim.fn.VSCodeExtensionNotify('reveal', current.pos, 0)
  else
    vim.cmd("normal! " .. current.cmd)
  end

  -- Update state (0→1→2→0→...)
  recenter_state = (recenter_state + 1) % 3
  last_recenter_time = current_time
end

-- vscode.call
local vscode
local function call_vscode(name, opts, timeout)
  if vim.g.vscode then
    if not vscode then
      vscode = require('vscode')
    end
    return vscode.call(name, opts, timeout)
  end
end

-- Share clipboard with OS
vim.opt.clipboard:append("unnamedplus")
-- Ignore case or not in search as convenient
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- In Clojure, want <C-w> to stop at / and .
vim.api.nvim_create_autocmd("FileType", {
  pattern = "clojure",
  callback = function()
    -- Remove / and . from iskeyword
    vim.opt_local.iskeyword:remove({ "/", "." })
  end,
})

-- keymap: 'i': insert, 'n': normal, 'v': visual, 'c': command
-- Bind Recenter Top Bottom to ctrl+l
vim.keymap.set({ 'n', 'v', 'i' }, '<C-l>', recenter_cycle)

-- In insert mode, pressing 'fd' quickly also acts as escape
vim.keymap.set('i', 'fd', '<ESC>')

-- Emulate Emacs keybindings (move/edit only) in insert mode
vim.keymap.set('i', '<C-p>', '<Up>')          -- up
vim.keymap.set('i', '<C-n>', '<Down>')        -- down
vim.keymap.set('i', '<C-b>', '<Left>')        -- left
vim.keymap.set('i', '<C-f>', '<Right>')       -- right
vim.keymap.set('i', '<C-a>', '<C-o>^')        -- move to line start
vim.keymap.set('i', '<C-e>', '<End>')         -- move to line end
vim.keymap.set('i', '<C-d>', '<Del>')         -- Del
vim.keymap.set('i', '<C-k>', '<C-o>d$')       -- delete to end of line
vim.keymap.set('i', '<C-y>', '<C-r>+')        -- paste last yank
vim.keymap.set({ 'i', 'c' }, '<C-h>', '<BS>') -- BackSpace

-- for yanky kill-ring
vim.keymap.set('n', 'p', '<Plug>(YankyPutAfter)')
vim.keymap.set('n', 'P', '<Plug>(YankyPutBefore)')
vim.keymap.set('n', 'gp', '<Plug>(YankyGPutAfter)')
vim.keymap.set('n', 'gP', '<Plug>(YankyGPutBefore)')
vim.keymap.set('n', '<C-p>', function()
  return require('yanky').can_cycle() and '<Plug>(YankyPreviousEntry)' or call_vscode('workbench.action.quickOpen')
end, { expr = true })
vim.keymap.set('n', '<C-n>', '<Plug>(YankyNextEntry)')

-- Set <leader> key to space
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Install lazy.nvim (only on first run)
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out,                            "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

-- lazy.nvim settings and plugin loading
require("lazy").setup({
  spec = {
    {
      -- Add surrounding text objects like (), [], {}, ""
      "kylechui/nvim-surround",
      version = "^3.0.0",
      event = "VeryLazy",
      config = function()
        require("nvim-surround").setup({})
      end
    },
    {
      -- Improve search with *
      "rapan931/lasterisk.nvim",
      event = "VeryLazy",
      config = function()
        vim.keymap.set('n', '*', function() require("lasterisk").search() end)
      end
    },
    {
      -- yank kill-ring
      "gbprod/yanky.nvim",
      event = "VeryLazy",
      opts = {
        ring = {
          cancel_event = "move",
        },
        highlight = {
          on_put = false,
          on_yank = true,
          timer = 200,
        },
      }
    },
    {
      -- im-select.nvim (require 'macism' installation)
      "keaising/im-select.nvim",
      config = function()
        require("im_select").setup({
          default_im_select = "net.mtgto.inputmethod.macSKK.ascii",
          set_default_events = { "InsertLeave", "CmdlineLeave", "VimEnter", "FocusGained" },
        })
      end,
    },
    {
      -- which-key
      "folke/which-key.nvim",
      event = "VeryLazy",
      opts = {
        delay = 0,
        show_help = false,
        show_keys = false,
        icons = {
          mappings = false
        }
      },
      keys = {
        -- Show All Commands
        {
          "<leader><leader>",
          function() call_vscode('workbench.action.showCommands') end,
          desc = "Show All Commands"
        },
        -- Hlsearch highlights off
        {
          "<leader>h",
          vim.cmd.nohlsearch,
          desc = "Highlights Off"
        },
        -- File operations
        {
          "<leader>f",
          group = "File"
        },
        -- Select Project's file
        {
          "<leader>ff",
          function() call_vscode('workbench.action.quickOpen') end,
          desc = "quickOpen"
        },
        -- Focus Explorer in side bar
        {
          "<leader>ft",
          function() call_vscode('workbench.view.explorer') end,
          desc = "File Explorer"
        },
        -- Select Opened file
        {
          "<leader>bb",
          function() call_vscode('workbench.action.showAllEditors') end,
          desc = "Select Opened File"
        },
        -- Focus Search in side bar
        {
          "<leader>s",
          function() call_vscode('workbench.view.search') end,
          desc = "Search in Project"
        },
        -- Rename symbol under cursor (and all its references)
        {
          "<leader>rn",
          function() call_vscode('editor.action.rename') end,
          desc = "Rename Symbol"
        },
        -- Remote Containers: reopen in container
        {
          "<leader>rr",
          function() call_vscode('remote-containers.reopenInContainer') end,
          desc = "Reopen in Container"
        },
        -- Go to next marker (error, warning, etc.)
        {
          "<leader>n",
          function() call_vscode('editor.action.marker.next') end,
          desc = "Next Marker"
        },
        -- Go to previous marker (error, warning, etc.)
        {
          "<leader>p",
          function() call_vscode('editor.action.marker.prev') end,
          desc = "Previous Marker"
        },
        -- Go to next change and show diff
        {
          "<leader>dd",
          function() call_vscode('editor.action.dirtydiff.next') end,
          desc = "Show Next Diff"
        },
        -- Start debugging
        {
          "<leader>ds",
          function() call_vscode('workbench.action.debug.start') end,
          desc = "Debug Start"
        },
        -- Run without debugging
        {
          "<leader>dr",
          function() call_vscode('workbench.action.debug.run') end,
          desc = "Debug Run"
        },
        -- Open terminal in editor side
        {
          "<leader>'",
          function() call_vscode('workbench.action.createTerminalEditorSide') end,
          desc = "Create Terminal in Editor Side"
        },
        -- Open terminal in editor
        {
          "<leader>\"",
          function() call_vscode('workbench.action.createTerminalEditor') end,
          desc = "Create Terminal in Editor"
        },
        -- Window (tab) Operations
        {
          "<leader>w",
          group = "Window",
        },
        -- Close other tabs
        {
          "<leader>wo",
          function() call_vscode('workbench.action.closeOtherEditors') end,
          desc = "Close Other Editors"
        },
        -- Close all tabs
        {
          "<leader>wa",
          function() call_vscode('workbench.action.closeEditorsInGroup') end,
          desc = "Close All Editors"
        },
        -- Reload Window
        {
          "<leader>wr",
          function() call_vscode('workbench.action.reloadWindow') end,
          desc = "Reload Window"
        },
        -- Git Operations
        {
          "<leader>g",
          group = "Git",
        },
        -- Focus SCM in side bar
        {
          "<leader>gs",
          function() call_vscode('workbench.scm.focus') end,
          desc = "Git Status"
        },
        -- Focus SCM history in side bar
        {
          "<leader>gl",
          function() call_vscode('workbench.scm.history.focus') end,
          desc = "Git Log"
        },
        -- Git checkout (branch operation)
        {
          "<leader>gb",
          function() call_vscode('git.checkout') end,
          desc = "Git Checkout"
        },
        -- Git pull
        {
          "<leader>gf",
          function() call_vscode('git.pull') end,
          desc = "Git Pull"
        },
        -- Git push
        {
          "<leader>gp",
          function() call_vscode('git.push') end,
          desc = "Git Push"
        },
        -- Git stash operations
        {
          "<leader>gz",
          group = "Git Stash",
        },
        -- Git stash all
        {
          "<leader>gzz",
          function() call_vscode('git.stashIncludeUntracked') end,
          desc = "Git Stash All"
        },
        -- Git stash pop latest
        {
          "<leader>gzp",
          function() call_vscode('git.stashPopLatest') end,
          desc = "Git Stash Pop Latest"
        },
        -- Execute the currently open file
        -- NOTE: need `formulahendry.code-runner`
        {
          "<leader>e",
          function() call_vscode('code-runner.run') end,
          desc = "Execute File"
        },
        {
          "<leader>t",
          group = "Toggle"
        },
        -- Toggle Word Wrap
        {
          "<leader>tl",
          function() call_vscode('editor.action.toggleWordWrap') end,
          desc = "Toggle Word Wrap"
        },
        -- Bookmarks operations
        -- NOTE: need `alefragnani.Bookmarks`
        -- Toggle unlabeled bookmark on current line
        {
          "<leader>tb",
          function() call_vscode('bookmarks.toggle') end,
          desc = "Bookmarks Toggle"
        },
        -- Toggle labeled bookmark on current line
        {
          "<leader>tB",
          function() call_vscode('bookmarks.toggleLabeled') end,
          desc = "Bookmarks Toggle Labeled"
        },
        -- Toggle Copilot completion
        {
          "<leader>tc",
          function() call_vscode('github.copilot.completions.toggle') end,
          desc = "Toggle Copilot Completion"
        },
        -- create new tempfile (.md)
        {
          "<leader>tn",
          function() call_vscode('tempfile.newfile') end,
          desc = "Create Tempfile"
        },
        -- create new tempfile with extension
        {
          "<leader>tN",
          function() call_vscode('tempfile.newfile_with_extension') end,
          desc = "Create Tempfile with Extension"
        },
        -- Clojure operations
        -- NOTE: need `betterthantomorrow.calva`
        -- NOTE: need `betterthantomorrow.calva-power-tools`
        {
          "<leader>m",
          group = "Clojure",
        },
        -- REPL operations
        {
          "<leader>mr",
          group = "REPL",
        },
        -- Connect to a running REPL (project)
        {
          "<leader>mrc",
          function() call_vscode('calva.connectNonProjectREPL') end,
          desc = "Connect to a running REPL (non-project)"
        },
        -- Disconnect from a running REPL
        {
          "<leader>mrd",
          function() call_vscode('calva.disconnect') end,
          desc = "Disconnect from a running REPL"
        },
        -- (Re)Start a project REPL and connect (Jack-In)
        {
          "<leader>mri",
          function() call_vscode('calva.jackIn') end,
          desc = "(Re)Start a project REPL and connect (Jack-In)"
        },
        -- Stop/Kill the project REPL started by Calva (Jack-Out)
        {
          "<leader>mrk",
          function() call_vscode('calva.jackOut') end,
          desc = "Stop/Kill the project REPL started by Calva (Jack-Out)"
        },
        -- Show REPL window
        {
          "<leader>mrs",
          function() call_vscode('calva.showReplWindow') end,
          desc = "Show REPL window"
        },
        -- Show REPL output view
        {
          "<leader>mro",
          function() call_vscode('calva.showReplOutputView') end,
          desc = "Show REPL output view"
        },
        -- Refresh all namespaces in REPL
        {
          "<leader>mrR",
          function() call_vscode('calva.refreshAll') end,
          desc = "Refresh all namespaces in REPL"
        },
        -- Refresh current namespace in REPL
        {
          "<leader>mrr",
          function() call_vscode('calva.refresh') end,
          desc = "Refresh current namespace in REPL"
        },
        -- Toggle the REPL Connection (clj/cljs)
        {
          "<leader>mrt",
          function() call_vscode('calva.toggleCLJCSession') end,
          desc = "Toggle the REPL Connection (clj/cljs)"
        },
        -- Load Snitch (injection debugger) in REPL
        {
          "<leader>mrn",
          function()
            local snippet_code =
            "(require '[clojure.repl.deps :refer [add-libs]])\n\n(add-libs '{ org.glassfish.jaxb/jaxb-runtime {:mvn/version \"2.3.2\"}, org.clojars.abhinav/snitch {:mvn/version \"0.1.16\"}})\n\n(require '[snitch.core :refer [defn* defmethod* *fn *let]])"
            call_vscode('calva.runCustomREPLCommand', { args = { snippet = snippet_code } })
          end,
          desc = "Load Snitch in REPL"
        },
        -- Evaluate currently open file
        {
          "<leader>me",
          function() call_vscode('calva.loadFile') end,
          desc = "Evaluate entire file"
        },
        -- Instrument current form for debugging
        {
          "<leader>mg",
          function() call_vscode('calva.debug.instrument') end,
          desc = "Instrument current form for debugging"
        },
        -- (Paredit) Move cursor to list start
        {
          "<leader>mh",
          function() call_vscode('paredit.openList') end,
          desc = "Move cursor to list start"
        },
        -- (Paredit) Move cursor to list end
        {
          "<leader>ml",
          function() call_vscode('paredit.closeList') end,
          desc = "Move cursor to list end"
        },
        -- (Paredit) Barf forwards
        {
          "<leader>mb",
          function() call_vscode('paredit.barfSexpForward') end,
          desc = "Barf forwards"
        },
        -- (Paredit) Barf backwards
        {
          "<leader>mB",
          function() call_vscode('paredit.barfSexpBackward') end,
          desc = "Barf backwards"
        },
        -- (Paredit) Slurp forwards
        {
          "<leader>ms",
          function() call_vscode('paredit.slurpSexpForward') end,
          desc = "Slurp forwards"
        },
        -- (Paredit) Slurp backwards
        {
          "<leader>mS",
          function() call_vscode('paredit.slurpSexpBackward') end,
          desc = "Slurp backwards"
        },
        -- (Paredit) Delete operations
        {
          "<leader>md",
          group = "Delete",
        },
        -- (Paredit) Delete to start of list
        {
          "<leader>mdh",
          function() call_vscode('paredit.killListBackward') end,
          desc = "Delete to start of list"
        },
        -- (Paredit) Delete to end of list
        {
          "<leader>mdl",
          function() call_vscode('paredit.killListForward') end,
          desc = "Delete to end of list"
        },
        -- (Paredit) Delete parent and raise children
        {
          "<leader>mdk",
          function() call_vscode('paredit.raiseSexp') end,
          desc = "Delete parent and raise children"
        },
        -- (Paredit) Wrap operations
        {
          "<leader>mw",
          group = "Wrap",
        },
        -- (Paredit) Wrap with ()
        {
          "<leader>mw(",
          function() call_vscode('paredit.wrapAroundParens') end,
          desc = "Wrap with ()"
        },
        -- (Paredit) Wrap with []
        {
          "<leader>mw[",
          function() call_vscode('paredit.wrapAroundSquare') end,
          desc = "Wrap with []"
        },
        -- (Paredit) Wrap with {}
        {
          "<leader>mw{",
          function() call_vscode('paredit.wrapAroundCurly') end,
          desc = "Wrap with {}"
        },
        -- (Paredit) Wrap with ""
        {
          "<leader>mw\"",
          function() call_vscode('paredit.wrapAroundQuote') end,
          desc = "Wrap with \"\""
        },
        -- (Paredit) Fiddle operations
        {
          "<leader>mf",
          group = "Fiddle",
        },
        -- (Paredit) Open fiddle for current source file
        {
          "<leader>mff",
          function() call_vscode('calva.openFiddleForSourceFile') end,
          desc = "Open fiddle for source file"
        },
        -- (Paredit) Open source file for current fiddle
        {
          "<leader>mfF",
          function() call_vscode('calva.openSourceFileForFiddle') end,
          desc = "Open source file for fiddle"
        },
        -- (Paredit) Evaluate fiddle for current source file
        {
          "<leader>mfe",
          function() call_vscode('calva.evaluateFiddleForSourceFile') end,
          desc = "Evaluate fiddle for source code"
        },
        -- Copy Relative Path
        {
          "<leader>cp",
          function() call_vscode('copyRelativeFilePath') end,
          desc = "Copy Relative Path"
        },
      }
    }
  },
  install = {},
  checker = { enabled = true },
})
