set nocompatible
"automated installation of vimplug if not installed
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
    silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source ~/.config/nvim/init.vim
endif

call plug#begin('~/.config/nvim/plugged')
Plug 'glepnir/dashboard-nvim'
Plug 'lambdalisue/suda.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'williamboman/mason.nvim'
Plug 'williamboman/mason-lspconfig.nvim'
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-vsnip'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/vim-vsnip'
Plug 'simrat39/rust-tools.nvim'
Plug 'kyazdani42/nvim-web-devicons' " If you want devicons
Plug 'glepnir/galaxyline.nvim'
Plug 'notseanray/nerd-galaxyline'
Plug 'sbdchd/neoformat'
Plug 'nanozuki/tabby.nvim'
Plug 'ms-jpq/coq_nvim', {'branch': 'coq'}
Plug 'ms-jpq/coq.artifacts', {'branch': 'artifacts'}
Plug 'kylechui/nvim-surround'
Plug 'machakann/vim-sandwich'
Plug 'lewis6991/gitsigns.nvim'
Plug 'nvim-neo-tree/neo-tree.nvim'
Plug 'ryanoasis/vim-devicons', { 'on': 'Neotree' }
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
Plug 'wakatime/vim-wakatime'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }
Plug 'ntpeters/vim-better-whitespace'
Plug 'kevinhwang91/nvim-hlslens'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'xiyaowong/nvim-transparent'
Plug 'chrisbra/csv.vim'
Plug 'numToStr/Comment.nvim'
Plug 'rcarriga/nvim-notify'
Plug 'tpope/vim-repeat'
Plug 'MunifTanjim/nui.nvim'

Plug 'ggandor/leap.nvim'
Plug 'ggandor/leap-spooky.nvim'
Plug 'ggandor/flit.nvim'

Plug 'notseanray/presence.nvim'
Plug 'windwp/nvim-autopairs'
Plug 'lukas-reineke/indent-blankline.nvim'
Plug 'p00f/clangd_extensions.nvim'
Plug 'norcalli/nvim-colorizer.lua'
Plug 'sindrets/winshift.nvim'
Plug 'kevinhwang91/rnvimr'
Plug 'uga-rosa/ccc.nvim', {'branch': '0.7.2'}
Plug 'folke/which-key.nvim'
Plug 'brenoprata10/nvim-highlight-colors'
" Plug 'phelipetls/jsonpath.nvim'
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
Plug 'jedrzejboczar/possession.nvim'
Plug 'mrjones2014/legendary.nvim'
Plug 'stevearc/dressing.nvim'
Plug 'krivahtoo/silicon.nvim', { 'do': './install.sh' }
Plug 'sindrets/diffview.nvim'

" Plug 'catppuccin/nvim', {'as': 'catppuccin', 'do': 'CatppuccinCompile'}
Plug 'projekt0n/github-nvim-theme'
" Plug 'morhetz/gruvbox'
Plug 'p00f/nvim-ts-rainbow'
Plug 'cbochs/grapple.nvim'
call plug#end()

set mouse=a

" rainbow parens
let g:rainbow_active = 1

" easy search/replace with current visual selection
" xnoremap ;s y:%s/<C-r>"//g<Left><Left>

" easy search/replace on current line with visual selection
" xnoremap ;ls y:.s/<C-r>"//g<Left><Left>

" set tabs to n amount of spaces
function! SetTab(n)
  let &tabstop=a:n
  let &shiftwidth=a:n
  let &softtabstop=a:n
  set expandtab
  set autoindent
  set smartindent
endfunction

command! -nargs=1 SetTab call SetTab(<f-args>)
" Binary files -> xxd
augroup Binary
  au!
  au BufReadPre  *.bin let &bin=1
  au BufReadPost *.bin if &bin | %!xxd
  au BufReadPost *.bin set ft=xxd | endif
  au BufWritePre *.bin if &bin | %!xxd -r
  au BufWritePre *.bin endif
  au BufWritePost *.bin if &bin | %!xxd
  au BufWritePost *.bin set nomod | endif
augroup END

" preserve cursor location on leave
autocmd BufLeave,BufWinLeave * silent! mkview

" restore cursor location on start
autocmd BufReadPost * silent! loadview

" autocmd vimenter * ++nested colorscheme gruvbox

" let g:gruvbox_italic=1
set background=light" Setting dark mode
" let g:gruvbox_contrast_light="hard"
" let g:gruvbox_contrast_dark="medium"
" autocmd vimenter * ++nested colorscheme gruvbox
" colorscheme github_light_default
colorscheme github_dark_default

" let g:gruvbox_contrast_dark = 'light'
" let g:gruvbox_improved_strings = '1'
" let g:gruvbox_improved_warnings = '1'


" this variable must be enabled for colors to be applied properly
set termguicolors

" nvim highlight colors
set t_Co=256

" if exists('+winbar')
    " let s:json_path_ns=luaeval("vim.api.nvim_create_namespace('json_path')")
    "
    " augroup json_path
    "     if &filetype ==# 'json'
    "         setlocal winbar=%{luaeval('require\"jsonpath\".get()')}
    "     endif
    " augroup END
" endif
if &filetype ==# "neo-tree"
    nnoremap <silent> o <ENTER>
endif

lua << EOF
require('dressing').setup({
  input = {
    -- Set to false to disable the vim.ui.input implementation
    enabled = true,

    -- Default prompt string
    default_prompt = "Input:",

    -- Can be 'left', 'right', or 'center'
    prompt_align = "left",

    -- When true, <Esc> will close the modal
    insert_only = true,

    -- When true, input will start in insert mode.
    start_in_insert = true,

    -- These are passed to nvim_open_win
    anchor = "SW",
    border = "rounded",
    -- 'editor' and 'win' will default to being centered
    relative = "cursor",

    -- These can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
    prefer_width = 40,
    width = nil,
    -- min_width and max_width can be a list of mixed types.
    -- min_width = {20, 0.2} means "the greater of 20 columns or 20% of total"
    max_width = { 140, 0.9 },
    min_width = { 20, 0.2 },

    -- Window transparency (0-100)
    winblend = 10,
    -- Change default highlight groups (see :help winhl)
    winhighlight = "",

    -- Set to `false` to disable
    mappings = {
      n = {
        ["<Esc>"] = "Close",
        ["<CR>"] = "Confirm",
      },
      i = {
        ["<C-c>"] = "Close",
        ["<CR>"] = "Confirm",
        ["<Up>"] = "HistoryPrev",
        ["<Down>"] = "HistoryNext",
      },
    },

    override = function(conf)
      -- This is the config that will be passed to nvim_open_win.
      -- Change values here to customize the layout
      return conf
    end,

    -- see :help dressing_get_config
    get_config = nil,
  },
  select = {
    -- Set to false to disable the vim.ui.select implementation
    enabled = true,

    -- Priority list of preferred vim.select implementations
    backend = { "telescope", "fzf_lua", "fzf", "builtin", "nui" },

    -- Trim trailing `:` from prompt
    trim_prompt = true,

    -- Options for telescope selector
    -- These are passed into the telescope picker directly. Can be used like:
    -- telescope = require('telescope.themes').get_ivy({...})
    telescope = nil,

    -- Options for fzf selector
    fzf = {
      window = {
        width = 0.5,
        height = 0.4,
      },
    },

    -- Options for fzf_lua selector
    fzf_lua = {
      winopts = {
        width = 0.5,
        height = 0.4,
      },
    },

    -- Options for nui Menu
    nui = {
      position = "50%",
      size = nil,
      relative = "editor",
      border = {
        style = "rounded",
      },
      buf_options = {
        swapfile = false,
        filetype = "DressingSelect",
      },
      win_options = {
        winblend = 10,
      },
      max_width = 80,
      max_height = 40,
      min_width = 40,
      min_height = 10,
    },

    -- Options for built-in selector
    builtin = {
      -- These are passed to nvim_open_win
      anchor = "NW",
      border = "rounded",
      -- 'editor' and 'win' will default to being centered
      relative = "editor",

      -- Window transparency (0-100)
      winblend = 10,
      -- Change default highlight groups (see :help winhl)
      winhighlight = "",

      -- These can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
      -- the min_ and max_ options can be a list of mixed types.
      -- max_width = {140, 0.8} means "the lesser of 140 columns or 80% of total"
      width = nil,
      max_width = { 140, 0.8 },
      min_width = { 40, 0.2 },
      height = nil,
      max_height = 0.9,
      min_height = { 10, 0.2 },

      -- Set to `false` to disable
      mappings = {
        ["<Esc>"] = "Close",
        ["<C-c>"] = "Close",
        ["<CR>"] = "Confirm",
      },

      override = function(conf)
        -- This is the config that will be passed to nvim_open_win.
        -- Change values here to customize the layout
        return conf
      end,
    },

    -- Used to override format_item. See :help dressing-format
    format_item_override = {},

    -- see :help dressing_get_config
    get_config = nil,
  },
})
require('legendary').setup({
  -- Initial keymaps to bind
  keymaps = {},
  -- Initial commands to bind
  commands = {},
  -- Initial augroups/autocmds to bind
  autocmds = {},
  -- Initial functions to bidn
  functions = {},
  -- Customize the prompt that appears on your vim.ui.select() handler
  -- Can be a string or a function that returns a string.
  select_prompt = ' legendary.nvim ',
  -- Character to use to separate columns in the UI
  col_separator_char = '│',
  -- Optionally pass a custom formatter function. This function
  -- receives the item as a parameter and the mode that legendary
  -- was triggered from (e.g. `function(item, mode): string[]`)
  -- and must return a table of non-nil string values for display.
  -- It must return the same number of values for each item to work correctly.
  -- The values will be used as column values when formatted.
  -- See function `default_format(item)` in
  -- `lua/legendary/ui/format.lua` to see default implementation.
  default_item_formatter = nil,
  -- Include builtins by default, set to false to disable
  include_builtin = true,
  -- Include the commands that legendary.nvim creates itself
  -- in the legend by default, set to false to disable
  include_legendary_cmds = true,
  -- Sort most recently used items to the top of the list
  -- so they can be quickly re-triggered when opening legendary again
  most_recent_items_at_top = true,
  which_key = {
    -- Automatically add which-key tables to legendary
    -- see ./doc/WHICH_KEY.md for more details
    auto_register = false,
    -- you can put which-key.nvim tables here,
    -- or alternatively have them auto-register,
    -- see ./doc/WHICH_KEY.md
    mappings = {},
    opts = {},
    -- controls whether legendary.nvim actually binds they keymaps,
    -- or if you want to let which-key.nvim handle the bindings.
    -- if not passed, true by default
    do_binding = true,
  },
  scratchpad = {
    -- How to open the scratchpad buffer,
    -- 'current' for current window, 'float'
    -- for floating window
    view = 'float',
    -- How to show the results of evaluated Lua code.
    -- 'print' for `print(result)`, 'float' for a floating window.
    results_view = 'float',
    -- Border style for floating windows related to the scratchpad
    float_border = 'rounded',
    -- Whether to restore scratchpad contents from a cache file
    keep_contents = true,
  },
  -- Directory used for caches
  cache_path = string.format('%s/legendary/', vim.fn.stdpath('cache')),
})
EOF

lua << EOF
require("nvim-highlight-colors").setup {
	render = 'background', -- or 'foreground' or 'first_column'
	enable_named_colors = true,
	enable_tailwind = true,
}

require('leap').add_default_mappings()
vim.keymap.del({'x', 'o'}, 'x')
vim.keymap.del({'x', 'o'}, 'X')

require('flit').setup {
  keys = { f = 'f', F = 'F', t = 't', T = 'T' },
  -- A string like "nv", "nvo", "o", etc.
  labeled_modes = "v",
  multiline = true,
  -- Like `leap`s similar argument (call-specific overrides).
  -- E.g.: opts = { equivalence_classes = {} }
  opts = {}
}
EOF

lua << EOF
require('possession').setup {
    commands = {
        save = 'SSave',
        load = 'SLoad',
        delete = 'SDelete',
        list = 'SList',
    }
}
require('leap-spooky').setup {
  affixes = {
    -- These will generate mappings for all native text objects, like:
    -- (ir|ar|iR|aR|im|am|iM|aM){obj}.
    -- Special line objects will also be added, by repeating the affixes.
    -- E.g. `yrr<leap>` and `ymm<leap>` will yank a line in the current
    -- window.
    -- You can also use 'rest' & 'move' as mnemonics.
    remote   = { window = 'r', cross_window = 'R' },
    magnetic = { window = 'm', cross_window = 'M' },
  },
  -- If this option is set to true, the yanked text will automatically be pasted
  -- at the cursor position if the unnamed register is in use (and the object is
  -- "non-magnetic").
  yank_paste = false,
}
EOF

" setlocal spelloptions+=noplainbuffer

filetype plugin on

" Make Ranger replace Netrw and be the file explorer
let g:rnvimr_enable_ex = 1

" Make Ranger to be hidden after picking a file
let g:rnvimr_enable_picker = 1

" Replace `$EDITOR` candidate with this command to open the selected file
let g:rnvimr_edit_cmd = 'drop'

" Disable a border for floating window
let g:rnvimr_draw_border = 0

" Hide the files included in gitignore
let g:rnvimr_hide_gitignore = 1

" Change the border's color
let g:rnvimr_border_attr = {'fg': 14, 'bg': -1}

" Make Neovim wipe the buffers corresponding to the files deleted by Ranger
let g:rnvimr_enable_bw = 1

" Add a shadow window, value is equal to 100 will disable shadow
let g:rnvimr_shadow_winblend = 70

" Draw border with both
let g:rnvimr_ranger_cmd = ['ranger', '--cmd=set draw_borders both']

" Link CursorLine into RnvimrNormal highlight in the Floating window
highlight link RnvimrNormal CursorLine

nnoremap <silent> <M-o> :RnvimrToggle<CR>
tnoremap <silent> <M-o> <C-\><C-n>:RnvimrToggle<CR>

" Resize floating window by all preset layouts
tnoremap <silent> <M-i> <C-\><C-n>:RnvimrResize<CR>

" Resize floating window by special preset layouts
tnoremap <silent> <M-l> <C-\><C-n>:RnvimrResize 1,8,9,11,5<CR>

" Resize floating window by single preset layout
tnoremap <silent> <M-y> <C-\><C-n>:RnvimrResize 6<CR>

" Map Rnvimr action
let g:rnvimr_action = {
            \ '<C-t>': 'NvimEdit tabedit',
            \ '<C-x>': 'NvimEdit split',
            \ '<C-v>': 'NvimEdit vsplit',
            \ 'gw': 'JumpNvimCwd',
            \ 'yw': 'EmitRangerCwd'
            \ }

" Add views for Ranger to adapt the size of floating window
let g:rnvimr_ranger_views = [
            \ {'minwidth': 90, 'ratio': []},
            \ {'minwidth': 50, 'maxwidth': 89, 'ratio': [1,1]},
            \ {'maxwidth': 49, 'ratio': [1]}
            \ ]

" Customize the initial layout
let g:rnvimr_layout = {
            \ 'relative': 'editor',
            \ 'width': float2nr(round(0.7 * &columns)),
            \ 'height': float2nr(round(0.7 * &lines)),
            \ 'col': float2nr(round(0.15 * &columns)),
            \ 'row': float2nr(round(0.15 * &lines)),
            \ 'style': 'minimal'
            \ }

" Customize multiple preset layouts
" '{}' represents the initial layout
let g:rnvimr_presets = [
            \ {'width': 0.600, 'height': 0.600},
            \ {},
            \ {'width': 0.800, 'height': 0.800},
            \ {'width': 0.950, 'height': 0.950},
            \ {'width': 0.500, 'height': 0.500, 'col': 0, 'row': 0},
            \ {'width': 0.500, 'height': 0.500, 'col': 0, 'row': 0.5},
            \ {'width': 0.500, 'height': 0.500, 'col': 0.5, 'row': 0},
            \ {'width': 0.500, 'height': 0.500, 'col': 0.5, 'row': 0.5},
            \ {'width': 0.500, 'height': 1.000, 'col': 0, 'row': 0},
            \ {'width': 0.500, 'height': 1.000, 'col': 0.5, 'row': 0},
            \ {'width': 1.000, 'height': 0.500, 'col': 0, 'row': 0},
            \ {'width': 1.000, 'height': 0.500, 'col': 0, 'row': 0.5}
            \ ]

" prettier for formatting on save
let g:neoformat_try_node_exe = 1

" always show tab line
set showtabline=1

" open default file if buffer name is empty
" au VimEnter * if eval("@%") == "" | e ~/Desktop/common.md | endif

" colorscheme one
" colorscheme one
" set background=dark

lua << EOF
local theme = {
  -- The following keys are all optional
  -- with default values
  font = 'Hack=20',
  theme = 'Dracula',
  background = '#eff',
  shadow = {
    blur_radius = 0.0,
    offset_x = 0,
    offset_y = 0,
    color = '#555'
  },
  pad_horiz = 100,
  pad_vert = 80,
  line_number = false,
  line_pad = 2,
  line_offset = 1,
  tab_width = 4,
  round_corner = true,
  window_controls = true,
}
require('silicon').setup(theme)
EOF

lua << EOF
require("notify").setup({
    relative = "editor",
    anchor = "NE",
    width = 50,
    height = 10,
    background_colour = "#c9d2e4",
    render = "minimal",
})
-- local noice_config = {
--   cmdline = {
--     enabled = true, -- enables the Noice cmdline UI
--     view = "cmdline_popup", -- view for rendering the cmdline. Change to `cmdline` to get a classic cmdline at the bottom
--     opts = { buf_options = { filetype = "vim" } }, -- enable syntax highlighting in the cmdline
--     ---@type table<string, CmdlineFormat>
--     format = {
--       -- conceal: (default=true) This will hide the text in the cmdline that matches the pattern.
--       -- view: (default is cmdline view)
--       -- opts: any options passed to the view
--       -- icon_hl_group: optional hl_group for the icon
--       cmdline = { pattern = "^:", icon = "λ" },
--       search = { pattern = "^[?/]", icon = " ", conceal = false },
--       filter = { pattern = "^:%s*!", icon = "$", opts = { buf_options = { filetype = "sh" } } },
--       lua = { pattern = "^:%s*lua%s+", icon = "", opts = { buf_options = { filetype = "lua" } } },
--       -- lua = false, -- to disable a format, set to `false`
--     },
--   },
--   messages = {
--     -- NOTE: If you enable messages, then the cmdline is enabled automatically.
--     -- This is a current Neovim limitation.
--     enabled = true, -- enables the Noice messages UI
--     view = "notify", -- default view for messages
--     view_error = "notify", -- view for errors
--     view_warn = "notify", -- view for warnings
--     view_history = "split", -- view for :messages
--     view_search = "virtualtext", -- view for search count messages. Set to `false` to disable
--   },
--   popupmenu = {
--     enabled = true, -- enables the Noice popupmenu UI
--     ---@type 'nui'|'cmp'
--     backend = "nui", -- backend to use to show regular cmdline completions
--   },
--   ---@type NoiceRouteConfig
--   history = {
--     -- options for the message history that you get with `:Noice`
--     view = "split",
--     opts = { enter = true, format = "details" },
--     filter = { event = { "msg_show", "notify" }, ["not"] = { kind = { "search_count", "echo" } } },
--   },
--   notify = {
--     -- Noice can be used as `vim.notify` so you can route any notification like other messages
--     -- Notification messages have their level and other properties set.
--     -- event is always "notify" and kind can be any log level as a string
--     -- The default routes will forward notifications to nvim-notify
--     -- Benefit of using Noice for this is the routing and consistent history view
--     enabled = true,
--     view = "notify",
--   },
--   lsp_progress = {
--     enabled = false,
--     -- Lsp Progress is formatted using the builtins for lsp_progress. See config.format.builtin
--     -- See the section on formatting for more details on how to customize.
--     --- @type NoiceFormat|string
--     format = "lsp_progress",
--     --- @type NoiceFormat|string
--     format_done = "lsp_progress_done",
--     throttle = 1000 / 30, -- frequency to update lsp progress message
--     view = "mini",
--   },
--   throttle = 1000 / 30, -- how frequently does Noice need to check for ui updates? This has no effect when in blocking mode.
--   ---@type NoiceConfigViews
--   views = {
--           cmdline_popup = {
--             position = {
--               row = 2,
--               col = "50%",
--             },
--           },
--           popupmenu = {
--             relative = "editor",
--             position = {
--               row = 5,
--               col = "50%",
--             },
--             size = {
--               width = 60,
--               height = 10,
--             },
--             border = {
--               style = "rounded",
--               padding = { 0, 1 },
--             },
--             win_options = {
--               winhighlight = { Normal = "Normal", FloatBorder = "DiagnosticInfo" },
--             },
--           },
--       }, ---@see section on views
--   ---@type NoiceRouteConfig[]
--   routes = {}, --- @see section on routes
--   ---@type table<string, NoiceFilter>
--   status = {}, --- @see section on statusline components
--   ---@type NoiceFormatOptions
--   format = {}, --- @see section on formatting
-- }
--
-- require("noice").setup(noice_config)
EOF

lua << EOF
  require("which-key").setup {
      plugins = {
    marks = true, -- shows a list of your marks on ' and `
    registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
    spelling = {
      enabled = false, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
      suggestions = 20, -- how many suggestions should be shown in the list?
    },
    -- the presets plugin, adds help for a bunch of default keybindings in Neovim
    -- No actual key bindings are created
    presets = {
      operators = true, -- adds help for operators like d, y, ... and registers them for motion / text object completion
      motions = true, -- adds help for motions
      text_objects = true, -- help for text objects triggered after entering an operator
      windows = true, -- default bindings on <c-w>
      nav = true, -- misc bindings to work with windows
      z = true, -- bindings for folds, spelling and others prefixed with z
      g = true, -- bindings for prefixed with g
    },
  },
  -- add operators that will trigger motion and text object completion
  -- to enable all native operators, set the preset / operators plugin above
  operators = { gc = "Comments" },
  key_labels = {
    -- override the label used to display some keys. It doesn't effect WK in any other way.
    -- For example:
    -- ["<space>"] = "SPC",
    -- ["<cr>"] = "RET",
    -- ["<tab>"] = "TAB",
  },
  icons = {
    breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
    separator = "➜", -- symbol used between a key and it's label
    group = "+", -- symbol prepended to a group
  },
  popup_mappings = {
    scroll_down = '<c-d>', -- binding to scroll down inside the popup
    scroll_up = '<c-u>', -- binding to scroll up inside the popup
  },
  window = {
    border = "none", -- none, single, double, shadow
    position = "top", -- bottom, top
    margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
    padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
    winblend = 20
  },
  layout = {
    height = { min = 4, max = 25 }, -- min and max height of the columns
    width = { min = 20, max = 50 }, -- min and max width of the columns
    spacing = 3, -- spacing between columns
    align = "left", -- align columns left, center or right
  },
  ignore_missing = false, -- enable this to hide mappings for which you didn't specify a label
  hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ "}, -- hide mapping boilerplate
  show_help = true, -- show help message on the command line when the popup is visible
  triggers = "auto", -- automatically setup triggers
  -- triggers = {"<leader>"} -- or specify a list manually
  triggers_blacklist = {
    -- list of mode / prefixes that should never be hooked by WhichKey
    -- this is mostly relevant for key maps that start with a native binding
    -- most people should not need to change this
    i = { "j", "k" },
    v = { "j", "k" },
  },
  -- disable the WhichKey popup for certain buf types and file types.
  -- Disabled by deafult for Telescope
  disable = {
    buftypes = {},
    filetypes = { "TelescopePrompt" },
  },
  }
EOF

lua << EOF
-- local augend = require("dial.augend")
-- require("dial.config").augends:register_group{
--   -- default augends used when no group name is specified
--   default = {
--     augend.integer.alias.decimal,   -- nonnegative decimal number (0, 1, 2, 3, ...)
--     augend.integer.alias.hex,       -- nonnegative hex number  (0x01, 0x1a1f, etc.)
--     augend.date.alias["%Y/%m/%d"],  -- date (2022/02/19, etc.)
--   },
--
--   -- augends used when group with name `mygroup` is specified
--   mygroup = {
--     augend.integer.alias.decimal,
--     augend.constant.alias.bool,    -- boolean value (true <-> false)
--     augend.date.alias["%m/%d/%Y"], -- date (02/19/2022, etc.)
--   }
-- }

local ccc = require("ccc")
local mapping = ccc.mapping
ccc.setup({})

--  require('spellsitter').setup {
--    -- Whether enabled, can be a list of filetypes, e.g. {'python', 'lua'}
--    enable = true,
--    debug = false
--  }
local my_augroup = vim.api.nvim_create_augroup("my_augroup", { clear = true })

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "json" }, -- disable spellchecking for these filetypes
  command = "setlocal nospell",
  group = my_augroup,
})
vim.api.nvim_create_autocmd("TermOpen", {
  pattern = "*", -- disable spellchecking in the embeded terminal
  command = "setlocal nospell",
  group = my_augroup,
})
EOF

" set spell
syntax on

" increment decrement etc
nmap  <C-a>  <Plug>(dial-increment)
nmap  <C-x>  <Plug>(dial-decrement)
vmap  <C-a>  <Plug>(dial-increment)
vmap  <C-x>  <Plug>(dial-decrement)
vmap g<C-a> g<Plug>(dial-increment)
vmap g<C-x> g<Plug>(dial-decrement)

lua << EOF
require("winshift").setup({
  highlight_moving_win = true,  -- Highlight the window being moved
  focused_hl_group = "Visual",  -- The highlight group used for the moving window
  moving_win_options = {
    -- These are local options applied to the moving window while it's
    -- being moved. They are unset when you leave Win-Move mode.
    wrap = false,
    cursorline = false,
    cursorcolumn = false,
    colorcolumn = "",
  },
  keymaps = {
    disable_defaults = false, -- Disable the default keymaps
    win_move_mode = {
      ["h"] = "left",
      ["j"] = "down",
      ["k"] = "up",
      ["l"] = "right",
      ["H"] = "far_left",
      ["J"] = "far_down",
      ["K"] = "far_up",
      ["L"] = "far_right",
      ["<left>"] = "left",
      ["<down>"] = "down",
      ["<up>"] = "up",
      ["<right>"] = "right",
      ["<S-left>"] = "far_left",
      ["<S-down>"] = "far_down",
      ["<S-up>"] = "far_up",
      ["<S-right>"] = "far_right",
    },
  },
  ---A function that should prompt the user to select a window.
  ---
  ---The window picker is used to select a window while swapping windows with
  ---`:WinShift swap`.
  ---@return integer? winid # Either the selected window ID, or `nil` to
  ---   indicate that the user cancelled / gave an invalid selection.
  window_picker = function()
    return require("winshift.lib").pick_window({
      -- A string of chars used as identifiers by the window picker.
      picker_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890",
      filter_rules = {
        -- This table allows you to indicate to the window picker that a window
        -- should be ignored if its buffer matches any of the following criteria.
        cur_win = true, -- Filter out the current window
        floats = true,  -- Filter out floating windows
        filetype = {},  -- List of ignored file types
        buftype = {},   -- List of ignored buftypes
        bufname = {},   -- List of vim regex patterns matching ignored buffer names
      },
      ---A function used to filter the list of selectable windows.
      ---@param winids integer[] # The list of selectable window IDs.
      ---@return integer[] filtered # The filtered list of window IDs.
      filter_func = nil,
    })
  end,
})
EOF

" Start Win-Move mode:
nnoremap <C-W><C-M> <Cmd>WinShift<CR>
nnoremap <C-W>m <Cmd>WinShift<CR>

" Swap two windows:
nnoremap <C-W>x <Cmd>WinShift swap<CR>

" If you don't want to use Win-Move mode you can create mappings for calling the
" move commands directly:
nnoremap <C-M-H> <Cmd>WinShift left<CR>
nnoremap <C-M-J> <Cmd>WinShift down<CR>
nnoremap <C-M-K> <Cmd>WinShift up<CR>
nnoremap <C-M-L> <Cmd>WinShift right<CR>

lua << EOF
local DEFAULT_SETTINGS = {
    ui = {
        -- Whether to automatically check for new versions when opening the :Mason window.
        check_outdated_packages_on_open = true,

        -- The border to use for the UI window. Accepts same border values as |nvim_open_win()|.
        border = "none",

         icons = {
            package_installed = "✓",
            package_pending = "➜",
            package_uninstalled = "✗"
        },

        keymaps = {
            -- Keymap to expand a package
            toggle_package_expand = "<CR>",
            -- Keymap to install the package under the current cursor position
            install_package = "i",
            -- Keymap to reinstall/update the package under the current cursor position
            update_package = "u",
            -- Keymap to check for new version for the package under the current cursor position
            check_package_version = "c",
            -- Keymap to update all installed packages
            update_all_packages = "U",
            -- Keymap to check which installed packages are outdated
            check_outdated_packages = "C",
            -- Keymap to uninstall a package
            uninstall_package = "X",
            -- Keymap to cancel a package installation
            cancel_installation = "<C-c>",
            -- Keymap to apply language filter
            apply_language_filter = "<C-f>",
        },
    },

    -- The directory in which to install packages.
    -- install_root_dir = path.concat { vim.fn.stdpath "data", "mason" },

    pip = {
        -- These args will be added to `pip install` calls. Note that setting extra args might impact intended behavior
        -- and is not recommended.
        --
        -- Example: { "--proxy", "https://proxyserver" }
        install_args = {},
    },

    -- Controls to which degree logs are written to the log file. It's useful to set this to vim.log.levels.DEBUG when
    -- debugging issues with package installations.
    log_level = vim.log.levels.INFO,

    -- Limit for the maximum amount of packages to be installed at the same time. Once this limit is reached, any further
    -- packages that are requested to be installed will be put in a queue.
    max_concurrent_installers = 4,

    github = {
        -- The template URL to use when downloading assets from GitHub.
        -- The placeholders are the following (in order):
        -- 1. The repository (e.g. "rust-lang/rust-analyzer")
        -- 2. The release version (e.g. "v0.3.0")
        -- 3. The asset name (e.g. "rust-analyzer-v0.3.0-x86_64-unknown-linux-gnu.tar.gz")
        download_url_template = "https://github.com/%s/releases/download/%s/%s",
    },
}

require("mason").setup({
    ui = {
        icons = {
            package_installed = "✓",
            package_pending = "➜",
            package_uninstalled = "✗"
        }
    }
})
local MASON_DEFAULT = {
    -- A list of servers to automatically install if they're not already installed. Example: { "rust_analyzer@nightly", "sumneko_lua" }
    -- This setting has no relation with the `automatic_installation` setting.
    ensure_installed = { "rust_analyzer", "pyright", "jdtls", "tsserver", "gopls", "volar", "tailwindcss", "clangd" },

    -- Whether servers that are set up (via lspconfig) should be automatically installed if they're not already installed.
    -- This setting has no relation with the `ensure_installed` setting.
    -- Can either be:
    --   - false: Servers are not automatically installed.
    --   - true: All servers set up via lspconfig are automatically installed.
    --   - { exclude: string[] }: All servers set up via lspconfig, except the ones provided in the list, are automatically installed.
    --       Example: automatic_installation = { exclude = { "rust_analyzer", "solargraph" } }
    automatic_installation = true,
}
require("mason-lspconfig").setup(MASON_DEFAULT)
EOF

lua <<EOF
  local home = os.getenv('HOME')
  local db = require('dashboard')
  db.custom_header = {
    \'',
    \'   ⣴⣶⣤⡤⠦⣤⣀⣤⠆     ⣈⣭⣭⣿⣶⣿⣦⣼⣆         ',
    \'    ⠉⠻⢿⣿⠿⣿⣿⣶⣦⠤⠄⡠⢾⣿⣿⡿⠋⠉⠉⠻⣿⣿⡛⣦       ',
    \'          ⠈⢿⣿⣟⠦ ⣾⣿⣿⣷⠄⠄⠄⠄⠻⠿⢿⣿⣧⣄     ',
    \'           ⣸⣿⣿⢧ ⢻⠻⣿⣿⣷⣄⣀⠄⠢⣀⡀⠈⠙⠿⠄    ',
    \'          ⢠⣿⣿⣿⠈  ⠡⠌⣻⣿⣿⣿⣿⣿⣿⣿⣛⣳⣤⣀⣀   ',
    \'   ⢠⣧⣶⣥⡤⢄ ⣸⣿⣿⠘⠄ ⢀⣴⣿⣿⡿⠛⣿⣿⣧⠈⢿⠿⠟⠛⠻⠿⠄  ',
    \'  ⣰⣿⣿⠛⠻⣿⣿⡦⢹⣿⣷   ⢊⣿⣿⡏  ⢸⣿⣿⡇ ⢀⣠⣄⣾⠄   ',
    \' ⣠⣿⠿⠛⠄⢀⣿⣿⣷⠘⢿⣿⣦⡀ ⢸⢿⣿⣿⣄ ⣸⣿⣿⡇⣪⣿⡿⠿⣿⣷⡄  ',
    \' ⠙⠃   ⣼⣿⡟  ⠈⠻⣿⣿⣦⣌⡇⠻⣿⣿⣷⣿⣿⣿ ⣿⣿⡇⠄⠛⠻⢷⣄ ',
    \'      ⢻⣿⣿⣄   ⠈⠻⣿⣿⣿⣷⣿⣿⣿⣿⣿⡟ ⠫⢿⣿⡆     ',
    \'       ⠻⣿⣿⣿⣿⣶⣶⣾⣿⣿⣿⣿⣿⣿⣿⣿⡟⢀⣀⣤⣾⡿⠃     ',
    \'                                   ',
    \'               NEOVIM              ',
    \
  }
  db.custom_footer = { "NVIM > VSCode" }
  db.custom_center = {
      {icon = '  ',
      desc = 'Recently latest session                ',
      shortcut = 'SPC s l',
      action ='SessionLoad'},
      {icon = '  ',
      desc = 'Recently opened files                   ',
      action =  'DashboardFindHistory',
      shortcut = 'SPC f h'},
      {icon = '  ',
      desc = 'Find  File                              ',
      action = 'Telescope find_files find_command=rg,--hidden,--files',
      shortcut = 'SPC f f'},
      {icon = '  ',
      desc ='File Browser                            ',
      action =  'Telescope file_browser',
      shortcut = 'SPC f b'},
      {icon = '  ',
      desc = 'Find  word                              ',
      action = 'Telescope live_grep',
      shortcut = 'SPC f w'},
      {icon = '  ',
      desc = 'Open Personal dotfiles                  ',
      action = 'Telescope dotfiles path=' .. home ..'/Desktop/stuff/dotfiles',
      shortcut = 'SPC f d'},
    }

EOF
" For 'Yggdroot/indentLine' and 'lukas-reineke/indent-blankline.nvim' "
let g:indentLine_fileTypeExclude = ['dashboard']
" For 'ntpeters/vim-better-whitespace' "
let g:better_whitespace_filetypes_blacklist = ['dashboard', 'terminal', 'neo-tree', 'md', 'diff', 'git', 'gitcommit', 'unite', 'qf', 'help', 'markdown', 'fugitive']
let g:strip_whitespace_confirm=0
let g:current_line_whitespace_disabled_hard=1
let g:current_line_whitespace_disabled_soft=1


lua << EOF
require 'colorizer'.setup {
  'scss';
  'html';
  'vue';
  'svelte';
  'tsx';
  'jsx';
  'css';
  'javascript';
  html = {
    mode = 'foreground';
  }
}
EOF


lua << EOF
require('gitsigns').setup {
  signs = {
    add          = {hl = 'GitSignsAdd'   , text = '│', numhl='GitSignsAddNr'   , linehl='GitSignsAddLn'},
    change       = {hl = 'GitSignsChange', text = '│', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
    delete       = {hl = 'GitSignsDelete', text = '_', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    topdelete    = {hl = 'GitSignsDelete', text = '‾', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    changedelete = {hl = 'GitSignsChange', text = '~', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
  },
  signcolumn = true,  -- Toggle with `:Gitsigns toggle_signs`
  numhl      = false, -- Toggle with `:Gitsigns toggle_numhl`
  linehl     = false, -- Toggle with `:Gitsigns toggle_linehl`
  word_diff  = false, -- Toggle with `:Gitsigns toggle_word_diff`
  watch_gitdir = {
    interval = 2000,
    follow_files = false
  },
  attach_to_untracked = true,
  current_line_blame = false, -- Toggle with `:Gitsigns toggle_current_line_blame`
  current_line_blame_opts = {
    virt_text = true,
    virt_text_pos = 'eol', -- 'eol' | 'overlay' | 'right_align'
    delay = 1000,
    ignore_whitespace = false,
  },
  current_line_blame_formatter = '<author>, <author_time:%Y-%m-%d> - <summary>',
  sign_priority = 6,
  update_debounce = 1000,
  status_formatter = nil, -- Use default
  max_file_length = 40000, -- Disable if file is longer than this (in lines)
  preview_config = {
    -- Options passed to nvim_open_win
    border = 'rounded',
    style = '',
    relative = 'cursor',
    row = 0,
    col = 1
  },
  yadm = {
    enable = false
  },
}
EOF

" lua << EOF
" -- venn.nvim: enable or disable keymappings
" function _G.Toggle_venn()
"     local venn_enabled = vim.inspect(vim.b.venn_enabled)
"     if venn_enabled == "nil" then
"         vim.b.venn_enabled = true
"         vim.cmd[[setlocal ve=all]]
"         -- draw a line on HJKL keystokes
"         vim.api.nvim_buf_set_keymap(0, "n", "J", "<C-v>j:VBox<CR>", {noremap = true})
"         vim.api.nvim_buf_set_keymap(0, "n", "K", "<C-v>k:VBox<CR>", {noremap = true})
"         vim.api.nvim_buf_set_keymap(0, "n", "L", "<C-v>l:VBox<CR>", {noremap = true})
"         vim.api.nvim_buf_set_keymap(0, "n", "H", "<C-v>h:VBox<CR>", {noremap = true})
"         -- draw a box by pressing "f" with visual selection
"         vim.api.nvim_buf_set_keymap(0, "v", "f", ":VBox<CR>", {noremap = true})
"     else
"         vim.cmd[[setlocal ve=]]
"         vim.cmd[[mapclear <buffer>]]
"         vim.b.venn_enabled = nil
"     end
" end
" -- toggle keymappings for venn using <leader>v
" vim.api.nvim_set_keymap('n', '<leader>v', ":lua Toggle_venn()<CR>", { noremap = true})
" EOF

let g:coq_settings = ({'display.icons.mode': 'none', 'keymap.jump_to_mark': '<C-i>', 'auto_start': v:false})

lua require('Comment').setup()

" Function to trim extra whitespace in whole file
function! Trim()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfun

command! -nargs=0 Trim call Trim()
autocmd BufWritePost * call Trim()
autocmd BufWritePost * StripWhitespace

" auto + smart indent for code
" set autoindent
" set smartindent

set visualbell
set wrap

set shortmess+=c
set wildoptions+=pum
set completeopt=menuone,noinsert,noselect


" Compile Catppuccin
" autocmd BufWritePost init.vim :CatppuccinCompile

lua << EOF
require("transparent").setup({
  enable = true, -- boolean: enable transparent
})
vim.opt.list = true
vim.opt.listchars:append "space:⋅"

require("indent_blankline").setup {
    space_char_blankline = "",
}
EOF
"
" lua << EOF
"     vim.g.catppuccin_flavour = "mocha" -- latte, frappe, macchiato, mocha
"     require("catppuccin").setup({
"         transparent_background = false,
"         term_colors = false,
"         compile = {
"             enabled = true,
"             path = vim.fn.stdpath "cache" .. "/catppuccin",
"         },
"         styles = {
"             comments = { "italic" },
"             conditionals = { "italic" },
"             loops = { "italic" },
"             functions = { "italic" },
"             keywords = { "italic" },
"             strings = { "italic" },
"             variables = { "italic" },
"             numbers = { "italic" },
"             booleans = { "italic" },
"             properties = { "italic" },
"             types = { "italic" },
"             operators = { "italic" },
"         },
"         integrations = {
"             treesitter = true,
"             native_lsp = {
"                 enabled = true,
"                 virtual_text = {
"                     errors = { "italic" },
"                     hints = { "italic" },
"                     warnings = { "italic" },
"                     information = { "italic" },
"                 },
"                 underlines = {
"                     errors = { "underline" },
"                     hints = { "underline" },
"                     warnings = { "underline" },
"                     information = { "underline" },
"                 },
"             },
"             coc_nvim = false,
"             lsp_trouble = false,
"             cmp = true,
"             lsp_saga = false,
"             gitgutter = true,
"             gitsigns = true,
"             leap = false,
"             telescope = true,
"             nvimtree = {
"                 enabled = true,
"                 show_root = true,
"                 transparent_panel = false,
"             },
"             neotree = {
"                 enabled = false,
"                 show_root = true,
"                 transparent_panel = false,
"             },
"             dap = {
"                 enabled = false,
"                 enable_ui = false,
"             },
"             which_key = false,
"             indent_blankline = {
"                 enabled = true,
"                 colored_indent_levels = true,
"             },
"             dashboard = true,
"             neogit = false,
"             vim_sneak = false,
"             fern = false,
"             barbar = false,
"             bufferline = true,
"             markdown = true,
"             lightspeed = false,
"             ts_rainbow = false,
"             hop = false,
"             notify = true,
"             telekasten = true,
"             symbols_outline = true,
"         }
"     })
"     vim.cmd[[colorscheme catppuccin]]
" EOF


" Catppuccin
" let g:catppuccin_flavour = "mocha" " latte, frappe, macchiato, mocha
" colorscheme catppuccin

lua << EOF
require('nvim-autopairs').setup({
  enable_check_bracket_line = false
})
require'nvim-treesitter.configs'.setup {
    rainbow = {
        enable = true,
        -- disable = { "jsx", "cpp" }, list of languages you want to disable the plugin for
        extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
        max_file_lines = nil, -- Do not enable for files with more than n lines, int
        -- colors = {}, -- table of hex strings
        -- termcolors = {} -- table of colour name strings
      },
  -- A list of parser names, or "all"
  ensure_installed = { "rust", "lua", "c" },
  ignore_install = { "regex" },

  -- Install parsers synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- Automatically install missing parsers when entering buffer
  auto_install = true,

  highlight = {
    -- `false` will disable the whole extension
    enable = true,

    -- NOTE: these are the names of the parsers and not the filetype. (for example if you want to
    -- disable highlighting for the `tex` filetype, you need to include `latex` in this list as this is
    -- the name of the parser)
    -- list of language that will be disabled
    -- disable = { "c" },

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
}
EOF

lua << EOF
require("hlslens").setup()
-- require("scrollbar").setup({
--     show = true,
--     show_in_active_only = false,
--     set_highlights = true,
--     folds = 500, -- handle folds, set to number to disable folds if no. of lines in buffer exceeds this
--     max_lines = false, -- disables if no. of lines in buffer exceeds this
--     handle = {
--         text = " ",
--         color = nil,
--         cterm = nil,
--         highlight = "CursorColumn",
--         hide_if_all_visible = true, -- Hides handle if all lines are visible
--     },
--     marks = {
--         Search = {
--             text = { "-", "=" },
--             priority = 0,
--             color = nil,
--             cterm = nil,
--             highlight = "Search",
--         },
--         Error = {
--             text = { "-", "=" },
--             priority = 1,
--             color = nil,
--             cterm = nil,
--             highlight = "DiagnosticVirtualTextError",
--         },
--         Warn = {
--             text = { "-", "=" },
--             priority = 2,
--             color = nil,
--             cterm = nil,
--             highlight = "DiagnosticVirtualTextWarn",
--         },
--         Info = {
--             text = { "-", "=" },
--             priority = 3,
--             color = nil,
--             cterm = nil,
--             highlight = "DiagnosticVirtualTextInfo",
--         },
--         Hint = {
--             text = { "-", "=" },
--             priority = 4,
--             color = nil,
--             cterm = nil,
--             highlight = "DiagnosticVirtualTextHint",
--         },
--         Misc = {
--             text = { "-", "=" },
--             priority = 5,
--             color = nil,
--             cterm = nil,
--             highlight = "Normal",
--         },
--     },
--     excluded_buftypes = {
--         "terminal",
--     },
--     excluded_filetypes = {
--         "prompt",
--         "TelescopePrompt",
--     },
--     autocmd = {
--         render = {
--             "BufWinEnter",
--             "TabEnter",
--             "TermEnter",
--             "WinEnter",
--             "CmdwinLeave",
--             "TextChanged",
--             "VimResized",
--             "WinScrolled",
--         },
--         clear = {
--             "BufWinLeave",
--             "TabLeave",
--             "TermLeave",
--             "WinLeave",
--         },
--     },
--     handlers = {
--         diagnostic = true,
--         search = true, -- Requires hlslens to be loaded, will run require("scrollbar.handlers.search").setup() for you
--     },
-- })
EOF

lua << EOF
require("clangd_extensions").setup {
    server = {
        -- options to pass to nvim-lspconfig
        -- i.e. the arguments to require("lspconfig").clangd.setup({})
    },
    extensions = {
        -- defaults:
        -- Automatically set inlay hints (type hints)
        autoSetHints = true,
        -- These apply to the default ClangdSetInlayHints command
        inlay_hints = {
            -- Only show inlay hints for the current line
            only_current_line = false,
            -- Event which triggers a refersh of the inlay hints.
            -- You can make this "CursorMoved" or "CursorMoved,CursorMovedI" but
            -- not that this may cause  higher CPU usage.
            -- This option is only respected when only_current_line and
            -- autoSetHints both are true.
            only_current_line_autocmd = "CursorHold",
            -- whether to show parameter hints with the inlay hints or not
            show_parameter_hints = true,
            -- prefix for parameter hints
            parameter_hints_prefix = "<- ",
            -- prefix for all the other hints (type, chaining)
            other_hints_prefix = "=> ",
            -- whether to align to the length of the longest line in the file
            max_len_align = false,
            -- padding from the left if max_len_align is true
            max_len_align_padding = 1,
            -- whether to align to the extreme right or not
            right_align = false,
            -- padding from the right if right_align is true
            right_align_padding = 7,
            -- The color of the hints
            highlight = "Comment",
            -- The highlight group priority for extmark
            priority = 100,
        },
        ast = {
            role_icons = {
                type = "",
                declaration = "",
                expression = "",
                specifier = "",
                statement = "",
                ["template argument"] = "",
            },

            kind_icons = {
                Compound = "",
                Recovery = "",
                TranslationUnit = "",
                PackExpansion = "",
                TemplateTypeParm = "",
                TemplateTemplateParm = "",
                TemplateParamObject = "",
            },

            highlights = {
                detail = "Comment",
            },
        },
        memory_usage = {
            border = "none",
        },
        symbol_info = {
            border = "none",
        },
    },
}
vim.fn.sign_define(
    "DiagnosticSignError",
    { text = " ", texthl = "DiagnosticSignError", linehl = "DiagnosticLineError", numhl = "" }
)
vim.fn.sign_define(
    "DiagnosticSignWarn",
    { text = " ", texthl = "DiagnosticSignWarn", linehl = "DiagnosticLineWarn", numhl = "" }
)
vim.fn.sign_define(
    "DiagnosticSignWarning",
    { text = " ", texthl = "DiagnosticSignWarning", linehl = "DiagnosticLineWarning", numhl = "" }
)
vim.fn.sign_define(
    "DiagnosticSignInformation",
    { text = " ", texthl = "DiagnosticSignInformation", linehl = "DiagnosticLineInformation", numhl = "" }
)
vim.fn.sign_define(
    "DiagnosticSignInfo",
    { text = " ", texthl = "DiagnosticSignInfo", linehl = "DiagnosticLineInfo", numhl = "" }
)
vim.fn.sign_define(
    "DiagnosticSignHing",
    { text = " ", texthl = "DiagnosticSignHint", linehl = "DiagnosticLineHint", numhl = "" }
)
EOF

lua << EOF
-- default settings, but here anyway to allow easy swapping
require('telescope').setup {
  extensions = {
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    }
  }
}

-- To get fzf loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
require('telescope').load_extension('fzf')
require('telescope').load_extension('possession')

local nvim_lsp = require'lspconfig'

local kopts = { noremap=true, silent=true }
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, kopts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, kopts)

local opts = {
    tools = { -- rust-tools options
        autoSetHints = true,
        --hover_with_actions = true,
        inlay_hints = {
            show_parameter_hints = true,
            parameter_hints_prefix = "",
            other_hints_prefix = "",
        },
    },

    -- all the opts to send to nvim-lspconfig
    -- these override the defaults set by rust-tools.nvim
    -- see https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#rust_analyzer
    server = {
        -- on_attach is a callback called when the language server attachs to the buffer
        -- on_attach = on_attach,
        settings = {
            -- to enable rust-analyzer settings visit:
            -- https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/user/generated_config.adoc
            ["rust-analyzer"] = {
                -- enable clippy on save
                checkOnSave = {
                    command = "clippy"
                },
                inlay_hints = {
                    maxLength = 20,
                    closureReturnTypeHints = true
                }
            }
        }
    },
    -- options same as lsp hover / vim.lsp.util.open_floating_preview()
    hover_actions = {
        -- the border that is used for the hover window
        -- see vim.api.nvim_open_win()
        border = {
            { "╭", "FloatBorder" },
            { "─", "FloatBorder" },
            { "╮", "FloatBorder" },
            { "│", "FloatBorder" },
            { "╯", "FloatBorder" },
            { "─", "FloatBorder" },
            { "╰", "FloatBorder" },
            { "│", "FloatBorder" },
        },

        -- whether the hover action window gets automatically focused
        -- default: false
        auto_focus = true,
    },
}

-- require("document-color").setup {
--     -- Default options
--     mode = "background", -- "background" | "foreground" | "single"
-- }
--
-- local on_attach = function(client)
--   if client.server_capabilities.colorProvider then
--     -- Attach document colour support
--     require("document-color").buf_attach(bufnr)
--   end
-- end

local capabilities = vim.lsp.protocol.make_client_capabilities()

-- BEING UFO
-- -- nvim ufo folding
-- capabilities.textDocument.foldingRange = {
--     dynamicRegistration = false,
--     lineFoldingOnly = true
-- }
--
-- -- You are now capable!
-- capabilities.textDocument.colorProvider = {
--   dynamicRegistration = true
-- }

-- Lsp servers that support documentColor
require("lspconfig").tailwindcss.setup({
  on_attach = on_attach,
  capabilities = capabilities
})

-- local ftMap = {
--     vim = 'indent',
--     python = {'indent'},
--     git = ''
-- }
--
-- local handler = function(virtText, lnum, endLnum, width, truncate)
--     local newVirtText = {}
--     local suffix = ('  %d '):format(endLnum - lnum)
--     local sufWidth = vim.fn.strdisplaywidth(suffix)
--     local targetWidth = width - sufWidth
--     local curWidth = 0
--     for _, chunk in ipairs(virtText) do
--         local chunkText = chunk[1]
--         local chunkWidth = vim.fn.strdisplaywidth(chunkText)
--         if targetWidth > curWidth + chunkWidth then
--             table.insert(newVirtText, chunk)
--         else
--             chunkText = truncate(chunkText, targetWidth - curWidth)
--             local hlGroup = chunk[2]
--             table.insert(newVirtText, {chunkText, hlGroup})
--             chunkWidth = vim.fn.strdisplaywidth(chunkText)
--             -- str width returned from truncate() may less than 2nd argument, need padding
--             if curWidth + chunkWidth < targetWidth then
--                 suffix = suffix .. (' '):rep(targetWidth - curWidth - chunkWidth)
--             end
--             break
--         end
--         curWidth = curWidth + chunkWidth
--     end
--     table.insert(newVirtText, {suffix, 'MoreMsg'})
--     return newVirtText
-- end
--
-- vim.keymap.set('n', 'zK', function()
--     local winid = require('ufo').peekFoldedLinesUnderCursor()
--     if not winid then
--         -- choose one of them
--         -- coc.nvim
--         -- vim.fn.CocActionAsync('definitionHover')
--         -- nvimlsp
--         vim.lsp.buf.hover()
--     end
-- end)
--
-- require('ufo').setup({
--     fold_virt_text_handler = handler,
--     open_fold_hl_timeout = 100,
--     close_fold_kinds = {'imports', 'comment'},
--     preview = {
--         win_config = {
--             border = {'', '─', '', '', '', '─', '', ''},
--             winhighlight = 'Normal:Folded',
--             winblend = 0
--         },
--         mappings = {
--             scrollU = '<C-u>',
--             scrollD = '<C-d>'
--         }
--     },
--     provider_selector = function(bufnr, filetype, buftype)
--         -- if you prefer treesitter provider rather than lsp,
--         -- return ftMap[filetype] or {'treesitter', 'indent'}
--         -- return ftMap[filetype]
--         return {'treesitter', 'indent'}
--
--         -- refer to ./doc/example.lua for detail
--     end
-- })
--
-- local coq = require"coq"
-- END UFO

local servers = { 'pyright', 'tsserver', 'gopls', 'clangd', 'volar', 'tailwindcss', 'clangd', 'jdtls' }

for _, lsp in pairs(servers) do
    require('lspconfig')[lsp].setup({
        capabilities = capabilities,
    })
end

require('rust-tools').setup(opts)

local palettes = {
  gruvbox_light = {
    accent = '#d65d0e', -- orange
    accent_sec = '#7c6f64', -- fg4
    bg = '#ebdbb2', -- bg1
    bg_sec = '#d5c4a1', -- bg2
    fg = '#504945', -- fg2
    fg_sec = '#665c54', -- fg3
  },
  gruvbox_dark = {
    accent = '#be5e67', -- orange
    accent_sec = '#a89984', -- fg4
    bg = '#0f101a', -- bg1
    bg_sec = '#3b4252', -- bg2
    fg = '#e5e9f0', -- fg2
    fg_sec = '#d8dee9', -- fg3
  },
  edge_light = {
    accent = '#bf75d6', -- bg_purple
    accent_sec = '#8790a0', -- grey
    bg = '#eef1f4', -- bg1
    bg_sec = '#dde2e7', -- bg4
    fg = '#33353f', -- default:bg1
    fg_sec = '#4b505b', -- fg
  },
  nord = {
    accent = '#88c0d0', -- nord8
    accent_sec = '#81a1c1', -- nord9
    bg = '#3b4252', -- nord1
    bg_sec = '#4c566a', -- nord3
    fg = '#e5e9f0', -- nord4
    fg_sec = '#d8dee9', -- nord4
  },
}

local theme = {
  fill = 'TabLineFill',
  -- Also you can do this: fill = { fg='#f2e9de', bg='#907aa9', style='italic' }
  head = 'TabLine',
  current_tab = 'TabLineSel',
  tab = 'TabLine',
  win = 'TabLine',
  tail = 'TabLine',
}
local tabby_config = function()
  local palette = palettes.gruvbox_dark
  local filename = require('tabby.filename')
  local cwd = function()
    return ' ' .. vim.fn.fnamemodify(vim.fn.getcwd(), ':t') .. ' '
  end
  local tabname = function(tabid)
    return vim.api.nvim_tabpage_get_number(tabid)
  end
  local line = {
    hl = { fg = palette.fg, bg = palette.bg },
    layout = 'active_wins_at_tail',
    head = {
      { cwd, hl = { fg = palette.bg, bg = palette.accent } },
      { '', hl = { fg = palette.accent, bg = palette.bg } },
    },
    active_tab = {
      label = function(tabid)
        return {
          '  ' .. tabname(tabid) .. ' ',
          hl = { fg = palette.bg, bg = palette.accent_sec, style = 'bold' },
        }
      end,
      left_sep = { '', hl = { fg = palette.accent_sec, bg = palette.bg } },
      right_sep = { '', hl = { fg = palette.accent_sec, bg = palette.bg } },
    },
    inactive_tab = {
      label = function(tabid)
        return {
          '  ' .. tabname(tabid) .. ' ',
          hl = { fg = palette.fg, bg = palette.bg_sec, style = 'bold' },
        }
      end,
      left_sep = { '', hl = { fg = palette.bg_sec, bg = palette.bg } },
      right_sep = { '', hl = { fg = palette.bg_sec, bg = palette.bg } },
    },
    top_win = {
      label = function(winid)
        return {
          '  ' .. filename.unique(winid) .. ' ',
          hl = { fg = palette.fg, bg = palette.bg_sec },
        }
      end,
      left_sep = { '', hl = { fg = palette.bg_sec, bg = palette.bg } },
      right_sep = { '', hl = { fg = palette.bg_sec, bg = palette.bg } },
    },
    win = {
      label = function(winid)
        return {
          '  ' .. filename.unique(winid) .. ' ',
          hl = { fg = palette.fg, bg = palette.bg_sec },
        }
      end,
      left_sep = { '', hl = { fg = palette.bg_sec, bg = palette.bg } },
      right_sep = { '', hl = { fg = palette.bg_sec, bg = palette.bg } },
    },
    tail = {
      { '', hl = { fg = palette.accent_sec, bg = palette.bg } },
      { '  ', hl = { fg = palette.bg, bg = palette.accent_sec } },
    },
  }
  require('tabby').setup({ tabline = line })
end
tabby_config()

EOF
lua << EOF
vim.cmd([[ let g:neo_tree_remove_legacy_commands = 1 ]])
require("neo-tree").setup({
    close_if_last_window = true, -- Close Neo-tree if it is the last window left in the tab
    popup_border_style = "rounded",
    enable_git_status = true,
    enable_diagnostics = true,
    sort_case_insensitive = false, -- used when sorting files and directories in the tree
    sort_function = nil , -- use a custom function for sorting files and directories in the tree
    -- sort_function = function (a,b)
    --       if a.type == b.type then
    --           return a.path > b.path
    --       else
    --           return a.type > b.type
    --       end
    --   end , -- this sorts files and directories descendantly
    default_component_configs = {
      container = {
        enable_character_fade = true
      },
      indent = {
        indent_size = 2,
        padding = 1, -- extra padding on left hand side
        -- indent guides
        with_markers = true,
        indent_marker = "│",
        last_indent_marker = "└",
        highlight = "NeoTreeIndentMarker",
        -- expander config, needed for nesting files
        with_expanders = nil, -- if nil and file nesting is enabled, will enable expanders
        expander_collapsed = "",
        expander_expanded = "",
        expander_highlight = "NeoTreeExpander",
      },
      icon = {
        folder_closed = "",
        folder_open = "",
        folder_empty = "ﰊ",
        -- The next two settings are only a fallback, if you use nvim-web-devicons and configure default icons there
        -- then these will never be used.
        default = "*",
        highlight = "NeoTreeFileIcon"
      },
      modified = {
        symbol = "+",
        highlight = "NeoTreeModified",
      },
      name = {
        trailing_slash = false,
        use_git_status_colors = true,
        highlight = "NeoTreeFileName",
      },
      git_status = {
        symbols = {
          -- Change type
          added     = "", -- or "✚", but this is redundant info if you use git_status_colors on the name
          modified  = "", -- or "", but this is redundant info if you use git_status_colors on the name
          deleted   = "✖",-- this can only be used in the git_status source
          renamed   = "",-- this can only be used in the git_status source
          -- Status type
          untracked = "",
          ignored   = "",
          unstaged  = "",
          staged    = "",
          conflict  = "",
        }
      },
    },
    window = {
      position = "left",
      width = 33,
      mapping_options = {
        noremap = true,
        nowait = true,
      },
      mappings = {
        ["<space>"] = {
            "toggle_node",
            nowait = false, -- disable `nowait` if you have existing combos starting with this char that you want to use
        },
        ["<2-LeftMouse>"] = "open",
        ["<cr>"] = "open",
        ["<esc>"] = "revert_preview",
        ["P"] = { "toggle_preview", config = { use_float = true } },
        ["S"] = "open_split",
        ["s"] = "open_vsplit",
        -- ["S"] = "split_with_window_picker",
        -- ["s"] = "vsplit_with_window_picker",
        ["t"] = "open_tabnew",
        -- ["<cr>"] = "open_drop",
        -- ["t"] = "open_tab_drop",
        ["w"] = "open_with_window_picker",
        --["P"] = "toggle_preview", -- enter preview mode, which shows the current node without focusing
        ["C"] = "close_node",
        ["z"] = "close_all_nodes",
        --["Z"] = "expand_all_nodes",
        ["a"] = {
          "add",
          -- some commands may take optional config options, see `:h neo-tree-mappings` for details
          config = {
            show_path = "none" -- "none", "relative", "absolute"
          }
        },
        ["A"] = "add_directory", -- also accepts the optional config.show_path option like "add".
        ["d"] = "delete",
        ["r"] = "rename",
        ["y"] = "copy_to_clipboard",
        ["x"] = "cut_to_clipboard",
        ["p"] = "paste_from_clipboard",
        ["c"] = "copy", -- takes text input for destination, also accepts the optional config.show_path option like "add":
        -- ["c"] = {
        --  "copy",
        --  config = {
        --    show_path = "none" -- "none", "relative", "absolute"
        --  }
        --}
        ["m"] = "move", -- takes text input for destination, also accepts the optional config.show_path option like "add".
        ["q"] = "close_window",
        ["R"] = "refresh",
        ["?"] = "show_help",
        ["<"] = "prev_source",
        [">"] = "next_source",
      }
    },
    nesting_rules = {},
    filesystem = {
      filtered_items = {
        visible = false, -- when true, they will just be displayed differently than normal items
        hide_dotfiles = true,
        hide_gitignored = true,
        hide_hidden = true, -- only works on Windows for hidden files/directories
        hide_by_name = {
          --"node_modules"
        },
        hide_by_pattern = { -- uses glob style patterns
          --"*.meta",
          --"*/src/*/tsconfig.json",
        },
        always_show = { -- remains visible even if other settings would normally hide it
          --".gitignored",
        },
        never_show = { -- remains hidden even if visible is toggled to true, this overrides always_show
          --".DS_Store",
          --"thumbs.db"
        },
        never_show_by_pattern = { -- uses glob style patterns
          --".null-ls_*",
        },
      },
      follow_current_file = true, -- This will find and focus the file in the active buffer every
                                   -- time the current file is changed while the tree is open.
      group_empty_dirs = false, -- when true, empty folders will be grouped together
      hijack_netrw_behavior = "open_default", -- netrw disabled, opening a directory opens neo-tree
                                              -- in whatever position is specified in window.position
                            -- "open_current",  -- netrw disabled, opening a directory opens within the
                                              -- window like netrw would, regardless of window.position
                            -- "disabled",    -- netrw left alone, neo-tree does not handle opening dirs
      use_libuv_file_watcher = false, -- This will use the OS level file watchers to detect changes
                                      -- instead of relying on nvim autocmd events.
      window = {
        mappings = {
          ["<bs>"] = "navigate_up",
          ["."] = "set_root",
          ["H"] = "toggle_hidden",
          ["/"] = "fuzzy_finder",
          ["D"] = "fuzzy_finder_directory",
          ["f"] = "filter_on_submit",
          ["<c-x>"] = "clear_filter",
          ["[g"] = "prev_git_modified",
          ["]g"] = "next_git_modified",
        }
      }
    },
    buffers = {
      follow_current_file = true, -- This will find and focus the file in the active buffer every
                                   -- time the current file is changed while the tree is open.
      group_empty_dirs = true, -- when true, empty folders will be grouped together
      show_unloaded = true,
      window = {
        mappings = {
          ["bd"] = "buffer_delete",
          ["<bs>"] = "navigate_up",
          ["."] = "set_root",
        }
      },
    },
    git_status = {
      window = {
        position = "float",
        mappings = {
          ["A"]  = "git_add_all",
          ["gu"] = "git_unstage_file",
          ["ga"] = "git_add_file",
          ["gr"] = "git_revert_file",
          ["gc"] = "git_commit",
          ["gp"] = "git_push",
          ["gg"] = "git_commit_and_push",
        }
      }
  }})
EOF

lua << EOF
local coq = require'coq'
local cmp = require'cmp'
cmp.setup(coq.lsp_ensure_capabilities({
  -- Enable LSP snippets
  snippet = {
    expand = function(args)
        vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    -- Add tab support
    ['<S-Tab>'] = cmp.mapping.select_prev_item(),
    ['<Tab>'] = cmp.mapping.select_next_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Insert,
      select = true,
    })
  },

  -- Installed sources
  sources = {
    { name = 'nvim_lsp' },
    { name = 'vsnip' },
    { name = 'path' },
    { name = 'buffer' },
  },
}))
EOF

" Find files using Telescope command-line sugar.
nnoremap <silent> <leader>ff <cmd>Telescope find_files<CR>
nnoremap <silent> <leader>fg <cmd>Telescope live_grep<CR>
" default bind for this is fg, above, but 'find word' makes more sense in my
" head
nnoremap <silent> <leader>fw <cmd>Telescope live_grep<CR>
nnoremap <silent> <leader>fb <cmd>Telescope buffers<CR>
nnoremap <silent> <leader>fh <cmd>Telescope help_tags<CR>
nnoremap <silent> <leader>fs <cmd>Telescope possession list<CR>

nnoremap <silent> <leader>tt <cmd>lua require("grapple").toggle()<CR>
nnoremap <silent> <leader>tr <cmd>lua require("grapple").untag()<CR>
nnoremap <silent> <leader>tn <cmd>lua require("grapple").cycle_forward()<CR>
nnoremap <silent> <leader>tb <cmd>lua require("grapple").cycle_backward()<CR>
nnoremap <silent> <leader>tl <cmd>lua require("grapple").popup_tags()<CR>

" search buffers with fzf
nnoremap <silent> <C-b>Telescope buffers<CR>
nnoremap <silent> <C-f>Telescope live_grep<CR>


" General options
let g:presence_auto_update         = 1
let g:presence_neovim_image_text   = "nvim > vscode"
let g:presence_main_image          = "neovim"
let g:presence_client_id           = "793271441293967371"
let g:presence_debounce_timeout    = 5000
let g:presence_enable_line_number  = 1
let g:presence_blacklist           = []
let g:presence_buttons             = 1
let g:presence_file_assets         = {}

" Rich Presence text options
let g:presence_editing_text        = "editing %s"
let g:presence_file_explorer_text  = "browsing %s"
let g:presence_git_commit_text     = "git-ing good"
let g:presence_plugin_manager_text = "fixing plugins"
let g:presence_reading_text        = "reading %s"
let g:presence_workspace_text      = "working on %s"
let g:presence_line_number_text    = "line [%s/%s]"

" add '_' as a word delimeter
" set iskeyword-=_

" if the current buffer is open when fzf opens, jump to it in selection
let g:fzf_buffers_jump = 1

" disable status line for fzf
autocmd! FileType fzf set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

" shorter lsp update time
set updatetime=200
" Show diagnostic popup on cursor hold
autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focusable = false })

" Goto previous/next diagnostic warning/error
nnoremap <silent> g[ <cmd>lua vim.diagnostic.goto_prev()<CR>
nnoremap <silent> g] <cmd>lua vim.diagnostic.goto_next()<CR>

" Code navigation shortcuts
nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<CR>
nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR>
nnoremap <silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>
nnoremap <silent> gd    <cmd>lua vim.lsp.buf.definition()<CR>

nnoremap <silent> ga    <cmd>lua vim.lsp.buf.code_action()<CR>

function! WinMove(key)
    let t:curwin = winnr()
    exec "wincmd ".a:key
    if (t:curwin == winnr())
        if (match(a:key,'[jk]'))
            wincmd v
        else
            wincmd s
        endif
        exec "wincmd ".a:key
    endif
endfunction

autocmd BufEnter * ++nested if winnr('$') == 1 && bufname() == 'NvimTree_' . tabpagenr() | quit | endif

function InitialOpen()
    if eval("@%") == ""
        :DashboardNewFile
    endif
    :Neotree toggle
endfunction

nnoremap <silent> <C-n> :call InitialOpen()<CR>
" More available functions:
" NvimTreeOpen
" NvimTreeClose
" NvimTreeFocus
" NvimTreeFindFileToggle
" NvimTreeResize
" NvimTreeCollapse
" NvimTreeCollapseKeepBuffers

set termguicolors

nnoremap <silent> <C-h> :call WinMove('h')<CR>
nnoremap <silent> <C-j> :call WinMove('j')<CR>
nnoremap <silent> <C-k> :call WinMove('k')<CR>
nnoremap <silent> <C-l> :call WinMove('l')<CR>

" comfy keybind to search
nnoremap <C-s> /
vnoremap <silent> <C-s> gc
" clear search next time esc is hit
nnoremap <silent> <esc> :noh <CR>

" open terminal
nnoremap <silent> t :terminal<CR>

" unicode characters in the file autoload/float.vim
set encoding=utf-8

" TextEdit might fail if hidden is not set.
set hidden

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=1

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("nvim-0.5.0") || has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif
" inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" binds escape in terminal mode
tnoremap <Esc> <C-\><C-n>

" buffer navigation
nnoremap <silent> gl :bn<CR>
nnoremap <silent> gh :bp<CR>
" kill buffer, gd doesn't seem right but g-kill makes sense
nnoremap <silent> gk :bp<bar>sp<bar>bn<bar>bd<CR>

if has('clipboard')
    " copy and cut to both buffers
    vnoremap <silent> <C-c> "+y | %+y
    vnoremap <silent> <C-x> "+d | %+d
    nnoremap <silent> <p> "+p <CR>
endif

" turn hybrid line numbers on
:set number relativenumber
:set nu rnu

" when using :w! to save, chmod the file too
function! g:ChmodOnWrite()
  if v:cmdbang
    silent !chmod u+w %
  endif
endfunction

autocmd BufWrite * call g:ChmodOnWrite()

" handle urls with # in them correctly
function! HandleURL()
  let s:uri = matchstr(getline("."), '[a-z]*:\/\/[^ >,;]*')
  echo s:uri
  if s:uri != ""
    silent exec "!xdg-open '".s:uri."'"
  else
    echo "No URI found in line."
  endif
endfunction

map <leader>u :call HandleURL()<cr>

let g:Hexokinase_highlighters = ['backgroundfull']

" :Rename command
command! -nargs=1 Rename saveas <args> | call delete(expand('#')) | bd #

set viminfo=%,<800,'10,/50,:100,h,f0,n~/.config/viminfo
"           | |    |   |   |    | |  + viminfo file path
"           | |    |   |   |    | + file marks 0-9,A-Z 0=NOT stored
"           | |    |   |   |    + disable 'hlsearch' loading viminfo
"           | |    |   |   + command-line history saved
"           | |    |   + search history saved
"           | |    + files marks saved
"           | + lines saved each register (old name for <, vi6.2)
"           + save/restore buffer list

augroup myvimrc au!
    au BufWritePost .vimrc,_vimrc,vimrc,.gvimrc,_gvimrc,gvimrc so $MYVIMRC | if has('gui_running') | so $MYGVIMRC | endif
augroup END

command! PU PlugUpdate | PlugUpgrade

" move vim info file
set viminfo+=n~/.config/viminfo
" The width of a hard tabstop measured in "spaces" -- effectively the (maximum) width of an actual tab character.
set tabstop=4
" Setting this to a non-zero value other than tabstop will make the tab key (in insert mode) insert a combination of spaces (and possibly tabs) to simulate tab stops at this width.
set softtabstop=4
" Enabling this will make the tab key (in insert mode) insert spaces instead of tab characters. This also affects the behavior of the retab command.
set expandtab
" Enabling this will make the tab key (in insert mode) insert spaces or tabs to go to the next indent of the next tabstop when the cursor is at the beginning of a line (i.e. the only preceding characters are whitespace).
set smarttab
set shiftwidth=4

nnoremap <silent> <C-p> :lua require("notify").dismiss() <CR>

" auto update plugged every week
function! OnVimEnter() abort
  if exists('g:plug_home')
    let l:filename = printf('%s/.vim_plug_update', g:plug_home)
    if filereadable(l:filename) == 0
      call writefile([], l:filename)
    endif

    let l:this_week = strftime('%Y_%V')
    let l:contents = readfile(l:filename)
    if index(l:contents, l:this_week) < 0
      " launch headless
      call execute(':!nvim +PlugUpdate +qa > /dev/null 2&>1')
      call writefile([l:this_week], l:filename, 'a')
    endif
  endif
  lua require("notify").dismiss()
endfunction

autocmd VimEnter * call OnVimEnter()

if has('autocmd') && execute("file") != '.config/nvim/init.vim' " Remain compatible with earlier versions
 augroup vimrc
    autocmd! BufWritePost $MYVIMRC source % | echom "Reloaded " . $MYVIMRC | redraw
    autocmd! BufWritePost $MYGVIMRC if has('gui_running') | so % | echom "Reloaded " . $MYGVIMRC | endif | redraw
  augroup END
endif " has autocmd
