vim.g.python3_host_prog = "/home/sean/.config/pyenv/shims/python3"
vim.opt.ttimeoutlen = 2
vim.opt.mouse = "a"
vim.opt.cmdheight = 1
vim.opt.shortmess:append({ c = true })
vim.opt.hidden = true
vim.opt.nu = true
vim.opt.updatetime = 100
vim.opt.encoding = "utf-8"
vim.opt.termguicolors = true
vim.opt.showtabline = 2
-- THEME
vim.cmd([[
	silent! colorscheme snow
	hi Normal guibg=#191919
]])
vim.cmd("set signcolumn=number")
vim.cmd("set t_Co=256")
vim.cmd("set nobackup")
vim.cmd("set nowritebackup")
vim.cmd("autocmd BufLeave,BufWinLeave * silent! mkview")
vim.cmd("autocmd BufReadPost * silent! loadview")
vim.cmd(
	"autocmd! FileType fzf set laststatus=0 noshowmode noruler | autocmd BufLeave <buffer> set laststatus=2 showmode ruler"
)
vim.cmd("autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focusable = false })")
vim.cmd("autocmd BufWrite * call g:ChmodOnWrite()")
vim.cmd("command! -nargs=1 Rename saveas <args> | call delete(expand('#')) | bd #")
vim.cmd("set viminfo=%,<800,'10,/50,:100,h,f0,n~/.config/viminfo")
vim.cmd("set viminfo=%,<800,'10,/50,:100,h,f0,n~/.config/viminfo")

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

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

-- example using a list of specs with the default options
vim.g.mapleader = " " -- make sure to set `mapleader` before lazy so your mappings are correct

require("lazy").setup({
	{
		"williamboman/mason.nvim",
		config = function()
			require("mason").setup({

				-- Controls to which degree logs are written to the log file. It's useful to set this to vim.log.levels.DEBUG when
				-- debugging issues with package installations.
				log_level = vim.log.levels.INFO,

				-- Limit for the maximum amount of packages to be installed at the same time. Once this limit is reached, any further
				-- packages that are requested to be installed will be put in a queue.
				max_concurrent_installers = 4,

				-- [Advanced setting]
				-- The registries to source packages from. Accepts multiple entries. Should a package with the same name exist in
				-- multiple registries, the registry listed first will be used.
				registries = {
					"github:mason-org/mason-registry",
				},

				-- The provider implementations to use for resolving supplementary package metadata (e.g., all available versions).
				-- Accepts multiple entries, where later entries will be used as fallback should prior providers fail.
				-- Builtin providers are:
				--   - mason.providers.registry-api  - uses the https://api.mason-registry.dev API
				--   - mason.providers.client        - uses only client-side tooling to resolve metadata
				providers = {
					"mason.providers.registry-api",
					"mason.providers.client",
				},

				github = {
					-- The template URL to use when downloading assets from GitHub.
					-- The placeholders are the following (in order):
					-- 1. The repository (e.g. "rust-lang/rust-analyzer")
					-- 2. The release version (e.g. "v0.3.0")
					-- 3. The asset name (e.g. "rust-analyzer-v0.3.0-x86_64-unknown-linux-gnu.tar.gz")
					download_url_template = "https://github.com/%s/releases/download/%s/%s",
				},

				pip = {
					-- Whether to upgrade pip to the latest version in the virtual environment before installing packages.
					upgrade_pip = false,

					-- These args will be added to `pip install` calls. Note that setting extra args might impact intended behavior
					-- and is not recommended.
					--
					-- Example: { "--proxy", "https://proxyserver" }
					install_args = {},
				},

				ui = {
					-- Whether to automatically check for new versions when opening the :Mason window.
					check_outdated_packages_on_open = false,

					-- The border to use for the UI window. Accepts same border values as |nvim_open_win()|.
					border = "none",

					-- Width of the window. Accepts:
					-- - Integer greater than 1 for fixed width.
					-- - Float in the range of 0-1 for a percentage of screen width.
					width = 0.8,

					-- Height of the window. Accepts:
					-- - Integer greater than 1 for fixed height.
					-- - Float in the range of 0-1 for a percentage of screen height.
					height = 0.9,

					icons = {
						package_installed = "✓",
						package_pending = "➜",
						package_uninstalled = "✗",
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
			})
		end,
	},
	{
		"linux-cultist/venv-selector.nvim",
		dependencies = {
			"neovim/nvim-lspconfig",
			"mfussenegger/nvim-dap",
			"mfussenegger/nvim-dap-python", --optional
			{ "nvim-telescope/telescope.nvim", branch = "0.1.x", dependencies = { "nvim-lua/plenary.nvim" } },
		},
		lazy = true,
		branch = "regexp", -- This is the regexp branch, use this for the new version
		config = function()
			require("venv-selector").setup()
		end,
		keys = {
			{ ",v", "<cmd>VenvSelect<cr>" },
		},
	},
	--"nosduco/remote-sshfs.nvim",
	{
		"junegunn/fzf",
		build = "fzf#install()",
	},
	{
		"shellRaining/hlchunk.nvim",
		event = { "UIEnter" },
		config = function()
			require("hlchunk").setup({
				indent = {
					chars = { "│", "¦", "┆", "┊" }, -- more code can be found in https://unicodeplus.com/

					style = {
						"#48484d",
					},
				},
				blank = {
					enable = false,
				},
			})
		end,
	},
	"ntpeters/vim-better-whitespace",
	{
		"vidocqh/data-viewer.nvim",
		opts = {},
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
	},
	{
		"NeogitOrg/neogit",
		dependencies = {
			"nvim-lua/plenary.nvim", -- required
			"sindrets/diffview.nvim", -- optional - Diff integration

			-- Only one of these is needed.
			"nvim-telescope/telescope.nvim", -- optional
		},
		config = true,
	},
	"junegunn/fzf.vim",
	{
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup()
		end,
	},
	{
		"kevinhwang91/nvim-hlslens",
		config = function()
			require("hlslens").setup()
		end,
	},
	{
		"stevearc/dressing.nvim",
		opts = {},
		config = function()
			require("dressing").setup({
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
					-- format_item_override = {},

					-- see :help dressing_get_config
					get_config = nil,
				},
			})
		end,
	},
	{
		"stevearc/conform.nvim",
		config = function()
			require("conform").setup({
				formatters_by_ft = {
					lua = { "stylua" },
					go = { "goimports", "gofmt" },
					-- Conform will run multiple formatters sequentially
					python = { "isort", "black" },
					-- Use a sub-list to run only the first available formatter
					javascript = { "prettierd", "prettier", stop_after_first = true },
					html = { "prettierd", "prettier", stop_after_first = true },
					rust = { "rustfmt", args = { "--edition", "2021" } },
				},
			})
		end,
	},
	"sindrets/diffview.nvim",
	{
		"lewis6991/gitsigns.nvim",
		config = function()
			require("gitsigns").setup({
				signs = {
					add = { text = "┃" },
					change = { text = "┃" },
					delete = { text = "_" },
					topdelete = { text = "‾" },
					changedelete = { text = "~" },
					untracked = { text = "┆" },
				},
				signs_staged = {
					add = { text = "┃" },
					change = { text = "┃" },
					delete = { text = "_" },
					topdelete = { text = "‾" },
					changedelete = { text = "~" },
					untracked = { text = "┆" },
				},
				signs_staged_enable = true,
				signcolumn = true, -- Toggle with `:Gitsigns toggle_signs`
				numhl = false, -- Toggle with `:Gitsigns toggle_numhl`
				linehl = false, -- Toggle with `:Gitsigns toggle_linehl`
				word_diff = false, -- Toggle with `:Gitsigns toggle_word_diff`
				watch_gitdir = {
					interval = 4000,
					follow_files = true,
				},
				auto_attach = true,
				attach_to_untracked = false,
				current_line_blame = true, -- Toggle with `:Gitsigns toggle_current_line_blame`
				current_line_blame_opts = {
					virt_text = true,
					virt_text_pos = "eol", -- 'eol' | 'overlay' | 'right_align'
					delay = 4000,
					ignore_whitespace = false,
					virt_text_priority = 100,
					use_focus = true,
				},
				current_line_blame_formatter = "<author>, <author_time:%R> - <summary>",
				sign_priority = 6,
				update_debounce = 1000,
				status_formatter = nil, -- Use default
				max_file_length = 20000, -- Disable if file is longer than this (in lines)
				preview_config = {
					-- Options passed to nvim_open_win
					border = "single",
					style = "minimal",
					relative = "cursor",
					row = 0,
					col = 1,
				},
			})
		end,
	},
	--[[
	{
		"rebelot/kanagawa.nvim",
		config = function()
			require("kanagawa").setup({
				compile = true, -- enable compiling the colorscheme
				undercurl = true, -- enable undercurls
				commentStyle = { italic = true },
				functionStyle = {},
				keywordStyle = { italic = true },
				statementStyle = { bold = true },
				typeStyle = {},
				transparent = true, -- do not set background color
				dimInactive = false, -- dim inactive window `:h hl-NormalNC`
				terminalColors = true, -- define vim.g.terminal_color_{0,17}
				colors = { -- add/modify theme and palette colors
					palette = {},
					theme = {
						all = {
							ui = {
								bg_gutter = "none",
							},
						},
					},
				},
				overrides = function(colors) -- add/modify highlights
					local theme = colors.theme
					return {
						Pmenu = { fg = theme.ui.shade0, bg = theme.ui.bg_p1 }, -- add `blend = vim.o.pumblend` to enable transparency
						PmenuSel = { fg = "NONE", bg = theme.ui.bg_p2 },
						PmenuSbar = { bg = theme.ui.bg_m1 },
						PmenuThumb = { bg = theme.ui.bg_p2 },
					}
				end,
				theme = "dragon", -- Load "wave" theme when 'background' option is not set
				background = { -- map the value of 'background' option to a theme
					dark = "dragon", -- try "dragon" !
					light = "lotus",
				},
			})
		end,
	},
	--]]
	--[[
	{
		"ggandor/flit.nvim",
		config = function()
			require("flit").setup({
				keys = { f = "f", F = "F", t = "t", T = "T" },
				-- A string like "nv", "nvo", "o", etc.
				labeled_modes = "v",
				multiline = true,
				-- Like `leap`s similar argument (call-specific overrides).
				-- E.g.: opts = { equivalence_classes = {} }
				opts = {},
			})
		end,
	},]]
	--
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		config = function()
			require("nvim-treesitter.configs").setup({
				indent = { enable = false },
				incremental_selection = { enable = false },
				rainbow = {
					enable = true,
					-- disable = { "jsx", "cpp" }, list of languages you want to disable the plugin for
					extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
					max_file_lines = 15000, -- Do not enable for files with more than n lines, int
					-- colors = {}, -- table of hex strings
					-- termcolors = {} -- table of colour name strings
				},
				-- A list of parser names, or "all"
				ensure_installed = { "rust", "lua", "c", "python", "lua", "javascript", "typescript", "toml" },
				ignore_install = { "regex" },

				-- Install parsers synchronously (only applied to `ensure_installed`)
				sync_install = false,

				-- Automatically install missing parsers when entering buffer
				auto_install = true,

				highlight = {
					-- `false` will disable the whole extension
					disable = function(lang, buf)
						local max_filesize = 100 * 1024 -- 100 KB
						local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
						if ok and stats and stats.size > max_filesize then
							return true
						end
					end,

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
			})
		end,
	},
	{ "simrat39/rust-tools.nvim" },
	{
		"williamboman/mason-lspconfig.nvim",
		config = function()
			local MASON_DEFAULT = {
				-- A list of servers to automatically install if they're not already installed. Example: { "rust_analyzer@nightly", "sumneko_lua" }
				-- This setting has no relation with the `automatic_installation` setting.
				ensure_installed = {
					"rust_analyzer",
					"tailwindcss",
					"svelte",
					"lua_ls",
					"jedi_language_server",
					"clangd",
				},

				-- Whether servers that are set up (via lspconfig) should be automatically installed if they're not already installed.
				-- This setting has no relation with the `ensure_installed` setting.
				-- Can either be:
				--   - false: Servers are not automatically installed.
				--   - true: All servers set up via lspconfig are automatically installed.
				--   - { exclude: string[] }: All servers set up via lspconfig, except the ones provided in the list, are automatically installed.
				--       Example: automatic_installation = { exclude = { "rust_analyzer", "solargraph" } }
				automatic_installation = true,

				handlers = {
					-- The first entry (without a key) will be the default handler
					-- and will be called for each installed server that doesn't have
					-- a dedicated handler.
					-- Next, you can provide a dedicated handler for specific servers.
					-- For example, a handler override for the `rust_analyzer`:
					["rust_analyzer"] = function()
						local opts = {
							tools = { -- rust-tools options

								-- how to execute terminal commands
								-- options right now: termopen / quickfix / toggleterm / vimux
								executor = require("rust-tools.executors").termopen,

								-- callback to execute once rust-analyzer is done initializing the workspace
								-- The callback receives one parameter indicating the `health` of the server: "ok" | "warning" | "error"
								on_initialized = nil,

								-- automatically call RustReloadWorkspace when writing to a Cargo.toml file.
								reload_workspace_from_cargo_toml = true,

								-- These apply to the default RustSetInlayHints command
								inlay_hints = {
									-- automatically set inlay hints (type hints)
									-- default: true
									auto = true,

									-- Only show inlay hints for the current line
									only_current_line = false,

									-- whether to show parameter hints with the inlay hints or not
									-- default: true
									show_parameter_hints = true,

									-- prefix for parameter hints
									-- default: "<-"
									parameter_hints_prefix = "<- ",

									-- prefix for all the other hints (type, chaining)
									-- default: "=>"
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

									-- Maximal width of the hover window. Nil means no max.
									max_width = nil,

									-- Maximal height of the hover window. Nil means no max.
									max_height = nil,

									-- whether the hover action window gets automatically focused
									-- default: false
									auto_focus = false,
								},

								-- settings for showing the crate graph based on graphviz and the dot
								-- command
								crate_graph = {
									-- Backend used for displaying the graph
									-- see: https://graphviz.org/docs/outputs/
									-- default: x11
									backend = "x11",
									-- where to store the output, nil for no output stored (relative
									-- path from pwd)
									-- default: nil
									output = nil,
									-- true for all crates.io and external crates, false only the local
									-- crates
									-- default: true
									full = true,

									-- List of backends found on: https://graphviz.org/docs/outputs/
									-- Is used for input validation and autocompletion
									-- Last updated: 2021-08-26
									enabled_graphviz_backends = {
										"bmp",
										"cgimage",
										"canon",
										"dot",
										"gv",
										"xdot",
										"xdot1.2",
										"xdot1.4",
										"eps",
										"exr",
										"fig",
										"gd",
										"gd2",
										"gif",
										"gtk",
										"ico",
										"cmap",
										"ismap",
										"imap",
										"cmapx",
										"imap_np",
										"cmapx_np",
										"jpg",
										"jpeg",
										"jpe",
										"jp2",
										"json",
										"json0",
										"dot_json",
										"xdot_json",
										"pdf",
										"pic",
										"pct",
										"pict",
										"plain",
										"plain-ext",
										"png",
										"pov",
										"ps",
										"ps2",
										"psd",
										"sgi",
										"svg",
										"svgz",
										"tga",
										"tiff",
										"tif",
										"tk",
										"vml",
										"vmlz",
										"wbmp",
										"webp",
										"xlib",
										"x11",
									},
								},
							},

							-- all the opts to send to nvim-lspconfig
							-- these override the defaults set by rust-tools.nvim
							-- see https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#rust_analyzer
							server = {
								-- standalone file support
								-- setting it to false may improve startup time
								standalone = true,
							}, -- rust-analyzer options

							-- debugging stuff
							dap = {
								adapter = {
									type = "executable",
									command = "lldb-vscode",
									name = "rt_lldb",
								},
							},
						}

						require("rust-tools").setup(opts)
					end,
				},
			}
			require("mason-lspconfig").setup(MASON_DEFAULT)
		end,
	},
	{
		"iamcco/markdown-preview.nvim",
		build = "mkdp#util#install()",
	},
	{
		"nvim-telescope/telescope-fzf-native.nvim",
		build = "make",
	},
	{
		"jedrzejboczar/possession.nvim",
		config = function()
			require("possession").setup({
				commands = {
					save = "SSave",
					load = "SLoad",
					delete = "SDelete",
					list = "SList",
				},
			})
		end,
	},
	"timakro/vim-yadi",
	"kyazdani42/nvim-web-devicons",
	{
		"nvim-telescope/telescope.nvim",
		config = function()
			require("telescope").setup({
				extensions = {
					fzf = {
						fuzzy = true, -- false will only do exact matching
						override_generic_sorter = true, -- override the generic sorter
						override_file_sorter = true, -- override the file sorter
						case_mode = "smart_case", -- or "ignore_case" or "respect_case"
						-- the default case_mode is "smart_case"
					},
				},
			})

			-- To get fzf loaded and working with telescope, you need to call
			-- load_extension, somewhere after setup function:
			require("telescope").load_extension("fzf")
			require("telescope").load_extension("possession")
			-- require("telescope").load_extension("dir")

			local nvim_lsp = require("lspconfig")

			local kopts = { noremap = true, silent = true }
			vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, kopts)
			vim.keymap.set("n", "]d", vim.diagnostic.goto_next, kopts)

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
				capabilities = capabilities,
			})
		end,
	},
	"nvim-telescope/telescope-fzf-native.nvim",
	"hrsh7th/cmp-vsnip",
	"hrsh7th/cmp-nvim-lsp",
	"hrsh7th/cmp-path",
	"hrsh7th/cmp-buffer",
	"hrsh7th/vim-vsnip",
	{
		"hrsh7th/nvim-cmp",
		config = function()
			local cmp = require("cmp")
			cmp.setup({
				-- Enable LSP snippets
				snippet = {
					expand = function(args)
						vim.fn["vsnip#anonymous"](args.body)
					end,
				},
				mapping = {
					["<C-p>"] = cmp.mapping.select_prev_item(),
					["<C-n>"] = cmp.mapping.select_next_item(),
					-- Add tab support
					["<S-Tab>"] = cmp.mapping.select_prev_item(),
					["<Tab>"] = cmp.mapping.select_next_item(),
					["<C-d>"] = cmp.mapping.scroll_docs(-4),
					["<C-f>"] = cmp.mapping.scroll_docs(4),
					["<C-Space>"] = cmp.mapping.complete(),
					["<C-e>"] = cmp.mapping.close(),
					["<CR>"] = cmp.mapping.confirm({
						behavior = cmp.ConfirmBehavior.Insert,
						select = true,
					}),
				},

				-- Installed sources
				sources = {
					{ name = "nvim_lsp" },
					{ name = "vsnip" },
					{ name = "path" },
					{ name = "buffer" },
				},
			})
		end,
	},
	{
		"neovim/nvim-lspconfig",
	},
	"wakatime/vim-wakatime",
	{
		"windwp/nvim-autopairs",
		config = function()
			require("nvim-autopairs").setup({
				enable_check_bracket_line = false,
			})
		end,
	},
	{
		"ellisonleao/gruvbox.nvim",
		priority = 1000,
		config = function()
			require("gruvbox").setup({
				terminal_colors = true, -- add neovim terminal colors
				undercurl = true,
				underline = true,
				bold = true,
				italic = {
					strings = true,
					emphasis = true,
					comments = true,
					operators = false,
					folds = true,
				},
				strikethrough = true,
				invert_selection = false,
				invert_signs = false,
				invert_tabline = false,
				invert_intend_guides = false,
				inverse = true, -- invert background for search, diffs, statuslines and errors
				contrast = "hard", -- can be "hard", "soft" or empty string
				palette_overrides = {},
				overrides = {},
				dim_inactive = false,
				transparent_mode = true,
			})
			vim.cmd("colorscheme gruvbox")
		end,
	},
	"tpope/vim-repeat",
	--	{
	--		"ggandor/leap.nvim",
	--		config = function()
	--			require("leap").add_default_mappings()
	--			vim.keymap.del({ "x", "o" }, "x")
	--			vim.keymap.del({ "x", "o" }, "X")
	--		end,
	--	},
	{
		"folke/flash.nvim",
		event = "VeryLazy",
		---@type Flash.Config
		opts = {},
	  -- stylua: ignore
	  keys = {
		{ "s", mode = { "n", "x", "o" }, function() require("flash").jump() end, desc = "Flash" },
		{ "S", mode = { "n", "x", "o" }, function() require("flash").treesitter() end, desc = "Flash Treesitter" },
		{ "r", mode = "o", function() require("flash").remote() end, desc = "Remote Flash" },
		{ "R", mode = { "o", "x" }, function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
		{ "<c-s>", mode = { "c" }, function() require("flash").toggle() end, desc = "Toggle Flash Search" },
	  },
	},
	"MunifTanjim/nui.nvim",
	{
		"rcarriga/nvim-notify",
		config = function()
			require("notify").setup({
				relative = "editor",
				anchor = "NE",
				width = 50,
				height = 10,
				background_colour = "#c9d2e4",
				render = "minimal",
			})
		end,
	},
	"petobens/poet-v",
	"lambdalisue/suda.vim",
	"nvim-lua/plenary.nvim",
	{
		"norcalli/nvim-colorizer.lua",
		config = function()
			require("colorizer").setup()
		end,
	},
	{
		"nvim-neo-tree/neo-tree.nvim",
		config = function()
			require("neo-tree").setup({
				close_if_last_window = true, -- Close Neo-tree if it is the last window left in the tab
				popup_border_style = "rounded",
				enable_git_status = true,
				enable_diagnostics = false,
				sort_case_insensitive = true, -- used when sorting files and directories in the tree
				sort_function = nil, -- use a custom function for sorting files and directories in the tree
				-- sort_function = function (a,b)
				--       if a.type == b.type then
				--           return a.path > b.path
				--       else
				--           return a.type > b.type
				--       end
				--   end , -- this sorts files and directories descendantly
				default_component_configs = {
					container = {
						enable_character_fade = true,
					},
					indent = {
						indent_size = 1,
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
						highlight = "NeoTreeFileIcon",
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
							added = "", -- or "✚", but this is redundant info if you use git_status_colors on the name
							modified = "", -- or "", but this is redundant info if you use git_status_colors on the name
							deleted = "✖", -- this can only be used in the git_status source
							renamed = "", -- this can only be used in the git_status source
							-- Status type
							untracked = "",
							ignored = "",
							unstaged = "",
							staged = "",
							conflict = "",
						},
					},
				},
				window = {
					position = "float",
					width = 30,
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
								show_path = "none", -- "none", "relative", "absolute"
							},
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
					},
				},
				nesting_rules = {},
				filesystem = {
					filtered_items = {
						visible = false, -- when true, they will just be displayed differently than normal items
						hide_dotfiles = false,
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
							".DS_Store",
							"thumbs.db",
						},
						never_show_by_pattern = { -- uses glob style patterns
							--".null-ls_*",
						},
					},
					follow_current_file = {
						enabled = true, -- This will find and focus the file in the active buffer every
					},
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
						},
					},
				},
				buffers = {
					follow_current_file = {
						enabled = true, -- This will find and focus the file in the active buffer every
					},
					-- time the current file is changed while the tree is open.
					group_empty_dirs = true, -- when true, empty folders will be grouped together
					show_unloaded = true,
					window = {
						mappings = {
							["bd"] = "buffer_delete",
							["<bs>"] = "navigate_up",
							["."] = "set_root",
						},
					},
				},
				git_status = {
					window = {
						position = "float",
						mappings = {
							["A"] = "git_add_all",
							["gu"] = "git_unstage_file",
							["ga"] = "git_add_file",
							["gr"] = "git_revert_file",
							["gc"] = "git_commit",
							["gp"] = "git_push",
							["gg"] = "git_commit_and_push",
						},
					},
				},
			})
		end,
	},
})

local function map(mode, lhs, rhs, opts)
	local options = { noremap = true }
	if opts then
		options = vim.tbl_extend("force", options, opts)
	end
	vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

local function get_visual_selection()
	-- Yank current visual selection into the 'v' register
	--
	-- Note that this makes no effort to preserve this register
	vim.cmd('noau normal! "vy"')

	return vim.fn.getreg("v")
end

vim.cmd([[
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
]])

vim.cmd([[
function! g:ChmodOnWrite()
  if v:cmdbang
    silent !chmod u+w %
  endif
endfunction
]])

map("v", "<C-c>", '"+y', { noremap = true, silent = true, nowait = true })
map("v", "<C-x>", ':d+" | d+%<ENTER>', { noremap = true, silent = true })
map("v", "<p>", ':"+p', { silent = true })
map("t", "<ESC>", "<C-\\><C-n>")
map("n", "gl", ":bn<ENTER>", { silent = true })
map("n", "gh", ":bp<ENTER>", { silent = true })
map("n", "gk", ":bp<bar>sp<bar>bn<bar>bd<ENTER>", { silent = true })
map("n", "<esc>", ":noh<ENTER>", { silent = true })
map("n", "T", ":terminal<ENTER>", { silent = true })
map("n", "<c-]>", ":lua vim.lsp.buf.definition()<ENTER>", { silent = true })
map("n", "K", ":lua vim.lsp.buf.hover()<ENTER>", { silent = true })
map("n", "gD", ":lua vim.lsp.buf.implementation()<ENTER>", { silent = true })
map("n", "<c-K>", ":lua vim.lsp.buf.signature_help()<ENTER>", { silent = true })
map("n", "1gD", ":lua vim.lsp.buf.type_definition()<ENTER>", { silent = true })
map("n", "gr", ":lua vim.lsp.buf.references()<ENTER>", { silent = true })
map("n", "g0", ":lua vim.lsp.buf.document_symbol()<ENTER>", { silent = true })
map("n", "gW", ":lua vim.lsp.buf.workspace_symbol()<ENTER>", { silent = true })
map("n", "gd", ":lua vim.lsp.buf.definition<ENTER>", { silent = true })
map("n", "ga", ":lua vim.lsp.buf.code_action()<ENTER>", { silent = true })
map("n", "<C-b>", ":Telescope buffers<ENTER>", { silent = true })
map("n", "<C-f>", ":Telescope live_grep<ENTER>", { silent = true })
map("n", "g[", ":lua vim.diagnostic.goto_prev()", { silent = true })
map("n", "g]", ":lua vim.diagnostic.goto_next()", { silent = true })
map("n", "gss", ":SList<ENTER>", { silent = true })
map("n", "gsl", ":SLoad", { silent = true })
map("n", "gsn", ":SSave<ENTER>", { silent = true })
map("n", "<C-s>", "/\\c")
map("v", "<C-s>", "gc")
map("n", "<C-p>", ':lua require("notify").dismiss()<ENTER>', { silent = true })
map("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", { noremap = true, silent = true })
map("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", { noremap = true, silent = true })
map("n", "K", "vim.diagnostic.open_float", { silent = true })
--require('remote-sshfs').setup({})
vim.cmd(":set guicursor=")
-- Workaround some broken plugins which set guicursor indiscriminately.
vim.cmd(":autocmd OptionSet guicursor noautocmd set guicursor=")

vim.g.clipboard = unnamedplus
--vim.g.indentLine_fileTypeExclude = ['dashboard']
--vim.g.better_whitespace_filetypes_blacklist = ['dashboard', 'terminal', 'neo-tree', 'md', 'diff', 'git', 'gitcommit', 'unite', 'qf', 'help', 'markdown', 'fugitive']
vim.cmd("let g:indentLine_fileTypeExclude = ['dashboard']")
vim.cmd(
	"let g:better_whitespace_filetypes_blacklist = ['dashboard', 'terminal', 'neo-tree', 'md', 'diff', 'git', 'gitcommit', 'unite', 'qf', 'help', 'markdown', 'fugitive', 'zsh', 'bash', '', 'json' ]"
)
vim.cmd("let g:poetv_auto_activate = 1")
vim.cmd(":set nomodeline")
vim.g.strip_whitespace_confirm = 0
vim.g.current_line_whitespace_disabled_hard = 0
vim.g.current_line_whitespace_disabled_soft = 1

local function settab(n)
	vim.o.tabstop = n
	vim.o.shiftwidth = n
	vim.o.softtabstop = n
	vim.opt.expandtab = true
	vim.opt.smartindent = true
end
settab(4)
vim.cmd("autocmd BufRead * DetectIndent")

-- vim.cmd('source ~/.config/nvim/colors.vim')
-- vim.cmd("colorscheme kanagawa")
vim.cmd([[
function! Trim()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfun

command! -nargs=0 Trim call Trim()
autocmd BufWritePost * call Trim()
]])

if vim.g.filetype == "neo-tree" then
	map("n", "o", "<ENTER>", { silent = true })
end
map("n", "<C-n>", ":Neotree<ENTER>", { silent = true })
map("n", "<C-h>", ":call WinMove('h')<ENTER>", { silent = true })
map("n", "<C-j>", ":call WinMove('j')<ENTER>", { silent = true })
map("n", "<C-k>", ":call WinMove('k')<ENTER>", { silent = true })
map("n", "<C-l>", ":call WinMove('l')<ENTER>", { silent = true })

vim.api.nvim_create_user_command("Binary", function(args)
	vim.api.nvim_exec(":%!xxd -b", false)
end, { range = false })

vim.api.nvim_create_user_command("Format", function(args)
	local range = nil
	if args.count ~= -1 then
		local end_line = vim.api.nvim_buf_get_lines(0, args.line2 - 1, args.line2, true)[1]
		range = {
			start = { args.line1, 0 },
			["end"] = { args.line2, end_line:len() },
		}
	end
	require("conform").format({ async = true, lsp_fallback = true, range = range })
end, { range = true })

function MagmaInitPython()
	vim.cmd([[
    :MagmaInit python3
    :MagmaEvaluateArgument a=5
    ]])
end

function MagmaInitCSharp()
	vim.cmd([[
    :MagmaInit .net-csharp
    :MagmaEvaluateArgument Microsoft.DotNet.Interactive.Formatting.Formatter.SetPreferredMimeTypesFor(typeof(System.Object),"text/plain");
    ]])
end

function MagmaInitFSharp()
	vim.cmd([[
    :MagmaInit .net-fsharp
    :MagmaEvaluateArgument Microsoft.DotNet.Interactive.Formatting.Formatter.SetPreferredMimeTypesFor(typeof<System.Object>,"text/plain")
    ]])
end
-- move selected lines up one line
vim.cmd("xnoremap <somekey>  :m-2<CR>gv=gv")
-- move selected lines down one line
vim.cmd("xnoremap <otherkey> :m'>+<CR>gv=gv")

vim.cmd([[
:command MagmaInitPython lua MagmaInitPython()
:command MagmaInitCSharp lua MagmaInitCSharp()
:command MagmaInitFSharp lua MagmaInitFSharp()
]])
local opts = { noremap = true, silent = true }
vim.keymap.set({ "n", "i" }, "gI", function()
	vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
end, opts)
