;;; ef-catppuccin-macchiato-theme.el --- A Catppuccin Macchiato port for ef-themes -*- lexical-binding:t -*-

;;; Commentary:
;;
;; A port of the Catppuccin Macchiato color scheme to the
;; semantic structure of the ef-themes.

;;; Code:

(eval-and-compile
  (require 'ef-themes)

  ;;;###theme-autoload
  (deftheme ef-catppuccin-macchiato
    "A Catppuccin Macchiato port for the ef-themes."
    :background-mode 'dark
    :kind 'color-scheme
    :family 'ef)

  (defconst ef-catppuccin-macchiato-palette
    '(
      ;;; Basic values from Catppuccin Macchiato
      (bg-main     "#24273a") ; base
      (fg-main     "#cad3f5") ; text
      (bg-dim      "#1e2030") ; mantle
      (fg-dim      "#a5adcb") ; subtext0
      (bg-alt      "#363a4f") ; surface0
      (fg-alt      "#b8c0e0") ; subtext1

      (bg-active   "#494d64") ; surface1
      (bg-inactive "#181926") ; crust

      ;;; Basic hues for foreground values
      (red             "#ed8796") ; red
      (red-warmer      "#f5a97f") ; peach
      (red-cooler      "#f5bde6") ; pink
      (red-faint       "#ee99a0") ; maroon
      (green           "#a6da95") ; green
      (green-warmer    "#a6da95") ; (reused)
      (green-cooler    "#8bd5ca") ; teal
      (green-faint     "#a6da95") ; (reused)
      (yellow          "#eed49f") ; yellow
      (yellow-warmer   "#f5a97f") ; peach
      (yellow-cooler   "#f4dbd6") ; rosewater
      (yellow-faint    "#eed49f") ; (reused)
      (blue            "#8aadf4") ; blue
      (blue-warmer     "#b7bdf8") ; lavender
      (blue-cooler     "#7dc4e4") ; sapphire
      (blue-faint      "#91d7e3") ; sky
      (magenta         "#c6a0f6") ; mauve
      (magenta-warmer  "#f5bde6") ; pink
      (magenta-cooler  "#b7bdf8") ; lavender
      (magenta-faint   "#c6a0f6") ; (reused)
      (cyan            "#8bd5ca") ; teal
      (cyan-warmer     "#91d7e3") ; sky
      (cyan-cooler     "#7dc4e4") ; sapphire
      (cyan-faint      "#8bd5ca") ; (reused)

      ;;; Basic hues for background values
      (bg-red-intense     "#5b6078") ; surface2
      (bg-green-intense   "#5b6078") ; surface2
      (bg-yellow-intense  "#5b6078") ; surface2
      (bg-blue-intense    "#5b6078") ; surface2
      (bg-magenta-intense "#5b6078") ; surface2
      (bg-cyan-intense    "#5b6078") ; surface2

      (bg-red-subtle      "#494d64") ; surface1
      (bg-green-subtle    "#494d64") ; surface1
      (bg-yellow-subtle   "#494d64") ; surface1
      (bg-blue-subtle     "#494d64") ; surface1
      (bg-magenta-subtle  "#494d64") ; surface1
      (bg-cyan-subtle     "#494d64") ; surface1

      ;;; Diffs
      (bg-added          "#363a4f") ; surface0
      (fg-added          green)
      (bg-changed        "#363a4f") ; surface0
      (fg-changed        yellow)
      (bg-removed        "#363a4f") ; surface0
      (fg-removed        red)
      ;; Faint and Refine are optional, mapping to main diffs for consistency
      (bg-added-faint    bg-added)
      (bg-added-refine   bg-added)
      (bg-changed-faint  bg-changed)
      (bg-changed-refine bg-changed)
      (bg-removed-faint  bg-removed)
      (bg-removed-refine bg-removed)

      ;;; Special hues
      (bg-mode-line       bg-dim)    ; mantle
      (fg-mode-line       fg-main)   ; text
      (bg-completion      bg-alt)
      (bg-hover           bg-active)
      (bg-hover-secondary bg-alt)
      (bg-hl-line         "#363a4f") ; surface0
      (bg-paren           "#5b6078") ; surface2
      (bg-err             bg-alt)
      (bg-warning         bg-alt)
      (bg-info            bg-alt)

      (border             "#5b6078") ; surface2
      (cursor             "#f4dbd6") ; rosewater
      (fg-intense         "#ffffff")

      (modeline-err       red)
      (modeline-warning   yellow)
      (modeline-info      green)

      (underline-err      red)
      (underline-warning  yellow)
      (underline-info     green)

      ;;; Mappings (Semantic assignments based on catppuccin-theme.el)
      (bg-fringe          bg-main)
      (fg-fringe          "#6e738d") ; overlay0

      (err                red)
      (warning            yellow)
      (info               green)

      (link               lavender)
      (link-alt           pink)
      (name               blue)
      (keybind            green)
      (identifier         fg-main) ; for variables
      (prompt             sky)

      (bg-region          "#494d64") ; surface1
      (fg-region          unspecified)

      ;;;; Code mappings
      (builtin            red)
      (comment            "#6e738d") ; overlay0
      (constant           peach)
      (fnname             blue)
      (keyword            mauve)
      (preprocessor       yellow)
      (docstring          comment)
      (string             green)
      (type               yellow)
      (variable           fg-main)

      ;;; Terminal mappings (ANSI colors)
      (bg-term-black        "#5b6078") ; surface2
      (fg-term-black        "#6e738d") ; overlay0
      (bg-term-black-bright "#8087a2") ; overlay1
      (fg-term-black-bright "#939ab7") ; overlay2

      (bg-term-red          red)
      (fg-term-red          red)
      (bg-term-red-bright   red)
      (fg-term-red-bright   red)

      (bg-term-green        green)
      (fg-term-green        green)
      (bg-term-green-bright green)
      (fg-term-green-bright green)

      (bg-term-yellow        yellow)
      (fg-term-yellow        yellow)
      (bg-term-yellow-bright yellow)
      (fg-term-yellow-bright yellow)

      (bg-term-blue        blue)
      (fg-term-blue        blue)
      (bg-term-blue-bright blue)
      (fg-term-blue-bright blue)

      (bg-term-magenta        pink)
      (fg-term-magenta        pink)
      (bg-term-magenta-bright pink)
      (fg-term-magenta-bright pink)

      (bg-term-cyan        teal)
      (fg-term-cyan        teal)
      (bg-term-cyan-bright teal)
      (fg-term-cyan-bright teal)

      (bg-term-white        subtext1)
      (fg-term-white        subtext1)
      (bg-term-white-bright text)
      (fg-term-white-bright text)

      ;;;; Rainbow mappings
      (rainbow-0 red)
      (rainbow-1 peach)
      (rainbow-2 yellow)
      (rainbow-3 green)
      (rainbow-4 sapphire)
      (rainbow-5 lavender)
      (rainbow-6 mauve)
      (rainbow-7 maroon)
      )
    "The `ef-catppuccin-macchiato' palette.")

  (defcustom ef-catppuccin-macchiato-palette-overrides nil
    "Overrides for `ef-catppuccin-macchiato-palette'."
    :group 'ef-themes
    :package-version '(ef-themes . "1.0.0")
    :type '(repeat (list symbol (choice symbol string)))
    :link '(info-link "(ef-themes) Palette overrides"))

  (ef-themes-theme ef-catppuccin-macchiato
                   ef-catppuccin-macchiato-palette
                   ef-catppuccin-macchiato-palette-overrides)

  (provide-theme 'ef-catppuccin-macchiato))

;;; ef-catppuccin-macchiato-theme.el ends here
