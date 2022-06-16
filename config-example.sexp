;; -*- mode: lisp -*-

(font "xft:LucidaTypewriter:size=8")
(type rofi) ; emulates dmenu on rofi
(position bottom)
(browser-command "chromium")
(browser-incognito-flag "--incognito")

(favorites
 ((netflix       "https://netflix.com")
  (protonmail    "https://mail.protonmail.com/login")
  (github        "https://github.com")
  (gitlab        "https://gitlab.com")
  (linkedin      "https://linkedin.com")
  (hackernews    "https://news.ycombinator.com")
  (slashdot      "https://slashdot.org")
  (lobsters      "https://lobste.rs")
  (instagram     "https://instagram.com")
  (spotify       "https://open.spotify.com")
  (feedly        "https://feedly.com")
  (whatsapp      "https://web.whatsapp.com")
  (mstdn-sdf     "https://mastodon.sdf.org")
  (amazon        "https://amazon.com.br")
  (discord       "https://discordapp.com/activity")
  (steam         "https://store.steampowered.com")
  (gitlab-cl     "https://gitlab.common-lisp.net")
  (keybase       "https://keybase.io")
  (bitbucket     "https://bitbucket.com")))

(search-engines
 ((youtube    ("https://www.youtube.com/results?search_query="))
  (invidio.us ("https://invidio.us/search?q="))
  (rutracker  ("https://rutracker.org/forum/search_cse.php?q="))
  (torrentz2  ("https://torrentz2.eu/search?f="))
  (piratebay  ("https://thepiratebay.bid/s/?q="
               "&category=0&page=0&orderby=99"))
  (amazon     ("https://www.amazon.com.br/s/ref=nb_sb_noss?__mk_pt_BR=ÅMÅŽÕÑ&url=search-alias%3Daps&keywords="))
  (ml         ("https://lista.mercadolivre.com.br/"))
  (wiki-en    ("https://en.wikipedia.org/wiki/Special:Search?search="
               "&go=Go&searchToken=1myp3m6eooqqr34dcuh5h9hh1"))
  (wiki-pt    ("https://pt.wikipedia.org/w/index.php?search="
               "&title=Especial%3APesquisar&go=Ir"))))

;; Common program launchers
(commands
 (mpd    (launch "xterm -e ncmpcpp"))
 (lisp   (launch "urxvt -e ros run"))
 (scheme (launch "urxvt -e scheme"))
 (apl    (launch "urxvt"
                 "-fn xft:APL385 Unicode-10"
                 "-e apl"))
 (dyalog (launch "urxvt"
                 "-fn xft:APL385 Unicode-10"
                 "-e dyalog")))
;; Sub-menus
(submenus
 (games (launch
         (prompt-select
          "Game?"
          '((steam      "/usr/bin/steam")
            (doom       "/usr/bin/gzdoom")
            (brutaldoom "/usr/local/bin/brutaldoom")))))
 ;; Argh, would be better to just do this directly
 ;; in CL, but let's keep going and see where this
 ;; leads us to.
 (translate (use languages
                 ((english    "en")
                  (portuguese "pt")
                  (spanish    "es")))
            ;; (use orig
            ;;      (select-from languages
            ;;                   (prompt "Original?")))
            (use orig
                 (prompt-select "Original?" languages))
            ;; (use to
            ;;      (select-from languages
            ;;                   (prompt "To?")))
            (use to
                 (prompt-select "To?" languages))
            (use query
                 (prompt "Translate ~a->~a:" orig to))
            (launch-browser
             "https://translate.google.com/#"
             orig "/" to "/" query))
 ;; Query subdomains on websites
 (forums (query-subdomain
          ((reddit "https://reddit.com"
                   ((index)
                    (emacs "r/emacs")
                    (lisp  "r/lisp")
                    (linux "r/linux")
                    (plan9 "r/plan9")
                    (stoic "r/stoicism")
                    (math  "r/learnmath")))
           (lain "https://lainchan.org"
                 ((index)
                  (prog  "lambda")
                  (diy   "delta")
                  (sec   "sec")
                  (tech  "omega")
                  (music "music")))))))
